open Ast 
open Ast_print
open Format
open Range
open Util
open Dswp_task

let generated_init_task = ref None
let generated_tasks = ref []
let generated_decl_vars = ref []
let codegen = ref false
let commutativity_spec_exist = ref false

type dependency =
| ControlDep
| DataDep of (ty * id) list
| Commute of (exp node) * (string list) * (string list)
| Disjoint

type enode_ast_elt = 
| Entry 
| EWhile of exp node
| EIf of exp node
| EIfElse of exp node
| EFor of vdecl list * exp node option * stmt node option
| EStmt of stmt node

let transform_stmt (s: stmt node) : enode_ast_elt = 
  match s.elt with 
  | If (e, b1, b2) ->
    begin match b2.elt with
    | [] -> EIf e
    | _ -> EIfElse e
    end
  | While (e,_) -> EWhile e 
  | For (v,e,s,_) -> EFor (v,e,s)
  | _ -> EStmt s

type pdg_node = {
  l: Range.t; 
  n: enode_ast_elt;
  src: stmt node option 
}

type pdg_edge = {
  src : pdg_node;
  dst : pdg_node;
  dep : dependency;
  loop_carried : bool
}

type exe_pdg = {
  entry_node: pdg_node option;
  nodes : pdg_node list;
  edges : pdg_edge list;
}

let empty_exe_pdg () : exe_pdg =
  { entry_node = None; nodes = []; edges = [] }

let add_node (pdg : exe_pdg) (s : stmt node) : pdg_node * exe_pdg =
  let n = {l = s.loc; n = transform_stmt s; src = Some s} in 
  n, { pdg with nodes = pdg.nodes @ [n] }

let add_edge (pdg : exe_pdg) (src : pdg_node) (dst : pdg_node) dep : exe_pdg = 
  { pdg with edges = pdg.edges @ [{ src; dst; dep; loop_carried = false }] }


let string_of_dep = function
  | ControlDep -> "ControlDep"
  | DataDep vars -> sp "DataDep (%s)" (AstPP.string_of_args vars) 
  | Commute (b,args1,args2) -> sp "[%s]; [%s]; Commute (%s)" (AstML.string_of_list Fun.id args1) (AstML.string_of_list Fun.id args2) (AstPP.string_of_exp b)
  | Disjoint -> "Disjoint"

(*
let c_of_stmt = function
  | Entry -> "Entry"
  | EWhile e -> sp "while(%s)" (Ast_to_c.c_of_expnode e)
  | EIf e -> sp "if(%s)" (Ast_to_c.c_of_expnode e) 
  | EIfElse e -> sp "if(%s)" (Ast_to_c.c_of_expnode e) 
  | EFor(inits, e, update) -> sp "for(%s; %s; %s)" (String.concat ", " @@ List.map (fun (id, (ty, rhs)) -> sp "%s %s = %s" (Ast_to_c.c_of_ty ty) (!Ast_to_c.mangle id) (Ast_to_c.c_of_expnode rhs)) inits) (e |> Option.map Ast_to_c.c_of_expnode |> Option.value ~default:"") (update |> Option.map Ast_to_c.c_of_stmtnode |> Option.value ~default:"")
  | EStmt s -> Ast_to_c.c_of_stmt s.elt
*)
let pp_stmt = function
  | Entry -> "Entry"
  | EWhile e -> sp "while(%s)" (Ast_print.AstPP.string_of_exp e)
  | EIf e -> sp "if(%s)" (Ast_print.AstPP.string_of_exp e) 
  | EIfElse e -> sp "if(%s)" (Ast_print.AstPP.string_of_exp e) 
  | EFor(inits, e, update) -> sp "for(%s; %s; %s)" (String.concat ", " @@ List.map (fun (id, (ty, rhs)) -> sp "%s %s = %s" (Ast_print.AstPP.string_of_ty ty) id (Ast_print.AstPP.string_of_exp rhs)) inits) (e |> Option.map Ast_print.AstPP.string_of_exp |> Option.value ~default:"") (update |> Option.map Ast_print.AstPP.string_of_stmt |> Option.value ~default:"")
  | EStmt s -> Ast_print.AstPP.string_of_stmt (no_loc s.elt)

let string_of_pdg_node_stmt s =
  (* let big_string = Ast_to_c.c_of_stmt s in  *)
  (* if String.length big_string > 20 then String.sub big_string 0 19 else big_string *)
  (* c_of_stmt s *)
  pp_stmt s


let penwidth_of_pdgedge p = 
  if p.loop_carried then "4.0" else "1.0"

let print_pdg pdg fn : unit = 
  let oc = open_out fn in
  output_string oc (String.concat "\n" [
    "digraph G {\n";
    (* Styles *)
    "  graph [rankdir=\"TB\", fontname=\"Arial\", fontsize=24, label=\"Program Dependency Graph (PDG): red=control, green=data\", labelloc=t, labeljust=l]";
    "  node [shape=box, style=\"rounded,filled\", fontname=\"Courier\", margin=0.05]";
    "  edge [arrowhead=vee, arrowsize=1, fontname=\"Courier\"]";
    (* Nodes *)
    (* let s = "\" [label=\""^(match pdg.entry_node with | Some en -> string_of_pdg_node_stmt en.n)^"\"];\n" in *)
    List.fold_left (fun acc node -> acc ^ "\"" ^ (Range.string_of_range_nofn node.l)
    ^ "\" [label=\""^(Util.dot_escape (string_of_pdg_node_stmt node.n))^"\"];\n") "" pdg.nodes;
    (* edges *)
    List.fold_left (fun acc e -> 
      let pw = penwidth_of_pdgedge e in
      acc ^ (match e.dep with
       | DataDep vars ->
           let vars = AstPP.string_of_args vars in
          "\"" ^ (Range.string_of_range_nofn e.src.l) ^ "\" -> \"" 
                ^ (Range.string_of_range_nofn e.dst.l) ^ "\" "
                ^ "[style=solid, color=green, label=\""^(dot_escape vars)^"\", penwidth=\""^pw^"\"];\n" 
       | Commute (exp, args1, args2) ->
          let cond = AstPP.string_of_exp exp in
          "\"" ^ (Range.string_of_range_nofn e.src.l) ^ "\" -> \"" 
                ^ (Range.string_of_range_nofn e.dst.l) ^ "\" "
                ^ "[style=dotted, color=red, label=\""^(dot_escape cond)^"\", penwidth=\""^pw^"\"];\n"
       | Disjoint 
       | ControlDep ->
          "\"" ^ (Range.string_of_range_nofn e.src.l) ^ "\" -> \"" 
               ^ (Range.string_of_range_nofn e.dst.l) ^ "\" "
               ^ "[style=dashed, color=maroon, penwidth=\""^(dot_escape pw)^"\"];\n" (*label=\""^(string_of_dep e.dep)^"\"];\n"*)
    )) "" pdg.edges;
    "}\n";
  ]);
  debug_print (lazy ("pdg written to " ^ fn));
  close_out oc


let print_pdg_debug pdg =
  if !Util.debug then begin
    begin match pdg.entry_node with | Some en -> Printf.printf "entry node: %s\n" (Range.string_of_range en.l) | _ -> () end;
    List.iteri (fun i s -> Printf.printf "node %d: %s\n" i (Range.string_of_range s.l)) pdg.nodes;
    List.iteri (fun i e -> Printf.printf "pdg_edge %d (%s) - %b: %s - %s\n" i (string_of_dep e.dep) e.loop_carried (Range.string_of_range_nofn e.src.l) (Range.string_of_range_nofn e.dst.l)) pdg.edges
  end

let find_node (s: stmt node) pdg : pdg_node =
    let sl = s.loc in 
    List.find (
        fun {l=loc;_} -> String.equal (Range.string_of_range loc) (Range.string_of_range sl) 
    ) pdg.nodes

let compare_nodes n1 n2 = 
  String.equal (Range.string_of_range n1.l) (Range.string_of_range n2.l)


let rvalue = 1
let lvalue = 0
let decl_vars = ref []
let m_vars = ref []

let set_vars_side (vars : (ty * string) list) side : ((ty * string) * int) list = 
  List.map (fun v -> (v, side)) vars

let find_global_by_name_opt name = List.find_opt (function (Gvdecl d) -> String.equal d.elt.name name | _ -> false) !decl_vars

let rec find_block_vars block : ((ty * string) * int) list = 
  match block with 
  | [] -> []
  | stmt::tl -> (find_stmt_vars (EStmt stmt)) @ (find_block_vars tl)

and find_stmt_vars (stmt: enode_ast_elt) : ((ty * string) * int) list = 
  match stmt with
  | EWhile e | EIf e | EIfElse e  -> set_vars_side (find_exp_vars e) rvalue
  | EFor (vdecls, eoption, soption) -> 
    List.concat_map (fun v -> 
      let id, (ty, e) = v in 
      ((ty , id), lvalue) :: (set_vars_side (find_exp_vars e) rvalue)
    ) vdecls 
    @
    begin match eoption with 
    | Some e -> set_vars_side (find_exp_vars e) rvalue
    | None -> []
    end 
    @
    begin match soption with 
    | Some s -> find_stmt_vars (EStmt s)
    | None -> []
    end 
  | EStmt s ->
    begin match s.elt with 
    | Assn (e1,e2) -> (set_vars_side (find_exp_vars e1) lvalue) @ (set_vars_side (find_exp_vars e2) rvalue)
    | Decl vdecl ->
      let id, (ty, e) = vdecl in 
      let decl = Gvdecl (no_loc { name = id; ty = ty; init = e }) in 
      if not (List.mem decl !decl_vars) then 
        decl_vars := decl :: !decl_vars;
      ((ty , id), lvalue) :: (set_vars_side (find_exp_vars e) rvalue)
    | Ret (Some e) -> set_vars_side (find_exp_vars e) rvalue
    | SBlock (Some (_, None),body) | SBlock (None, body) -> find_block_vars body.elt
    | SBlock (Some (_, Some bl),body) -> (List.concat_map (fun e -> (set_vars_side (find_exp_vars e) rvalue)) bl) @ find_block_vars body.elt 
    | While (e, body) -> (set_vars_side (find_exp_vars e) rvalue) @ find_block_vars body.elt
    | For (vdecls,eoption,soption,body) -> 
      List.concat_map (fun v -> 
      let id, (ty, e) = v in 
      ((ty , id), lvalue) :: (set_vars_side (find_exp_vars e) rvalue)
      ) vdecls 
      @
      begin match eoption with 
      | Some e -> set_vars_side (find_exp_vars e) rvalue
      | None -> []
      end 
      @
      begin match soption with 
      | Some s -> find_stmt_vars (EStmt s)
      | None -> []
      end 
      @ find_block_vars body.elt
    | If (e,b1,b2) -> (set_vars_side (find_exp_vars e) rvalue) @ (find_block_vars b1.elt) @ (find_block_vars b2.elt)
    | Assert e | Assume e | Require e | Raise e -> set_vars_side (find_exp_vars e) rvalue
    | SCall (_, el) | SCallRaw (_, el) -> (set_vars_side (List.concat_map find_exp_vars el) rvalue)
    | _ -> []
    end 
  | Entry -> []

and find_exp_vars exp : (ty * string) list =
  match exp.elt with 
  | CStr s | Id s -> 
  begin match find_global_by_name_opt s with 
  | None -> begin match List.find_opt (fun (ty, id) -> String.equal id s) !m_vars with | None -> [] | Some l -> [l] end
  | Some (Gvdecl v) -> [(v.elt.ty, s)]
  | _ -> failwith "undefined variable"
  end
  | CArr (_, expl) -> List.concat_map find_exp_vars expl
  | NewArr (_, e) | Uop (_, e) -> find_exp_vars e
  | Index (e1, e2) | Bop (_, e1, e2) -> (find_exp_vars e1) @ (find_exp_vars e2) 
  | CallRaw (_, expl) -> List.concat_map find_exp_vars expl
  | Call (m, expl) -> List.concat_map find_exp_vars expl (* TODO: check *)
  | Ternary (e1, e2, e3) -> (find_exp_vars e1) @ (find_exp_vars e2) @ (find_exp_vars e3)
  | _ -> []
  (* 
  | CStruct of id * exp node bindlist
  | Proj of exp node * id *)

let src_to_dst = 1
let dst_to_src = 0

let has_data_dep src dst : bool * (ty * string) list * (ty * string) list =
  let list1 = find_stmt_vars src.n in 
  let list2 = find_stmt_vars dst.n in 
  let rec values_for_key_pair key lst =
  match lst with
  | [] -> []
  | (k, v) :: tl ->
      if k = key then 
        v :: values_for_key_pair key tl
      else 
        values_for_key_pair key tl
  in

  let dep = ref false in
  let std_vars = ref [] in 
  let dts_vars = ref [] in

  (* Function to check for common elements with specific value patterns *)
  let rec has_common_element list1 list2 : bool = 
    match list1 with
    | [] -> !dep
    | (head, val1) :: tail ->
      if List.mem_assoc head list2 then begin
        let val2 = values_for_key_pair head list2 in 
        let rec check_val2 val2_list =
          match val2_list with 
          | [] -> !dep
          | val2 :: rest -> 
            begin match val1, val2 with 
            | 0, 1 -> 
              if not (List.mem head !std_vars) then begin
                std_vars := head :: !std_vars;
                dep := true
              end;
              check_val2 rest
            | 1, 0 -> 
              if not (List.mem head !dts_vars) then begin
                dts_vars := head :: !dts_vars;
                dep := true
              end;
              check_val2 rest
            | _, _ -> check_val2 rest
            end
        in
        let res1 = check_val2 val2 in
        let res2 = has_common_element tail list2 in 
        res1 || res2
      end else
        has_common_element tail list2
  in let flag = has_common_element list1 list2 in 
  flag,
  List.map (
    fun (t, id) -> 
    match find_global_by_name_opt id with 
    | Some (Gvdecl d) -> (d.elt.ty, id)
    | _ -> (t,id)
  ) !std_vars,
  List.map (
    fun (t, id) -> 
    match find_global_by_name_opt id with 
    | Some (Gvdecl d) -> (d.elt.ty, id)
    | _ -> (t,id)
  ) !dts_vars

let add_dataDep_edges pdg = 
  let p = ref pdg in 

  let deps_equal deps1 deps2 =
    List.length deps1 = List.length deps2 && 
    List.for_all (fun d -> List.mem d deps2) deps1
  in 
  apply_pairs (fun x y -> 
    let dep, std_deps, dts_deps = has_data_dep x y in 
    if dep then begin
      if (not (List.is_empty std_deps)) && (not (compare_nodes x y) || not (deps_equal std_deps dts_deps)) then
        p := add_edge !p x y (DataDep std_deps);
      if (not (List.is_empty dts_deps)) && (not (compare_nodes x y) || not (deps_equal std_deps dts_deps)) then
        p := add_edge !p y x (DataDep dts_deps);
    end
  ) pdg.nodes;
  !p


let add_commuteDep_edges pdg (gc: group_commute node list) : exe_pdg =
  let find_commute_condition l1 l2 =
    let res = ref (None,[],[]) in 
    List.iter (
      fun {elt=(bl, cond); _} ->
        let check_label label lb_list = 
          List.exists (fun (l,_) -> String.equal (fst label) l) lb_list
        in 
        let find_label_args label lb_list = 
          let args = snd (List.find (fun (l,_) -> String.equal (fst label) l) lb_list)
          in match args with
          | None -> [] 
          | Some a -> List.map (function | {elt=Id s; _} -> s | _ -> failwith "Non-ids used for formal parameters in gcommute condition.") a
        in
        apply_distinct_pairs (
          fun x y -> 
            if (check_label l1 x && check_label l2 y) then 
              res := ((Some cond), find_label_args l1 x, find_label_args l2 y)
            else if (check_label l1 y && check_label l2 x) then 
              res := ((Some cond), find_label_args l1 y, find_label_args l2 x)
          ) bl
      ) gc;
    res
  in
  let p = ref pdg in 
  apply_pairs (fun (x : pdg_node) (y : pdg_node) -> 
    begin match x.src, y.src with 
    | Some {elt=(SBlock (Some l1, _)); loc=_}, Some {elt=(SBlock (Some l2, _)); loc=_} -> 
      let cond, vars1, vars2 = !(find_commute_condition l1 l2) in
      begin match cond with 
      | None -> ()
      | Some (PhiExp cond) -> p := add_edge !p x y (Commute (cond, vars1, vars2))
      | _ -> failwith "undefined commute condition"
      end
    | _, _ -> ()
    end
  ) pdg.nodes;
  !p


type visited = (pdg_node * bool) list

let rec mark_visited n visited =
  match visited with
  | [] -> visited
  | (node, _) :: rest ->
    if node = n then
      (node, true) :: rest
    else
      (node, false) :: mark_visited n rest

(* Function to check if a dependence arc is loop-carried *)
let is_loop_carried_dependence (pdg: exe_pdg) (edge: pdg_edge) =
  let n1 = edge.src in
  let n2 = edge.dst in
let find_outermost_loop_header pdg mem_node : pdg_node option =
  let is_loop_header (node: pdg_node) =
    match node.n with
    | EFor _ | EWhile _ -> true
    | _ -> false
  in
  let header = ref None in 
  let rec traverse_backwards (node: pdg_node) visited =
    if List.mem node visited then None
    else begin
      if (is_loop_header node) then 
        header := Some node;
      let predecessors = List.filter_map (fun e ->
        if compare_nodes e.dst node && e.dep == ControlDep then Some e.src else None) pdg.edges in
      begin match 
      List.fold_left (fun acc pred ->
        match acc with
        | Some _ -> acc
        | None -> traverse_backwards pred (node :: visited)) None predecessors
      with 
      | None -> !header 
      | h -> h
      end
    end
  in
  traverse_backwards mem_node []

  in
  match edge.dep with
  | DataDep register ->
    (* Check if the definition of the register reaches the outermost loop header *)
    let rec definition_reaches_outer_loop_header (node: pdg_node) (visited: visited ref) =
      if List.assoc node !visited then false
      else begin
        visited := mark_visited node !visited;
        let outer_loop_header = find_outermost_loop_header pdg node in
        match outer_loop_header with
        | None -> false
        | Some outer_loop_header ->
          if compare_nodes outer_loop_header node then true 
          else match List.find_opt (fun e -> compare_nodes e.dst node) pdg.edges with
               | None -> false
               | Some e -> definition_reaches_outer_loop_header e.src visited
      end
    in
    let visited = ref @@ List.map (fun v -> (v, false)) pdg.nodes in
    let definition_reaches_outer_loop_header = definition_reaches_outer_loop_header n1 visited in
    (* Check if there is an upwards-exposed use of the register in n2 at the outermost loop header *)
    let upwards_exposed_use_in_outer_loop_header =
      let outer_loop_header = find_outermost_loop_header pdg n1 in
      match outer_loop_header with
      | None -> false
      | Some outer_loop_header ->
        let rec has_upwards_exposed_use node =
          let uses, _ = List.split (List.filter (fun (v, side) -> side == rvalue) (find_stmt_vars node.n)) in 
          if List.exists (fun (_, use) -> List.exists (fun (_, r) -> String.equal use r) register) uses then true
          else match List.find_opt (fun e -> compare_nodes e.src node) pdg.edges with
            | None -> false
            | Some e ->
              if compare_nodes outer_loop_header e.src then false (* Reached outermost loop header *)
              else has_upwards_exposed_use e.src
        in
        has_upwards_exposed_use n2
    in
    definition_reaches_outer_loop_header && upwards_exposed_use_in_outer_loop_header
  (* | ControlDep ->
    let rec is_loop_carried_control_dependence n1 n2 pdg visited =
      if compare_nodes n1 n2 then
        true 
      else if List.mem n1 !visited then
        false
      else begin
        visited := n1 :: !visited;
        List.fold_left (fun acc node ->
          if is_loop_carried_control_dependence node n2 pdg visited then
            acc && true 
          else
            false
        ) true (List.fold_left (fun acc e -> if compare_nodes e.src n1 then acc @ [e.dst] else acc ) [] pdg.edges)
      end
    in
    is_loop_carried_control_dependence n1 n2 pdg (ref []) *)
  | _ -> false 

(* Function to find loop-carried dependencies in the exe_pdg graph *)
let mark_loop_carried_dependencies pdg : exe_pdg =
  let nodes = match pdg.entry_node with | Some e -> e :: pdg.nodes | None -> pdg.nodes in
  let pdg_tmp = {pdg with nodes= nodes} in
  let e = List.map (fun edge -> if is_loop_carried_dependence pdg_tmp edge then {edge with loop_carried= true} else edge) pdg_tmp.edges
  in 
  {pdg with edges = e}

let local_decl = ref []

let build_pdg (block: block) entry_loc (gc: group_commute node list) : exe_pdg = 
  let pdg = empty_exe_pdg() in 
  let pdg = { pdg with entry_node = Some {l= entry_loc; n= Entry; src= None} } in
  let rec traverse_ast block pdg : exe_pdg =
    match block with
    | [] -> pdg
    | stmt::tl ->
      let updated_pdg = begin match stmt.elt with 
      | Decl _ ->
        local_decl := !local_decl @ [Some stmt];
        snd (add_node pdg stmt)
      | If (e, blk1, blk2) ->
        let src, pdg = add_node pdg stmt in
        let pdg = traverse_ast blk2.elt (traverse_ast blk1.elt pdg) in 

        List.fold_left (fun pdg s -> add_edge pdg src (find_node s pdg) ControlDep) pdg blk1.elt
          
      | While (_, bl) | For (_, _, _, bl) ->
        let src, pdg = add_node pdg stmt in
        let pdg = traverse_ast bl.elt pdg in 

        let pdg = add_edge pdg src src ControlDep in
        List.fold_left (fun pdg s -> add_edge pdg src (find_node s pdg) ControlDep) pdg bl.elt

      (* | SBlock (None, bl) ->
        traverse_ast bl.elt pdg *)
      (* | SBlock (blocklabel, bl) -> 
        let n = stmt in 
        snd (add_node pdg n) *)

      | _ ->  
        snd (add_node pdg stmt)
      end 
      in 
      traverse_ast tl updated_pdg
  in 
  let pdg = (traverse_ast block pdg) in
  (* add data dependency edges for each pairs of nodes *)
  let pdg = add_dataDep_edges pdg in 
  (* add commute dependency edges *)
  let pdg = add_commuteDep_edges pdg gc in
  (* connect the entry node to the header nodes *)
  let pdg = begin match pdg.entry_node with 
  | Some en -> List.fold_left (fun pdg s -> let n = find_node s pdg in add_edge pdg en n ControlDep) pdg block
  | None -> pdg
  end
  in
  mark_loop_carried_dependencies pdg

let find_neighbors pdg node : pdg_node list = 
  List.fold_left (fun neighbors e -> if compare_nodes e.src node then neighbors @ [e.dst] else neighbors) [] pdg.edges

let is_separate_node (node: pdg_node) : bool =
  match node.src with
  | Some{elt=(SBlock ((Some _), _))} -> true
  | Some{elt=(SBlock (None, _))} -> if !commutativity_spec_exist then true else false
  | _ -> false

let rec dfs_util pdg (curr: pdg_node) (visited: visited ref) : pdg_node list =
  visited := List.remove_assoc curr !visited @ [(curr, true)];
  let neighbors = find_neighbors pdg curr in
  curr :: List.flatten (List.map (fun n -> if not (List.assoc n !visited) && not (is_separate_node n) then dfs_util pdg n visited else []) neighbors)

let transpose pdg : exe_pdg =
  {pdg with edges = List.map (fun {src=s; dst=d; dep=dp; loop_carried=l} -> {src=d; dst=s; dep=dp; loop_carried=l}) pdg.edges}

let rec fill_order pdg (curr: pdg_node) (visited: visited ref) stack =
  if not (List.assoc curr !visited) then begin
    visited := List.remove_assoc curr !visited @ [(curr, true)];
    if not (is_separate_node curr) then begin
      let neighbors = find_neighbors pdg curr in
      List.iter (fun n -> fill_order pdg n visited stack) neighbors;
    end;
    Stack.push curr stack
  end

let find_sccs pdg : pdg_node list list =
  let stack = Stack.create () in
  let nodes = match pdg.entry_node with | Some e -> e :: pdg.nodes | None -> pdg.nodes in
  let pdg = {pdg with nodes= nodes} in
  let visited = ref @@ List.map (fun v -> (v, false)) pdg.nodes in 
  List.iter (fun n -> if not (List.assoc n !visited) then fill_order pdg n visited stack) pdg.nodes;
  let reversed_pdg = transpose pdg in
  let visited = ref @@ List.map (fun v -> (v, false)) pdg.nodes in 
  let sccs = ref [] in 
  while not (Stack.is_empty stack) do
    let s = Stack.pop stack in
    if not (List.assoc s !visited) then
      if is_separate_node s then
        sccs := !sccs @ [[s]]  (* Place commute fragment nodes in their own SCC *)
      else
        sccs := !sccs @ [dfs_util reversed_pdg s visited]
  done;

  let find_min li = List.fold_left (fun acc {l=(_,p,_)} -> Int.min acc (Range.line_of_pos p)) Int.max_int li in
  sccs := List.sort (fun n1 n2 -> Int.compare (find_min n1) (find_min n2)) !sccs;

  !sccs

let print_sccs (sccs: pdg_node list list) =
  List.iter (fun s -> List.iter (fun c -> Printf.printf "%s " (Range.string_of_range_nofn c.l)) s; print_newline ()) sccs

type dag_node_label = Doall | Sequential

type dag_node = {
  n : pdg_node list;
  label: dag_node_label
}

type dag_edge = {
  dag_src : dag_node;
  dag_dst : dag_node;
  dep : dependency;
  loop_carried : bool
}

type dag_scc = {
  entry_node: dag_node option;
  nodes : dag_node list;
  edges : dag_edge list;
}

let id_of_dag_node (dn:dag_node) : string = 
  List.fold_left (fun acc pdgnode -> acc ^ "_" ^ (Range.string_of_range_nofn pdgnode.l)) "" dn.n

let dag_pdgnode_to_string (pdgnodes:pdg_node list) : string = 
  List.fold_left (fun acc pnode -> acc ^ (Range.string_of_range_nofn pnode.l)
  ^ ":"^(string_of_pdg_node_stmt pnode.n) ^ ",") "" pdgnodes

let color_of_dagnode = function
  | Doall -> "white"
  | Sequential -> "gray"
let penwidth_of_dagedge de =
  if de.loop_carried then "4.0" else "1.0"

let print_dag (d:dag_scc) fn node_to_string_fn : unit = 
  let oc = open_out fn in
  output_string oc (String.concat "\n" [
    "digraph G {";
    (* Styles *)
    "  graph [rankdir=\"TB\", fontname=\"Arial\", fontsize=24, label=\"DAG\", labelloc=t, labeljust=l]";
    "  node [shape=box, style=\"rounded,filled\", fontname=\"Courier\", margin=0.05]";
    "  edge [arrowhead=vee, arrowsize=1, fontname=\"Courier\", penwidth=4.0]";
    (* Nodes *)
    List.fold_left (fun acc node -> acc ^ "\"" ^ (id_of_dag_node node)
    ^ "\" [color=\""^(color_of_dagnode node.label)^"\" label=\""^(node_to_string_fn node.n)^"\"];\n") "" d.nodes;
    (* edges *)
    List.fold_left (fun acc e -> 
      let pw = penwidth_of_dagedge e in
      acc ^ (match e.dep with
       | DataDep vars ->
           let vars = AstPP.string_of_args vars in
          "\"" ^ (id_of_dag_node e.dag_src) ^ "\" -> \"" 
                ^ (id_of_dag_node e.dag_dst) ^ "\" "
                ^ "[style=solid, color=green, label=\""^(dot_escape vars)^"\", penwidth=\""^pw^"\"];\n" 
       | Commute (exp, args1, args2) ->
          let cond = AstPP.string_of_exp exp in
          "\"" ^ (id_of_dag_node e.dag_src) ^ "\" -> \"" 
                ^ (id_of_dag_node e.dag_dst) ^ "\" "
                ^ "[style=dotted, color=red, label=\""^(dot_escape cond)^"\", penwidth=\""^pw^"\"];\n"  
       | Disjoint 
       | ControlDep ->
          "\"" ^ (id_of_dag_node e.dag_src) ^ "\" -> \"" 
               ^ (id_of_dag_node e.dag_dst) ^ "\" "
               ^ "[style=dashed, color=maroon, penwidth=\""^(dot_escape pw)^"\"];\n" (*label=\""^(string_of_dep e.dep)^"\"];\n"*)
    )) "" d.edges;
    "}\n";
  ]);
  debug_print (lazy ("dag written to " ^ fn));
  close_out oc

let has_loop_carried (scc: pdg_node list) (pdg: exe_pdg) : bool =
    let find_edge n1 n2 =
    List.find_opt (fun e -> e.src == n1 && e.dst == n2) pdg.edges
    in
    let res = ref false in 
    apply_pairs (
      fun s1 s2 -> 
      let e1 = find_edge s1 s2 in 
      let e2 = find_edge s2 s1 in 
      res := !res || (match e1 with | None -> false | Some e -> e.loop_carried) || (match e2 with | None -> false | Some e -> e.loop_carried) 
    ) scc;
    !res

let compare_dag_nodes n1 n2 =
    List.length n1.n = List.length n2.n &&
    List.for_all2 compare_nodes n1.n n2.n

let remove_duplicate_edge (edges: dag_edge list) =
  let rec is_member (n: dag_edge) (medges: dag_edge list) =
    match medges with
    | [] -> false
    | h::tl ->
        begin
          if compare_dag_nodes h.dag_src n.dag_src && compare_dag_nodes h.dag_dst n.dag_dst && String.equal (string_of_dep h.dep) (string_of_dep n.dep) then true
          else is_member n tl
        end
  in
  let rec loop (lbuf: dag_edge list) =
    match lbuf with
    | [] -> []
    | h::tl ->
        begin
        let rbuf = loop tl
        in
          if is_member h rbuf then rbuf
          else h::rbuf
        end
  in
  loop edges

let coalesce_sccs (pdg: exe_pdg) (sccs: pdg_node list list) : dag_scc =  
  let update_edges = ref pdg.edges in 
  let update_nodes = ref pdg.nodes in 
  let sccs = ref sccs in
  let entry = 
    match pdg.entry_node with 
    | None -> None 
    | Some e -> 
      let remove_node nodes n = 
        let rec remove_from_list x lst = match lst with
        | [] -> []
        | hd :: tl -> if (compare_nodes hd x) then remove_from_list x tl
                      else hd :: remove_from_list x tl
        in
        let rec remove_from_2d_list x lst2d = match lst2d with
            | [] -> []
            | hd :: tl -> (remove_from_list x hd) :: (remove_from_2d_list x tl)
        in
        sccs := List.filter (fun s -> not (List.is_empty s)) (remove_from_2d_list n !sccs);
        List.filter (fun node -> not (compare_nodes n node)) nodes
      in
      let remove_edge edges e = 
        List.filter (fun edge -> not ((compare_nodes e.src edge.src) && (compare_nodes e.dst edge.dst))) edges 
      in
      let new_entry = List.fold_left (fun acc edge -> if compare_nodes edge.src e && List.mem edge.dst.src !local_decl then begin update_nodes := remove_node !update_nodes edge.dst; update_edges := remove_edge !update_edges edge; acc @ [edge.dst] end else acc) [] pdg.edges in
      sccs := List.map (fun s -> if List.mem e s then [e] @ new_entry else s) !sccs;
      Some {n = [e] @ new_entry; label = Sequential} 
  in 
  let sccs = !sccs in
  let pdg = {pdg with nodes = !update_nodes; edges = !update_edges} in
  let nodes = List.map (fun scc -> if has_loop_carried scc pdg then {n= scc; label= Sequential} else {n= scc; label= Doall}) sccs in
  let find_node_scc n scc =
    List.mem n scc
  in
  let find_dag_node (sub_node : pdg_node) =
    List.find (fun node -> List.exists (fun e -> compare_nodes e sub_node) node.n) nodes
  in
  let is_scc (n1: pdg_node) (n2: pdg_node) : bool =
    List.exists (fun scc -> find_node_scc n1 scc && find_node_scc n2 scc) sccs 
  in
  let filtered_edges = List.filter (fun {src= s; dst= d; _} -> not (is_scc s d) || (compare_nodes s d)) pdg.edges in 
  let edges = List.map (
    fun {src= s; dst= d; dep=dp; loop_carried =l} -> 
    {dag_src= find_dag_node s; dag_dst= find_dag_node d; dep=dp; loop_carried=l}
  ) filtered_edges in 
  let edges = remove_duplicate_edge edges in
  let nodes = List.filter (fun n -> match entry with | None -> true | Some node -> not (compare_dag_nodes n node)) nodes in
  {entry_node = entry; nodes; edges}


let string_of_dag_label = function
  | Doall -> "doall"
  | Sequential -> "sequential"


let print_dag_debug dag_scc =
  if !Util.debug then begin
    let string_of_node n = List.fold_left (fun acc s -> acc ^ (Range.string_of_range_nofn s.l) ^ " ") "" n in 
    begin match dag_scc.entry_node with | Some en -> Printf.printf "entry node: %s\n" (string_of_node en.n) | _ -> () end;
    List.iteri (fun i sl -> Printf.printf "node %d (%s): %s" i (string_of_dag_label sl.label) (string_of_node sl.n); print_newline()) dag_scc.nodes;
    List.iteri (fun i e -> Printf.printf "dag_edge %d (%s) - %b: %s - %s\n" i (string_of_dep e.dep) e.loop_carried (string_of_node e.dag_src.n) (string_of_node e.dag_dst.n)) dag_scc.edges end

let rec all_in_list_a_in_b list_a list_b =
  match list_a with
  | [] -> true
  | hd :: tl ->
    if List.mem hd list_b then
      all_in_list_a_in_b tl list_b
    else
      false

let is_return_node (node : dag_node) =
  List.exists (fun {l=loc;n=_;src = Some s} -> match s.elt with | Ret _ -> true | _ -> false) node.n

let rec find_ancestors ancestors visited edges src_node =
  List.fold_left (fun acc edge ->
    if edge.dag_dst = src_node && not (List.mem edge.dag_src acc) && edge.dep == ControlDep then
      if not (List.mem edge.dag_src visited) then
        find_ancestors (edge.dag_src :: acc) (edge.dag_src :: visited) edges edge.dag_src
      else
        acc
    else
      acc
  ) ancestors edges

(* Function to merge doall blocks greedily *)
let merge_doall_blocks dag_scc (pdg: exe_pdg) =
  let find_reachable_blocks block dag_scc visited =
    let reachable_blocks = ref [] in
    let rec dfs node =
      if not (List.mem node !visited) then begin
        visited := node :: !visited;
        List.iter (fun e ->
          if e.dag_src == node then dfs e.dag_dst) dag_scc.edges;
        if node != block && node.label = Doall then
          reachable_blocks := node :: !reachable_blocks
      end
    in
    dfs block;
    !reachable_blocks
  in
  let can_merge_blocks block1 block2 dag_scc =
    let c = List.exists (
      fun e -> 
      (all_in_list_a_in_b e.dag_src.n block1.n 
      && all_in_list_a_in_b e.dag_dst.n block2.n
      ||
      all_in_list_a_in_b e.dag_src.n block2.n 
      && all_in_list_a_in_b e.dag_dst.n block1.n)
      &&(
        e.dep == ControlDep ||
        let b1_ancestors = last_element (find_ancestors [] [] dag_scc.edges block1) in
        let b2_ancestors = last_element (find_ancestors [] [] dag_scc.edges block2) in
        match b1_ancestors, b2_ancestors with | Some b1, Some b2 -> compare_dag_nodes b1 b2 | _ ->false
      )
      ) dag_scc.edges in 
    let reachable_from_block1 = find_reachable_blocks block1 dag_scc (ref []) in
    let reachable_from_block2 = find_reachable_blocks block2 dag_scc (ref []) in
    let a = not (List.exists (fun b -> List.exists (fun e -> compare_dag_nodes e.dag_src b && compare_dag_nodes e.dag_dst block2) dag_scc.edges) reachable_from_block1) in
    let b = not (List.exists (fun b -> List.exists (fun e -> compare_dag_nodes e.dag_src b && compare_dag_nodes e.dag_dst block1) dag_scc.edges) reachable_from_block2) in
    (* let d = not (has_loop_carried block1.n pdg) in
    let e = not (has_loop_carried block2.n pdg) in *)
    let d = not (List.exists (
      fun e -> 
      (all_in_list_a_in_b e.dag_src.n block1.n 
      && all_in_list_a_in_b e.dag_dst.n block2.n
      ||
      all_in_list_a_in_b e.dag_src.n block2.n 
      && all_in_list_a_in_b e.dag_dst.n block1.n)
      && e.loop_carried
      ||
      match e.dep with | Commute _ -> true | _ -> false
      ) dag_scc.edges) in
    a && b&& d && c && (block1.label == Doall && block2.label = Doall) 
    && not (is_return_node block1)
    && not (is_return_node block2)
  in
  let rec merge_blocks block dag_scc visited =
    if List.mem block !visited then
      dag_scc, visited
    else begin 
      visited := block :: !visited;
      let rec find_mergeable_blocks acc = function
        | [] -> acc
        | hd :: tl ->
          let a = can_merge_blocks block hd dag_scc in
          if hd != block && a then find_mergeable_blocks (hd :: acc) tl
          else find_mergeable_blocks acc tl
      in
      let mergeable_blocks = find_mergeable_blocks [] dag_scc.nodes in
      match mergeable_blocks with
      | [] -> dag_scc, visited
      | _ ->
        List.iter (fun block -> visited := block :: !visited) mergeable_blocks;
        let merged_block = { n = List.flatten (List.map (fun b -> b.n) (block :: mergeable_blocks)); label = Doall } in
        let remaining_blocks = List.filter (fun b -> not (List.mem b mergeable_blocks) && b != block) dag_scc.nodes in
        let new_edges = List.filter (fun e -> not (all_in_list_a_in_b e.dag_src.n merged_block.n && all_in_list_a_in_b e.dag_dst.n merged_block.n)) dag_scc.edges in
        let nodes = merged_block :: remaining_blocks in
        let temp_nodes = match dag_scc.entry_node with | Some s -> {n = s.n; label = Doall} :: nodes | None -> nodes in
        let updated_edges = List.map (
          fun e -> 
          let src = List.find (fun n -> all_in_list_a_in_b e.dag_src.n n.n) temp_nodes in
          let dst = List.find (fun n -> all_in_list_a_in_b e.dag_dst.n n.n) temp_nodes in
          { e with dag_src = src; dag_dst = dst } ) new_edges in
        let updated_edges = remove_duplicate_edge updated_edges in 
        let updated_dag_scc = { dag_scc with nodes = nodes; edges = updated_edges } in
        merge_blocks merged_block updated_dag_scc visited
      end
  in
  let merge_all_blocks dag_scc visited =
    let blocks_to_merge = List.filter (fun node -> node.label = Doall) dag_scc.nodes in
    List.fold_left (fun (acc, visited) block -> merge_blocks block acc visited) (dag_scc, visited) blocks_to_merge
  in
  let merged_dag_scc, _ = merge_all_blocks dag_scc (ref []) in 
  merged_dag_scc


(* Function to retain the doall block with the maximum profile weight *)
let retain_max_profile_doall dag_scc =
  let is_labeled (node:dag_node) = 
    List.for_all (fun (p:pdg_node) -> match p.n with | EStmt ({elt = SBlock _; _}) -> true | _ -> false ) node.n
  in
  let doall_blocks = List.filter (fun node -> node.label = Doall) dag_scc.nodes in
  match doall_blocks with
  | [] -> dag_scc
  | _ ->
    let max_profile_weight_block = List.fold_left (fun acc block ->
      let weight = List.length block.n in
      if weight > (List.length acc.n) then block else acc
    ) (List.hd doall_blocks) (List.tl doall_blocks) in
    let remaining_doall_blocks = List.filter (fun node -> node != max_profile_weight_block) doall_blocks in
    let updated_max_profile_block = { max_profile_weight_block with label = Doall } in
    let updated_sequential_blocks = List.map (fun node -> if is_labeled node then node else { node with label = Sequential }) remaining_doall_blocks in
    let updated_nodes = updated_max_profile_block :: updated_sequential_blocks @ List.filter (fun node -> node.label != Doall) dag_scc.nodes in
    { dag_scc with nodes = updated_nodes }

(** TODO: revise this*)
(* Function to merge sequential blocks greedily *)
let merge_sequential_blocks dag_scc =
  (* let rec merge_blocks acc dag_scc =
    let sequential_blocks = List.filter (fun node -> node.label = Sequential) dag_scc.nodes in
    match sequential_blocks with
    | [] -> dag_scc
    | _ ->
      let first_block = List.hd sequential_blocks in
      let remaining_blocks = List.tl sequential_blocks in
      let merged_block = List.fold_left (fun acc node -> merge_blocks acc node) first_block remaining_blocks in
      let updated_nodes = merged_block :: List.filter (fun node -> node != first_block && not (List.mem node remaining_blocks)) dag_scc.nodes in
      merge_blocks merged_block { dag_scc with nodes = updated_nodes }
  in
  merge_blocks [] dag_scc *)
  
  let can_merge_blocks block1 block2 dag_scc =
    List.exists (
      fun e -> 
      all_in_list_a_in_b e.dag_src.n block1.n 
      && all_in_list_a_in_b e.dag_dst.n block2.n
      ||
      all_in_list_a_in_b e.dag_src.n block2.n 
      && all_in_list_a_in_b e.dag_dst.n block1.n
      ) dag_scc.edges
    && (block1.label == Sequential && block2.label = Sequential)
    && not (is_return_node block1)
    && not (is_return_node block2)
  in
  let rec merge_blocks block dag_scc visited =
    if List.mem block !visited then
      dag_scc, visited
    else begin 
      visited := block :: !visited;
      let rec find_mergeable_blocks acc = function
        | [] -> acc
        | hd :: tl ->
          let a = can_merge_blocks block hd dag_scc in
          if hd != block && a then find_mergeable_blocks (hd :: acc) tl
          else find_mergeable_blocks acc tl
      in
      let mergeable_blocks = find_mergeable_blocks [] dag_scc.nodes in
      match mergeable_blocks with
      | [] -> dag_scc, visited
      | _ ->
        List.iter (fun block -> visited := block :: !visited) mergeable_blocks;
        let merged_block = { n = List.flatten (List.map (fun b -> b.n) (block :: mergeable_blocks)); label = Sequential } in
        let remaining_blocks = List.filter (fun b -> not (List.mem b mergeable_blocks) && b != block) dag_scc.nodes in
        let new_edges = List.filter (fun e -> not (all_in_list_a_in_b e.dag_src.n merged_block.n && all_in_list_a_in_b e.dag_dst.n merged_block.n)) dag_scc.edges in
        let nodes = merged_block :: remaining_blocks in
        let temp_nodes = match dag_scc.entry_node with | Some s -> {n = s.n; label = Sequential} :: nodes | None -> nodes in
        let updated_edges = List.map (
          fun e -> 
          let src = List.find (fun n -> all_in_list_a_in_b e.dag_src.n n.n) temp_nodes in
          let dst = List.find (fun n -> all_in_list_a_in_b e.dag_dst.n n.n) temp_nodes in
          { e with dag_src = src; dag_dst = dst } ) new_edges in
        let updated_edges = remove_duplicate_edge updated_edges in 
        let updated_dag_scc = { dag_scc with nodes = nodes; edges = updated_edges } in
        (* print_dag_debug updated_dag_scc; *)
        merge_blocks merged_block updated_dag_scc visited
      end
  in
  let merge_all_blocks dag_scc visited =
    let blocks_to_merge = List.filter (fun node -> node.label = Sequential) dag_scc.nodes in
    List.fold_left (fun (acc, visited) block -> merge_blocks block acc visited) (dag_scc, visited) blocks_to_merge
  in
  let merged_dag_scc, _ = merge_all_blocks dag_scc (ref []) in 
  merged_dag_scc


let ctr = ref 0 

let incr_uid (ctr: int ref) =
  ctr := !ctr + 1;
  !ctr


let find_taskIDs_from_node_list dag_scc elemList: int list =
  let find_taskID_from_node dag_scc elem : int = 
    let tmp = ref 0 in 
    List.iteri (fun i n -> if (List.exists (fun s -> String.equal elem (Range.string_of_range_nofn s.l)) n.n) then tmp := i + 1 ) dag_scc.nodes;
    !tmp
  in
  List.map (find_taskID_from_node dag_scc) elemList

let sendDep_exists = ref []
let make_new_job_out = ref []
let make_new_job_in = ref []

let reconstructAST dag dag_scc_node (block: block node) taskID : block =
  let sendDeps = ref [] in
  let remove_and_find_nodes dag new_block old_block =
    List.filter (fun s -> not (List.mem s new_block)) old_block.elt
    |> List.map (fun s ->
      let removed_loc = Range.string_of_range_nofn s.loc in
      let [tid] = find_taskIDs_from_node_list dag [removed_loc] in
      (s.loc, tid)
    )
  in 
  let augment_block new_block removed_nodes =
    let augmented_block =
      List.fold_left (fun acc (l, task_id) ->
        if not (List.mem task_id !sendDeps) then begin
          sendDeps := task_id :: !sendDeps;
          make_new_job_in := task_id :: !make_new_job_in;
          make_new_job_out := taskID :: !make_new_job_out;
          acc @ [{ elt = SendDep (task_id, []); loc = l }]
        end 
        else acc
      ) new_block removed_nodes
    in
    List.sort (fun s1 s2 ->
      let (_, p1, _) = s1.loc in
      let (_, p2, _) = s2.loc in
      Int.compare (Range.line_of_pos p1) (Range.line_of_pos p2)
    ) augmented_block
  in
  let rec transform_block dag_scc_node (block: block node) : block * bool =
    let stmt_exist stmt node = 
      List.exists (fun s -> String.equal (Range.string_of_range s.l) (Range.string_of_range stmt.loc)) node.n
    in
    let res = match block.elt with
      | [] -> [] , true
      | stmt::tl ->
        begin match stmt.elt with
        (* | If (e, b, {elt = []; loc = l}) -> 
          let new_b, new_f = transform_block dag_scc_node b in
          if stmt_exist stmt dag_scc_node then begin
            let updated_b =
              if not new_f then 
                remove_and_find_nodes dag new_b b
                |> augment_block new_b
              else 
                new_b 
            in
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            (node_up stmt (If (e, node_up b updated_b, {elt = []; loc = l}))) :: rest, true && f
          end else begin
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            new_b @ rest, false && f
          end *)
        | If (e, b1, b2) ->
          let new_b1, f1 = transform_block dag_scc_node b1 in
          let new_b2, f2 = transform_block dag_scc_node b2 in
          if stmt_exist stmt dag_scc_node then begin
            let updated_b1 =
              if not f1 then 
                remove_and_find_nodes dag new_b1 b1
                |> augment_block new_b1
              else 
                new_b1 
            in
            let updated_b2 =
              if not f2 then 
                remove_and_find_nodes dag new_b2 b2
                |> augment_block new_b2
              else 
                new_b2 
            in
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            (node_up stmt (If (e, node_up b1 updated_b1, node_up b2 updated_b2))) :: rest, true && f
          end else begin
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            new_b1 @ new_b2 @ rest, false && f
          end
        | While (e, b) ->
          let new_body, f = transform_block dag_scc_node b in
          if stmt_exist stmt dag_scc_node then begin
            let updated_body = 
              if not f then 
                remove_and_find_nodes dag new_body b
                |> augment_block new_body
              else 
                new_body 
            in
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            (node_up stmt (While (e, node_up b updated_body))) :: rest, true && f
          end else begin
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            new_body @ rest, false && f
          end

        | For (v,e,s,b) -> 
          let new_body, f = transform_block dag_scc_node b in
          if stmt_exist stmt dag_scc_node then begin
            let updated_body = 
              if not f then 
                remove_and_find_nodes dag new_body b
                |> augment_block new_body
              else 
                new_body 
            in
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            (node_up stmt (For (v, e, s, node_up b updated_body))) :: rest, true && f
          end else begin
            let rest, f = transform_block dag_scc_node (node_up block tl) in
            new_body @ rest, false && f
          end
        | s -> 
          if stmt_exist stmt dag_scc_node 
          then begin
            let rest, f = (transform_block dag_scc_node (no_loc tl)) in
            stmt :: rest, true && f
          end
          else begin
            let rest, f = (transform_block dag_scc_node (no_loc tl)) in
            rest, false && f
          end
        end
    in 
    res
  in
  let bl = fst (transform_block dag_scc_node block) in 
  sendDep_exists := !sendDep_exists @ !sendDeps;
  List.fold_left (fun b i -> b @ [no_loc (SendEOP i)]) bl (List.rev !sendDeps)
  

let fill_task_dependency (dag: dag_scc) (tasks: (int * dswp_task) list) = 
  let find_taskID node = 
    let temp = ref 0 in 
    List.iteri (fun i n -> if compare_dag_nodes n node then temp := i + 1) dag.nodes;
    !temp
  in 
  let res = ref tasks in 
  List.iter (
    fun e -> 
    let src_taskID = find_taskID e.dag_src in
    let dst_taskID = find_taskID e.dag_dst in 
    match e.dep with
    | DataDep vars -> 
      let src_task = List.assoc src_taskID !res in
      if src_taskID = dst_taskID then begin
        let updated_task = (src_taskID, 
          {src_task with deps_out = {pred_task= dst_taskID; make_new_job=if List.mem dst_taskID !make_new_job_in && List.mem src_taskID !make_new_job_out then true else false; vars; commute_cond = {my_task_formals =[]; other_task_formals=[]; condition=None}} :: src_task.deps_out;
                         deps_in = {pred_task= src_taskID; make_new_job=if List.mem src_taskID !make_new_job_out && List.mem dst_taskID !make_new_job_in then true else false; vars; commute_cond = {my_task_formals =[]; other_task_formals=[]; condition=None}} :: src_task.deps_in}) in
        res := updated_task :: List.remove_assoc src_taskID !res
      end
      else begin
        let dst_task = List.assoc dst_taskID !res in
        let new_src_task = (src_taskID, {src_task with deps_out = {pred_task= dst_taskID; make_new_job= if List.mem dst_taskID !make_new_job_in && List.mem src_taskID !make_new_job_out then true else false; vars; commute_cond = {my_task_formals =[]; other_task_formals=[]; condition=None}} :: src_task.deps_out}) in
        let new_dst_task = (dst_taskID, {dst_task with deps_in = {pred_task= src_taskID; make_new_job= if List.mem src_taskID !make_new_job_out && List.mem dst_taskID !make_new_job_in then true else false; vars; commute_cond = {my_task_formals =[]; other_task_formals=[]; condition=None}} :: dst_task.deps_in}) in
        res := new_src_task :: new_dst_task :: 
        List.remove_assoc dst_taskID (List.remove_assoc src_taskID !res)
      end
    | Commute (c, args1, args2) ->
      let src_task = List.assoc src_taskID !res in
      if src_taskID = dst_taskID then begin
        let updated_task = (src_taskID,
          {src_task with deps_out = {pred_task= dst_taskID; make_new_job = false; vars = []; commute_cond = {my_task_formals =args1; other_task_formals= args2; condition=Some c}} :: src_task.deps_out;
                         deps_in = {pred_task= src_taskID; make_new_job = false; vars = []; commute_cond = {my_task_formals =args2; other_task_formals= args1; condition=Some c}} :: src_task.deps_in}) in
        res := updated_task :: List.remove_assoc src_taskID !res
      end
      else begin
        (* When src_taskID and dst_taskID are different, update both tasks *)
        let dst_task = List.assoc dst_taskID !res in
        let new_src_task = (src_taskID, {src_task with deps_out = {pred_task= dst_taskID; make_new_job = false; vars = []; commute_cond = {my_task_formals =args1; other_task_formals= args2; condition=Some c}} :: src_task.deps_out}) in
        let new_dst_task = (dst_taskID, {dst_task with deps_in = {pred_task= src_taskID; make_new_job = false; vars = []; commute_cond = {my_task_formals =args2; other_task_formals= args1; condition=Some c}} :: dst_task.deps_in}) in
        res := new_src_task :: new_dst_task ::
        List.remove_assoc dst_taskID (List.remove_assoc src_taskID !res) 
      end
    | _ ->()
  ) dag.edges;

  let out_tasks = (snd (List.split !res)) in 

  let update_sendDep_of_task (task: dswp_task) : dswp_task = 
    let rec update_body (body: block) : block = 
      match body with
      | [] -> body
      | stmt::tl ->
        let s' = begin match stmt.elt with
        | SendDep (i, vars) -> 
          let t = (List.find (fun tk -> tk.id == i) out_tasks)
          in
          SendDep (i, vars @ (List.concat_map (fun d -> if d.pred_task == task.id then d.vars else []) t.deps_in))
        | If(e,bl1,bl2) -> If(e, node_up bl1 (update_body bl1.elt), node_up bl2 (update_body bl2.elt))
        | While(e,bl) -> While(e, node_up bl (update_body bl.elt))
        | s -> s 
        end
        in 
        (node_up stmt s') :: (update_body tl)
    in
    let b' = node_up task.body (update_body task.body.elt) in
    {task with body = b'}
    
  in
  List.map (fun t -> update_sendDep_of_task t) out_tasks

let combine_dependencies (deps: Dswp_task.dependency list) : Dswp_task.dependency list =
  let combine_dep d1 d2 =
    { pred_task = d1.pred_task;
      make_new_job = d1.make_new_job || d2.make_new_job;
      vars = d1.vars @ d2.vars;
      commute_cond = 
        if d1.commute_cond = d2.commute_cond then d1.commute_cond
        else if d1.commute_cond.condition = None then d2.commute_cond
        else if d2.commute_cond.condition = None then d1.commute_cond
        else failwith "Conflicting commute conditions for the same pred_task"
    }
  in
  let rec combine_helper sorted_deps =
    match sorted_deps with
    | [] | [_] -> sorted_deps
    | d1 :: d2 :: rest ->
        if d1.pred_task = d2.pred_task then
          combine_helper ((combine_dep d1 d2) :: rest)
        else
          d1 :: combine_helper (d2 :: rest)
  in
  deps
  |> List.sort (fun d1 d2 -> compare d1.pred_task d2.pred_task)
  |> combine_helper

let generate_tasks dag_scc (block: block node) : init_task * dswp_task list =
  let dag_scc = ref dag_scc in 
  let generate_init_task () : init_task = 
    let decls, body = match !dag_scc.entry_node with 
    | Some entry ->
      let entry_stmts = ref [] in
      List.iter (fun {l=_; n=_;src = p} -> match p with | Some s -> entry_stmts:= !entry_stmts @ [s] | None -> ()) entry.n;

      let senddep_list = 
      List.fold_left 
      (fun b e -> 
      if compare_dag_nodes entry e.dag_src then begin
        let elem = List.map (fun s -> Range.string_of_range_nofn s.l) e.dag_dst.n in
        let i = List.hd (find_taskIDs_from_node_list !dag_scc elem) in
        if List.mem i !sendDep_exists then 
          b 
        else
          b @ [i] 
      end
      else b
      ) [] !dag_scc.edges
      in
      !entry_stmts, senddep_list
    | None -> [], []
    in 
    let body = remove_duplicate body in
    {decls = no_loc decls ; jobs = body; label= Dswp_task.Doall }
  in 
  let dag_scc = !dag_scc in 
  let temp_dag = dag_scc in 
  let rec generate_tasks_from_dag dag_scc (block: block node) : dswp_task list =
    match dag_scc.nodes with 
    | [] -> []
    | node::tl -> 
      let taskID = incr_uid ctr in 
      let body = reconstructAST temp_dag node block taskID in
      let label = match node.label with | Doall -> Dswp_task.Doall | Sequential -> Dswp_task.Sequential in 
      let t = {id = taskID; deps_in = []; deps_out = []; body = node_up block body; label } in 
      t :: (generate_tasks_from_dag {dag_scc with nodes = tl} block)
  in 
  let tasks = generate_tasks_from_dag dag_scc block in
  debug_print (lazy (Printf.sprintf "Number of tasks: %d\n" (List.length tasks)));
  let init_task = generate_init_task () in 
  let new_edges = List.filter (fun {dag_src= s} -> match dag_scc.entry_node with | Some e -> not (compare_dag_nodes s e) | None -> true) dag_scc.edges in
  let new_edges = List.filter (fun {dag_dst= s} -> match dag_scc.entry_node with | Some e -> not (compare_dag_nodes s e) | None -> true) new_edges in
  let tasks = fill_task_dependency {dag_scc with edges = new_edges} (List.map (fun t -> (t.id, t)) tasks) in
  let tasks = List.map (fun t-> {t with deps_in = combine_dependencies t.deps_in; deps_out = combine_dependencies t.deps_out}) tasks in
  init_task, tasks

(* use empty data dependency edges intead of using SendEOP *)
let add_empty_data_dep_edges dag_scc =
  let new_edges = ref dag_scc.edges in
  let check_node_is_spawned node =
    List.exists (fun e -> compare_dag_nodes e.dag_dst node && e.dep == ControlDep) dag_scc.edges 
  in
  List.iter (fun edge ->
    match edge.dep with
    | DataDep _ -> 
        let ancestors = find_ancestors [] [] dag_scc.edges edge.dag_src in
        List.iter (fun ancestor ->
          if (not (compare_dag_nodes ancestor edge.dag_dst)) && check_node_is_spawned edge.dag_dst && check_node_is_spawned edge.dag_src then begin
            let new_edge = { dag_src = ancestor; dag_dst = edge.dag_dst; dep = (DataDep []); loop_carried = false } in
            if not (List.mem new_edge !new_edges) && not (List.exists (fun e -> compare_dag_nodes e.dag_src new_edge.dag_src && compare_dag_nodes e.dag_dst new_edge.dag_dst && match e.dep with | DataDep _ -> true | _ -> false) !new_edges) then
              new_edges := new_edge :: !new_edges
            else
             ()
          end 
        ) ancestors
    | _ -> ()
  ) dag_scc.edges;
  apply_distinct_pairs (fun x y -> 
    if !Util.manual_dependency then begin 
      let new_edge = { dag_src = x; dag_dst = y; dep = (DataDep []); loop_carried = false } in
        if not (List.mem new_edge !new_edges) && not (is_return_node x) && not (is_return_node y) then begin
          new_edges := new_edge :: !new_edges
        end
        else ()
    end
  ) dag_scc.nodes;
  { dag_scc with edges = !new_edges }

let thread_partitioning dag_scc pdg (threads: int list) body =
  debug_print (lazy "Merging DAG_scc:\n");
  let merged_dag = merge_doall_blocks dag_scc pdg in
  let dag_scc_with_max_profile = retain_max_profile_doall merged_dag in
  print_dag_debug dag_scc_with_max_profile;
  let dag_scc_merged_sequential = merge_sequential_blocks dag_scc_with_max_profile in
  let merged_dag = dag_scc_merged_sequential in 
  print_dag_debug merged_dag;
  print_dag merged_dag "/tmp/merged-dag-scc.dot" dag_pdgnode_to_string;
  let merged_dag_with_added_deps = add_empty_data_dep_edges merged_dag in
  debug_print (lazy "add empty data dependency edges:\n");
  print_dag_debug merged_dag_with_added_deps;
  print_dag merged_dag_with_added_deps "/tmp/merged-dag-scc2.dot" dag_pdgnode_to_string;
  let init_task, tasks = generate_tasks merged_dag_with_added_deps body in 
  if !Util.debug then begin
    Printf.printf "Init Task -> \n %s \n [%s] \n" (AstPP.string_of_block init_task.decls) (String.concat ", " (List.map Int.to_string init_task.jobs));
    List.iter (fun t -> Printf.printf "Task ID = %d ->\n %s \n" t.id (AstPP.string_of_block t.body)) tasks;
    List.iter (fun t -> Printf.printf "%s \n" (str_of_task t)) tasks end;
  init_task, tasks



let ps_dswp (body: block node) m_loc m_args (g: global_env) globals = 
  List.iter (
    fun (id, (ty,e)) -> 
    let decl = Gvdecl (no_loc { name = id; ty = ty; init = e}) in 
    decl_vars := !decl_vars @ [decl]
  ) globals;
  m_vars := m_args;

  commutativity_spec_exist := (List.length (g.group_commute) > 0);

  let pdg = build_pdg body.elt m_loc g.group_commute in 
  print_pdg_debug pdg;
  print_pdg pdg "/tmp/pdg.dot";
  let sccs = find_sccs pdg in
  if !Util.debug then begin
    Printf.printf "Strongly Connected Components:\n";
    print_sccs sccs end;
  let dag_scc = coalesce_sccs pdg sccs in
  debug_print (lazy "DAG_SCCs:\n");
  print_dag_debug dag_scc;
  print_dag dag_scc "/tmp/dag-scc.dot" dag_pdgnode_to_string;
  let init_task, tasks = thread_partitioning dag_scc pdg [] body in 
  debug_print (lazy (Printf.sprintf "gen_tasks called with %d globals\n" (List.length !decl_vars)));
  if !codegen then begin
    Codegen_c.gen_tasks (!decl_vars) tasks;
    Codegen_c.print_tasks init_task tasks "/tmp/tasks.dot" end;
  generated_init_task := Some init_task;
  generated_tasks := tasks;
  generated_decl_vars := !decl_vars;
