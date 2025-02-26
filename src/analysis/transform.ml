(* open Ast
open Ast_print
open Format
open Range
open Util
open Exe_pdg


let ctr = ref 0 

let incr_uid (ctr: int ref) =
  ctr := !ctr + 1;
  !ctr

let create_label () : string = 
    "Block" ^ string_of_int (incr_uid ctr)

let mk_blocklabel l : blocklabel =
    (l, None)
 
let get_label id : string = 
    "Block" ^ string_of_int id 

let rec string_of_stmt_aux (s: exe_stmt node) : string =
    begin match s.elt with 
    | ESIf (e, b1, b2) -> sp "ESIf (%s, %s, %s)"
        (AstML.string_of_exp e) b1 b2
    | ESFor (d,e,s,b) -> sp "For (%s, %s, %s, %s)"
                          (AstML.string_of_list AstML.string_of_vdecl_aux d) 
                          (AstML.string_of_option AstML.string_of_exp e)
                          (AstML.string_of_option AstML.string_of_stmt s) b
    | ESWhile (e,b) -> sp "While (%s, %s)" (AstML.string_of_exp e) b
    (* | ESGoto b -> sp "Goto (%s)" (string_of_stmt_aux b) *)
    | ESSBlock (blocklabel, b) -> let (Some (l, _)) = blocklabel in sp "ESSBlock %s: %s" l (string_of_block b)
    | Stmt s -> AstML.string_of_stmt s 
    end

and string_of_block (b:exe_block node) : string =
    AstML.string_of_list string_of_stmt_aux b.elt


let rec stmt_to_exeS s : exe_stmt = 
    failwith "not implemented"


(* let rec build_pdg (block: block) pdg : exe_pdg = 
    List.fold_left (
        fun pdg -> fun stmt -> 
        match stmt with 
        | If (e, blk1, blk2) ->
            let head_label = create_label() in
            let ctr_temp = !ctr in 
            (* let add_edge (pdg : exe_pdg) (src : exe_stmt) (dst : exe_stmt) dep *)

            let pdg = build_pdg blk2.elt (build_pdg blk1.elt pdg) in 
            let l1 = get_label (ctr_temp + 1) in 
            let l2 = if (blk2.elt != []) then get_label !ctr else "" in 
            
            let if_stmt = node_up stmt (ESIf (e, l1, l2)) in 
            let s = ESSBlock (Some (mk_blocklabel head_label), no_loc [if_stmt]) in 
            add_node pdg s 

        | _ -> 
            let s = node_up stmt (Stmt stmt) in 
            let n = ESSBlock (Some (mk_blocklabel @@ create_label()), no_loc [s]) in 
            add_node pdg n
        (* let estmt = stmt_to_exeS s in 
        add_node pdg estmt   *)
    ) pdg block *)
(* 
let rec f i pdg =
    | [] -> pdg
    | h::tl -> if (i >= (List.length nodes_temp)) then add_edge pdg s n else pdg *)

let rec build_pdg (block: block) pdg : exe_pdg =
    match block with
    | [] -> pdg
    | stmt::tl ->
        let updated_pdg = begin match stmt.elt with 
        | If (e, blk1, blk2) ->
            let head_label = create_label() in
            let ctr_temp = !ctr in 
            (* let add_edge (pdg : exe_pdg) (src : exe_stmt) (dst : exe_stmt) dep *)
            let nodes_temp = pdg.nodes in 

            let pdg = build_pdg blk2.elt (build_pdg blk1.elt pdg) in 
            let l1 = get_label (ctr_temp + 1) in 
            let l2 = if (blk2.elt != []) then get_label !ctr else "" in 
            
            let if_stmt = node_up stmt (ESIf (e, l1, l2)) in 
            let s = ESSBlock (Some (mk_blocklabel head_label), no_loc [if_stmt]) in 


            (* List.fold_left (fun i n ->  if (i >= (List.length nodes_temp)) then add_edge pdg s n else pdg) pdg pdg.nodes *)

            add_node pdg s 
             
        | While (e, bl) ->
            let head_label = create_label() in
            let ctr_temp = !ctr in 

            let pdg = build_pdg bl.elt pdg in 
            let l = get_label (ctr_temp + 1) in
            let while_stmt = node_up stmt (ESWhile (e, l)) in 
            let n = ESSBlock (Some(mk_blocklabel head_label), no_loc [while_stmt]) in
            
            (* List.fold_left (
                fun pdg -> fun s -> (add_edge pdg n (stmt_to_exeS s) ControlDep)
            ) pdg bl.elt *)

            add_node pdg n 

        | For (vdecll, exp, sl, bl) ->
            let head_label = create_label() in
            let ctr_temp = !ctr in 

            let pdg = build_pdg bl.elt pdg in 
            let l = get_label (ctr_temp + 1) in
            let for_stmt = node_up stmt @@ ESFor (vdecll, exp, sl, l) in 
            let s = ESSBlock (Some(mk_blocklabel head_label), no_loc [for_stmt]) in
            add_node pdg s 

        | SBlock (blocklabel, bl) -> 
            let s = node_up stmt (Stmt stmt) in 
            let n = ESSBlock (blocklabel, no_loc [s]) in 
            add_node pdg n

        | _ -> 
            let s = node_up stmt (Stmt stmt) in 
            let n = ESSBlock (Some (mk_blocklabel @@ create_label()), no_loc [s]) in 
            add_node pdg n
        end 
        in 
        build_pdg tl updated_pdg *)
