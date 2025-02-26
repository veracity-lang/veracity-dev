(*
  Code Generation via a C compiler:
    Convert Veracity statements to C code

  Most functions are parameterized first by the current task (Dswp_task.task)
  
*)
open Ast
open Ast_print
open Util
open Dswp_task

exception TaskCodeGenErr of string

(* TODO: Make all of these local to gen_prog *)
let indent = ref 0
let mk_newline () = "\n" ^ String.make !indent ' '

(* TODO: Return type -- not pure string, but a string with state? state monad and do St string?
   To capture env vars such as string/array constants. *)

let rec gen_ty = function
    | TVoid -> "void"
    | TInt -> "int"
    | TBool -> "int" (* TODO: Not ansi C. can use int, or stdbool.h? *)
    | TStr -> "const char*"
    | TArr(ty) -> sp "%s*" (gen_ty ty)
    | THashTable(kty, vty) -> "<<new Thashtable stmt>>" (*raise @@ NotImplemented "gen_ty THashTable"*)
    | TChanR -> "TChanR"
    | TChanW -> "TChanW"
    | TStruct(id) -> raise @@ NotImplemented "gen_ty TStruct"

let rec gen_expnode x = gen_exp x.elt
and gen_of_bool = function true -> "1" | _ -> "0"
and gen_exp = function
    | CNull(ty) -> "0"
    | CBool(b) -> gen_of_bool b
    | CInt(i) -> Int64.to_string i (* ^ "L" *)
    | CStr(s) -> sp "\"%s\"" s
    | CArr(ty, e) -> raise @@ NotImplemented "gen_exp CArr"
    | NewArr(ty, e) -> "malloc(sizeof("^(gen_ty ty)^") * "^(gen_expnode e)^")" 
    | NewHashTable(var, kty, vty) -> "<<new hashtable stmt>>" (*raise @@ NotImplemented "gen_exp NewHashTable"*)
    | Id(id) -> (!mangle id)
    | Index(arr, idx) -> sp "(%s[%s])" (gen_expnode arr) (gen_expnode idx)
    | CallRaw(id, es) -> sp "(%s(%s))" id (String.concat ", " @@ List.map gen_expnode es)
    | Call(var, es) -> begin match var with
        | MethodM(id, tmethod) -> raise @@ NotImplemented "gen_exp Call.MethodM"
        | MethodL(id, lmethod) -> raise @@ NotImplemented "gen_exp Call.MethodL"
        end
    | Bop(bop, l, r) -> sp "(%s%s%s)" (gen_expnode l) (AstPP.string_of_binop bop) (gen_expnode r)
    | Uop(uop, e) -> sp "(%s%s)" (AstPP.string_of_unop uop) (gen_expnode e)
    | Ternary(g,t,e) -> sp "(%s?%s:%s)" (gen_expnode g) (gen_expnode t) (gen_expnode e)
    | CStruct(id, e) -> raise @@ NotImplemented "gen_exp CStruct"
    | Proj(e, id) -> raise @@ NotImplemented "gen_exp Call.Proj"

and gen_stmt tsk = function
    | Assn(lhs, rhs) -> sp "%s = %s" (gen_expnode lhs) (gen_expnode rhs)
    | Decl(id, (ty, rhs)) -> env := (ty, id) :: !env; sp "%s %s = %s" (gen_ty ty) (!mangle id) (gen_expnode rhs)
    | Ret(eo) -> begin match eo with
        | Some e -> sp "return %s" @@ gen_expnode e
        | None -> "return"
        end
    | SCallRaw(id, args) -> sp "%s(%s)" id (String.concat ", " @@ List.map gen_expnode args)
    | SCall(var, args) -> begin match var with
        | MethodM(id, tmethod) -> raise @@ NotImplemented "gen_stmt SCall.MethodM"
        | MethodL(id, lmethod) -> raise @@ NotImplemented "gen_stmt SCall.MethodL"
        end
    | If(guard, t, e) -> sp "if(%s) %s%selse %s" (gen_expnode guard) (gen_blocknode tsk t) (mk_newline ()) (gen_blocknode tsk e)
    | For(inits, guard, update, body) -> sp "for(%s; %s; %s) %s" (String.concat ", " @@ List.map (fun (id, (ty, rhs)) -> sp "%s %s = %s" (gen_ty ty) (!mangle id) (gen_expnode rhs)) inits) (guard |> Option.map gen_expnode |> Option.value ~default:"") (update |> Option.map (gen_stmtnode tsk) |> Option.value ~default:"") (gen_blocknode tsk body)
    | While(guard, body) -> sp "while(%s) %s" (gen_expnode guard) (gen_blocknode tsk body)
    | Raise(e) -> raise @@ NotImplemented "gen_stmt Raise"
    | Commute(var, phi, bodies) -> raise @@ TaskCodeGenErr "gen_stmt should not have Commute stmts"
    | Havoc(id) -> sp "/* %s = __VERIFIER_nondet_int() */" (!mangle id)
    | Assume(e) -> sp "/* assume%s */" (gen_expnode e)
    | SBlock(blocklabel,block) -> sp "%s" (gen_blocknode tsk block) (* TODO: check *)
    | SendDep(tsk_id, vars) ->
      gen_senddep tsk tsk_id (* TODO: add vars *)
    | SendEOP id -> sp "/* SendEOP (%d) */" id
      


and gen_senddep tsk other_id = 
  try 
    (* Look up in my dependencies for other_id *)
    Printf.printf "gen_senddep: I am task %d. looking for other task %d in my deps: \n " 
      tsk.id other_id;
    print_endline (str_of_task tsk);
    let thedep = List.find (fun d -> d.pred_task == other_id) tsk.deps_out in 
       String.concat "\n" (["";
         ("        // Begin Send Deps to task " ^ (string_of_int other_id));
         ("        //   Vars to send: "^(str_of_vars_list thedep.vars));
         (Printf.sprintf "        printf(\"task_%d: sendout outputs to task %d\");" tsk.id other_id);
        ]
        @
        (List.map (fun (dep_type, dep_id) ->
          (Printf.sprintf "        %s %s = t%d_to_t%d_%s;" 
            (gen_ty dep_type)
            dep_id
            tsk.id
            other_id
            dep_id
          )
        ) thedep.vars)
        @
        [
          (Printf.sprintf "        sem_post(&t%d_to_t%d_sem);" tsk.id other_id);
          ("        // End Send Deps to task " ^ (string_of_int other_id));
          ""
        ])
  with 
    Not_found -> failwith ("gen_senddep: didn't find the other task id:" ^ (Int.to_string other_id))

and gen_stmtnode tsk x = gen_stmt tsk x.elt
and gen_block tsk b = let indent_pre = !indent in 
    indent := indent_pre + 4;
    let res = "{" ^ mk_newline () ^ String.concat (mk_newline ()) @@ List.map (fun x -> x ^ ";") @@ List.map (gen_stmtnode tsk) b in
    indent := indent_pre;
    res ^ mk_newline () ^ "}"
and gen_blocknode tsk b = gen_block tsk b.elt

and mangle = ref Fun.id

and env = ref [] (* TODO: When to refresh? etc? Better as state monad *)
    
let gen_decl tsk = function
    | Gvdecl(dnode) -> let d = dnode.elt in sp "%s %s = %s;" (gen_ty d.ty) d.name (gen_expnode d.init)
    | Gmdecl(dnode) -> let d = dnode.elt in sp "%s %s(%s) %s" (gen_ty d.mrtyp) d.mname (String.concat ", " @@ List.map (fun (ty, id) -> sp "%s %s" (gen_ty ty) id) d.args) (gen_blocknode tsk d.body)
    | Gsdecl(d) -> raise @@ NotImplemented "gen_decl Gsdecl"

let gen_prog tsk prog =
    String.concat "\n\n" @@ List.map (gen_decl tsk) prog

(* 
test this as:
  ./vcy.exe interp ../benchmarks/global_commutativity/ps-dswp.vcy    
*)
let gen tsk b : unit = 
  let str = gen_block tsk b in 
  let oc = open_out "/tmp/autogen_tasks.c" in
  output_string oc str;
  close_out oc;
  print_endline "Codegen_c: wrote /tmp/autogen_tasks.c"

let gen_semaphores tlist =
  List.fold_left (fun acc (tid1,tid2) ->
    acc ^ (
      Printf.sprintf "sem_t t%d_to_t%d_sem;\n" tid1 tid2
    )
  ) "\n// ##### Semaphore Declarations #####\n" (calculate_semaphores tlist)
  (* "todo - use calculate_semaphores. sem_t t1_to_t1_sem;  " *)

let gen_init tlist = 
   "\n// ##### Initialization #####\n"
  ^"void autogen_initialize() {\n"
  ^(
  List.fold_left (fun acc (tid1,tid2) ->
    acc ^ (
      Printf.sprintf "  sem_init(&t%d_to_t%d_sem, 0, 0);\n" tid1 tid2
    )
  ) "" (calculate_semaphores tlist)
  )
  ^"}\n"

let gen_gvar_decls tsk gv_decls : string =
  "\n// ##### Declare global variables #####\n"
  ^(List.fold_left (fun acc gv_decl ->
    acc ^ (gen_decl tsk gv_decl) ^ "\n"
  ) "" gv_decls)

let gen_handoff_vars t_id_tid1_tid2_list : string = 
  "\n// ##### Declare handoff (t1_t2_x) variables #####\n"
  ^(List.fold_left (fun acc (t,nm,tid1,tid2) ->
    let nm = Printf.sprintf "%s t%d_to_t%d_%s;\n" (gen_ty t) tid1 tid2 nm in
    acc ^ nm ^ "\n"
  ) "" t_id_tid1_tid2_list)

let gen_task_loader tlist : string = 
  "\n// ##### Method to load task body #####\n"
  ^"task_t* autogen_loadtask(int i) {\n"
  ^"  task_t* t = malloc(sizeof(task_t));\n"
  ^"  t->id = i;\n"
  ^"  switch (i) {\n"
  ^(String.concat "\n" (List.map (fun tsk ->
       "     case "^(string_of_int tsk.id)^":\n"
      ^"        t->function = task_"^(string_of_int tsk.id)^";\n"
      ^"        t->data = (void*)(intptr_t)i; /* just an int for now */\n"
      ^"        break;"
  ) tlist))
  ^"\n   }\n"
  ^"   return t;\n"
  ^"}\n"

let gen_tasks gvar_decls tlist = 
  let rec help ts : string list =
    match ts with 
    | [] -> [""]
    | (t::rest) ->
      let tid = (string_of_int t.id) in
      indent := 8;
      String.concat "\n" ([
        "\n// ##### TASK " ^ tid ^ " #############";
        "void task_" ^ tid ^ "(void *arg) {";
        "    while(1) {";
        "        // Collect my inputs"] @
        (List.map (fun dep_in -> 
          (Printf.sprintf "        printf(\"task_%d: waiting for input from task %d\");\n" t.id dep_in.pred_task)
          ^
          (Printf.sprintf "        sem_wait(&t%d_to_t%d_sem);\n" dep_in.pred_task t.id)
          ^
          (* collect all the vars *)
          (String.concat "\n" (List.map (fun (dep_type, dep_id) ->
            (Printf.sprintf "        %s %s = t%d_to_t%d_%s;" 
              (gen_ty dep_type)
              dep_id
              dep_in.pred_task
              t.id
              dep_id
            )
          ) dep_in.vars))
        ) t.deps_in)
        @
       ["        // End of Input collection";
       "";
       "        // ---- Begin task body";
       "        "^(gen_blocknode t t.body);
       "        // ---- End task body";
       "";
       "    } /* end task loop */";
       "}";
       ]
       )
       :: (help rest)
  in 
  let oc = open_out "/tmp/autogen_tasks.c" in
  output_string oc "// ##### AUTOGENERATED TASKS FROM VERACITY ####################\n";
  output_string oc "#include <pthread.h>\n";
  output_string oc "#include <semaphore.h>\n";
  output_string oc "#include \"task.h\"\n";
  output_string oc ("int autogen_taskcount() { return "^(string_of_int (List.length tlist))^"; }\n");
  output_string oc (gen_semaphores tlist);
  output_string oc (gen_gvar_decls (List.hd tlist) gvar_decls);
  output_string oc (gen_handoff_vars (Dswp_task.calculate_handoff_vars tlist));
  output_string oc (gen_init tlist);
  output_string oc (String.concat "\n\n" (help tlist));
  output_string oc (gen_task_loader tlist);
  close_out oc;
  print_endline "Codegen_c: wrote /tmp/autogen_tasks.c"

let edge_of_dep myid dp direction : string = 
  let (src,dst) = if direction then (myid,dp.pred_task) else (dp.pred_task,myid) in
  let note = if direction then "out:" else "in:" in
  Printf.sprintf "\"%d\" -> \"%d\" [label=\"T%d %s %s\"];\n"
      src dst myid note (String.concat "," (List.map (fun (t,i) -> i) dp.vars))

let dot_of_task_body tsk : string = 
  let t = gen_blocknode tsk tsk.body in
  let t' = Str.global_replace (Str.regexp_string "\n") "\\n" t in
  let t'' = Str.global_replace (Str.regexp_string "  ") " " t' in
  let es = Str.global_replace (Str.regexp_string "\"") "\\\"" t'' in
  if String.length es > 300 then 
    (String.sub es 0 300)^"...TRUNC ("^(string_of_int (String.length es))^" chars)"
  else es 

let print_tasks init_task tlist fn : unit = 
  let oc = open_out fn in
  output_string oc (String.concat "\n" [
    "digraph G {";
    "  graph [rankdir=\"TB\", fontname=\"Arial\", fontsize=24, label=\"Tasks\", labelloc=t, labeljust=l]";
    (* Styles *)
    "  node [shape=box, style=\"filled\", fontname=\"Courier\", margin=0.05]";
    "  edge [arrowhead=vee, arrowsize=1, fontname=\"Courier\"]";
    (* Nodes *)
    (* "\"0" "\"[label=\"Init Task: "^(dot_of_task_body {id =0; body=init_task.decls; deps_in=[]; deps_out=[]; label=init_task.label})^"\"];\n "^(List.fold_left (fun acc j -> acc ^ "," ^ (Int.to_string j)) "" init_task.jobs)^" \n" ^ *)
    (List.fold_left (fun acc tsk -> acc ^ "\"" ^ (string_of_int tsk.id)
    ^ "\" [label=\"Task "^(string_of_int tsk.id)^": "^(dot_of_task_body tsk)^"\"];\n") "" tlist);
    (* edges *)
    List.fold_left (fun acc tsk -> acc ^ 
        (List.fold_left (fun acc' din -> acc'^(edge_of_dep tsk.id din false)) "" tsk.deps_in)
        ^
        (List.fold_left (fun acc' dout -> acc'^(edge_of_dep tsk.id dout true)) "" tsk.deps_out)
    ) "" tlist;
    "}\n";
  ]);
  print_endline ("dag written to " ^ fn);
  close_out oc
