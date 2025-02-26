open Ast
open Ast_print
type dswp_taskid = int 

type commute_condition = {
  my_task_formals: string list; (* TODO: Can make string * type? *)
  other_task_formals: string list;
  condition: exp node option
}
(* t_i can depend on a list of variables written by some predecessor t_j *)
type dependency = {
  pred_task: dswp_taskid;
  make_new_job: bool;
  vars: (ty * id) list;
  commute_cond : commute_condition
}

type exe_label = Doall | Sequential

type dswp_task = {
  id : dswp_taskid;
  deps_in : dependency list; (* a list of other tasks/vars that I depend on *)
  deps_out : dependency list; (* a list of other tasks/vars that I provide for *)
  body: block node;
  label: exe_label;
}

type init_task = {
  decls: block node;
  jobs: dswp_taskid list;
  label: exe_label;
}

let str_of_vars_list (vlist : (ty * id) list) : string  = 
  (String.concat ";" (List.map (fun (t,i) -> 
       Printf.sprintf "%s %s" (AstPP.string_of_ty t) i
  ) vlist))

let str_of_exp_list (elist : exp node list) : string =
  (String.concat "," (List.map AstML.string_of_exp elist))

let str_of_task_deps deplist = 
  "{"
  ^(String.concat " AND " (List.map (fun dep ->
    match dep.commute_cond.condition with 
    | None -> Printf.sprintf "from %d [new job: %b]: %s" dep.pred_task dep.make_new_job (str_of_vars_list dep.vars)
    | Some c -> 
     Printf.sprintf "from %d [new job: %b]: %s / commute_cond: [%s],[%s] => %s" dep.pred_task dep.make_new_job (if not (Util.null dep.vars) then (str_of_vars_list dep.vars) else "[]") (String.concat "," dep.commute_cond.my_task_formals)(String.concat "," dep.commute_cond.other_task_formals) (AstPP.string_of_exp c)
  ) deplist))
  ^"}"

let str_of_task tsk = 
    Printf.sprintf "{Task %d:\n  deps_in:%s\n  deps_out:%s\n  label:%s}"
      tsk.id (str_of_task_deps tsk.deps_in) (str_of_task_deps tsk.deps_out)
      (match tsk.label with | Doall -> "DOALL" | Sequential -> "Seq")

let rec calculate_semaphores tlist : (dswp_taskid * dswp_taskid) list =
  match tlist with 
  | [] -> []
  | (tk::rest) ->
      (List.map (fun dep -> (dep.pred_task,tk.id)) tk.deps_in )
      @ (calculate_semaphores rest)

(* things like t1_to_t2_x *)
let rec calculate_handoff_vars tlist : (ty * id * dswp_taskid * dswp_taskid) list =
  match tlist with 
  | [] -> []
  | (tk::rest) ->
      List.flatten (List.map (fun dep -> 
        (List.map (fun (t,x) -> (t,x,dep.pred_task,tk.id) ) dep.vars)
      ) tk.deps_in)
      @ (calculate_handoff_vars rest)
      
(*
Example:

task1:
  { id=1; deps_in:(task1,p); deps_out:[(task1,p->next);(task2,p);(task3,p)]
    body="id=p->id;if(!visited[id])visited[id]=true;"}

task2:
  { id=2; deps_in:(task1,p); deps_out:[]
    body="q=p->inner_list; .."}

task3:
  { id=3; deps_in:(task1,p); deps_out:[]
    body="q=p->inner_list; .."}
*)

let mk_int_dep pred_id var_id = {pred_task=pred_id; make_new_job=false; vars=[(TInt,var_id)]; commute_cond = {my_task_formals =[]; other_task_formals=[];condition=None}}

let example_var_decls () =
  [
    Gvdecl(no_loc { name="p"; ty=TInt; init=(no_loc (CInt 0L))})
  ]

let example_tasks () : dswp_task list = 
  [
    { id=1; 
      deps_in=[(mk_int_dep 1 "p")]; 
      deps_out=[(mk_int_dep 1 "pnext");(mk_int_dep 2 "p");(mk_int_dep 3 "p")];
      body=no_loc [(no_loc (Ret(None)))];
      label=Doall
    };
    { id=2; 
      deps_in=[(mk_int_dep 1 "p")]; 
      deps_out=[];
      body=no_loc [(no_loc (Ret(None)))];
      label=Doall
    };
    { id=3; 
      deps_in=[(mk_int_dep 1 "p")]; 
      deps_out=[];
      body=no_loc [(no_loc (Ret(None)))];
      label=Doall
    }
  ]
