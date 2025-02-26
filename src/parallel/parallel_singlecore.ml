type t = Thread.t

let create (f : unit -> unit) : t =
  Thread.create f ()

let join : t -> unit =
  Thread.join

(* type job = { tid: taskid; env: env; } *)

let new_job t e : unit = 
  failwith "todo - new_job for parallel_singlecore"

let set_task_def tl : unit = 
  failwith "todo - new_job for parallel_singlecore"

let scheduler () = 
  failwith "todo - scheduler for parallel_singlecore"