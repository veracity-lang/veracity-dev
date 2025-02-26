open Ast
open Domainslib

type t = unit Domain.t

let create : (unit -> unit) -> t =
  Domain.spawn

let join : t -> unit =
  Domain.join

let new_job t e : unit = 
  failwith "todo - new_job for parallel_multicore"

let set_task_def tl : unit = 
  failwith "todo - new_job for parallel_multicore"

let scheduler () = 
  failwith "todo - scheduler for parallel_multicore"