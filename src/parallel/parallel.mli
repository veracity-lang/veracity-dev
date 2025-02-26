type t
open Ast

val create : (unit -> unit) -> t
val join : t -> unit
val new_job : int -> 'a -> unit
val scheduler : unit -> unit
val set_task_def : 'a -> unit