type t = int64 ref * Mutex.t

let init () : t =
  ref 0L, Mutex.create ()

let increment ((c, m) : t) =
  Mutex.protect m (fun () -> c := Int64.add !c 1L)

let decrement ((c, m) : t) =
  Mutex.protect m (fun () -> c := max (Int64.sub !c 1L) 0L)

let read ((c, m) : t) =
  Mutex.protect m (fun () -> !c)
