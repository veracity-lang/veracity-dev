exception UnreachableFailure of string
exception NotImplemented of string

let debug = ref false

(* let dprintf fmt = if !debug then Printf.printf fmt else Printf.ifprintf stdout fmt *)

let debug_print (s : string lazy_t) =
  if !debug then (print_string (Lazy.force s); flush stdout)

let debug_trunc (s : string) = if String.length s > 256 then String.sub s 0 241 ^ "... [truncated]" else s

let sp = Printf.sprintf

let servois2_synth_option = ref Servois2.Synth.default_synth_options

let check_prover () = !servois2_synth_option.prover

let assoc_update (k : 'a) (v : 'b) (l : ('a * 'b) list) =
  (k,v) :: List.remove_assoc k l

let swap (a,b) = b,a

let flip f x y = f y x

let compose f g x = f (g x)

let ap f x = f x

let rec repeat f n = if n <= 0 then () else (f (); repeat f (n - 1))

let const c f = c

let seq a b = b

let null = function
  | [] -> true
  | _ -> false

let trd (a, b, c) = c

let uncurry f (x, y) = f x y

let first f (x, y) = (f x, y)

let second f (x, y) = (x, f y)
  
let rec square_list_unordered = function
  | [] -> []
  | h::t ->
    (h,h) :: List.map (fun x -> h,x) t
    @ square_list_unordered t

let rec list_remove_instance a = function
  | [] -> []
  | h::t when h = a -> t
  | h::t -> h :: list_remove_instance a t

(* Randomize order of items in a list *)
let shuffle =
  let randomize = fun c -> Random.bits (), c in
  fun lst ->
    lst |>
    List.map randomize |>
    List.sort compare |>
    List.map snd


(* Read lines from an in_channel until EOF.
 * Close channel at the end *)
let read_all_in (chan : in_channel) : string list =
  let lines = ref [] in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let array_suffix (a : 'a array) (i : int) : 'a array =
  Array.sub a i (Array.length a - i)

let mod64 (x : int64) (y : int64) : int64 =
  (* TODO make actually work with int64 *)
  Int64.of_int (Int64.to_int x mod Int64.to_int y)

let explode_string s =
  let rec f i l =
    if i < 0 then l 
    else f (i - 1) (s.[i] :: l)
  in f (String.length s - 1) []

let collapse_string s =
  List.map (String.make 1) s |>
  String.concat ""

(* Reduce any more than 2 consecutive newlines to 2 newlines *)
let normalize_newlines =
  let rec f = function
  | [] -> []
  | '\n'::'\n'::'\n'::t ->
    f @@ '\n'::'\n'::t
  | '\n'::'\n'::t ->
    '\n'::'\n':: f t
  | c::t ->
    c :: f t
  in fun s ->
    explode_string s |>
    f |>
    collapse_string

(*** For printing colored strings in bash ***)
module ColorPrint = struct
  type color_code =
    | Default
    | Black      | White
    | Red        | Light_red
    | Green      | Light_green
    | Yellow     | Light_yellow
    | Blue       | Light_blue
    | Magenta    | Light_magenta
    | Cyan       | Light_cyan
    | Light_gray | Dark_gray

  (* https://misc.flogisoft.com/bash/tip_colors_and_formatting *)
  let color_string (fore_color : color_code) (back_color : color_code) : string -> string =
    let foreground =
      match fore_color with
      | Default -> 39
      | Black -> 30     | White -> 97
      | Red -> 31       | Light_red -> 91
      | Green -> 32     | Light_green -> 92
      | Yellow -> 33    | Light_yellow -> 93
      | Blue -> 34      | Light_blue -> 94
      | Magenta -> 35   | Light_magenta -> 95
      | Cyan -> 36      | Light_cyan -> 96
      | Dark_gray -> 90 | Light_gray -> 37
    in
    let background =
      match back_color with
      | Default -> 49
      | Black -> 40      | White -> 107
      | Red -> 41        | Light_red -> 101
      | Green -> 42      | Light_green -> 102
      | Yellow -> 43     | Light_yellow -> 103
      | Blue -> 44       | Light_blue -> 104
      | Magenta -> 45    | Light_magenta -> 105
      | Cyan -> 46       | Light_cyan -> 106
      | Dark_gray -> 100 | Light_gray -> 47
    in
    (* \027 in decimal instead of \033 in octal *)
    Printf.sprintf "\027[%d;%dm%s\027[0m" foreground background

end

let remove_duplicate lst =
  let rec is_member n mlst =
    match mlst with
    | [] -> false
    | h::tl ->
        begin
          if h=n then true
          else is_member n tl
        end
  in
  let rec loop lbuf =
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
  loop lst


(* replace an string in another string *)
let replace input output =
  Str.global_replace (Str.regexp_string input) output

let rec exp64 b = function
  | 0L -> 1L
  | 1L -> b
  | p ->
    let x = exp64 b (Int64.shift_right p 1) in
    Int64.mul x @@
    Int64.mul x @@
    if Int64.logand p 1L = 0L then 1L else b

let abs_path p =
  Filename.concat (Filename.dirname Sys.argv.(0)) p


let time_exec (f : unit -> 'a) : float * 'a =
  let t0 = Unix.gettimeofday () in
  let res = f () in
  let t1 = Unix.gettimeofday () in
  t1 -. t0, res

(* write the graph to a .dot file *)
let output_graph fn nodes edges  =
  let oc = open_out fn in
  output_string oc (String.concat "\n" [
    "digraph G {\n";
    (* Styles *)
    "  graph [rankdir=\"TB\", fontsize=20, label=\"Black=CFG, Red=ControlDep, Blue=DataDep\", labelloc=t]";
    "  node [shape=box, style=\"rounded,filled\", fontname=\"Courier\", margin=0.05]";
    "  edge [arrowhead=vee, arrowsize=1, fontname=\"Courier\"]";
    (* Nodes *)
    List.fold_left (fun acc node -> acc ^ "\"" ^ node ^ "\";\n") "" nodes;
    (* Edges *)
    List.fold_left (fun acc (src, dst) -> acc ^ "\"" ^ src ^ "\" -> \"" ^ dst ^ "\";\n") "" edges;
    "}\n";
  ]);
  print_endline ("Graph written to " ^ fn);
  close_out oc

let dot_escape s =
  let s' = Str.global_replace (Str.regexp_string "\n") "\\n" s in
  let s'' = Str.global_replace (Str.regexp_string "  ") " " s' in
     Str.global_replace (Str.regexp_string "\"") "\\\"" s''

let rec apply_pairs f lst =
  match lst with
  | [] -> ()
  | x::xs -> List.iter (fun y -> f x y) lst; apply_pairs f xs

let rec apply_distinct_pairs f lst =
  match lst with
  | [] -> ()
  | x::xs -> List.iter (fun y -> f x y) xs; apply_distinct_pairs f xs

let rec last_element lst =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last_element tail

  let contains_substring str s =
  try
    let regexp = Str.regexp_string s in
    Str.search_forward regexp str 0 >= 0
  with
    Not_found -> false

  
  let manual_dependency = ref false