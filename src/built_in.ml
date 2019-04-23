(* built-in functions *)

open Printf
open Py_val

exception Built_in_error of string

(* all of these functions are of type
 * Py_val.t list -> Py_val.t
 *)

let print args =
  let rec print' with_space args =
  match args with
    []      -> None
  | pv::pvs ->
      if with_space then
        printf " "
      else ();
      (match pv with
         Int i   -> printf "%d" i
       | Float f -> let str = sprintf "%f" f in
                    printf "%s" (Str.global_replace (Str.regexp "0*$") "" str)
       | Bool b  -> if b then printf "True" else printf "False"
       | Str s   -> printf "%s" s
       | Fun f   -> printf "<function>"
       | None    -> printf "None");
      print' true pvs in
  print' false args

(* print() *)
let print_ln args =
  print args |> ignore;
  printf "\n";
  None

(* input() *)
let input args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: INPUT()")
  else
    print args |> ignore;
    (Str (read_line ()))

(* int() *)
let int_cast args =
  if List.length args <> 1 then
    raise (Built_in_error "Exactly one argument expected: INT_CAST()")
  else
    match List.hd args with
      Int i   -> Int i
    | Float f -> Int (int_of_float f)
    | Str s   -> Int (int_of_string s)
    | Bool b  -> if b then Int 1 else Int 0
    | Fun _ | None ->
        raise (Built_in_error "Failed typecast: INT_CAST()")

(* round() *)
let round args =
  if List.length args <> 2 then
    raise (Built_in_error "Exactly two arguments expected: ROUND()")
  else
  let (num, digits) = (List.hd args, List.hd @@ List.tl args) in
  let n = match num with
            Int i   -> float_of_int i
          | Bool b  -> if b then 1.0 else 0.0
          | Float f -> f
          | Str _ | Fun _ | None ->
              raise (Built_in_error "Cannot round non-numeric type: ROUND()")
  in
  let d = match digits with
            Int i  -> i
          | Bool b -> if b then 1 else 0
          | Float _ | Str _ | Fun _ | None ->
              raise (Built_in_error "Precision must be integer: ROUND()")
  in
  (* round to nearest integer *)
  let round' x = floor (x +. 0.5) in
  let factor = 10.0 ** (float_of_int d) in
  Float ((round' (n *. factor)) /. (factor))
