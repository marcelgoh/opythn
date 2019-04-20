(* built-in functions *)

open Printf
open Py_val

exception Built_in_error of string

(* all of these functions are of type
 * Py_val.t list -> Py_val.t
 *)

let rec print args =
  match args with
    []      -> None
  | pv::pvs ->
      (match pv with
         Int i   -> printf "%d" i
       | Float f -> printf "%f" f
       | Bool b  -> if b then printf "True" else printf "False"
       | Str s   -> printf "%s" s
       | Fun f   -> printf "<function>"
       | None    -> printf "None");
      printf " ";
      print pvs

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

