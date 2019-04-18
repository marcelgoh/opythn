(* Typed Python values *)

type t =
  Int of int
| Float of float
| Bool of bool
| Str of string
| Fun of (t list -> t)
| None
[@@deriving show]

let str_of_py_val pv =
  Str.global_replace (Str.regexp_string "Py_val.") "" (show pv)
