(* Typed Python values *)

type t =
  Int of int
| Bool of bool
| Str of string
| Fun of (t list -> t)
| NoneType
