(* Typed Python values *)

type py_val =
  Int of int
| Bool of bool
| Str of id
| Fun of (py_val list -> py_val)
| NoneType
