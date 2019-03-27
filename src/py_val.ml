(* Representation of OPythn typed values *)

type py_val =
  INT of int
| BOOL of bool
| STR of string
| FUN of py_val list -> py_val
