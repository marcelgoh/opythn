(* Interface for OPythn typed values *)

type t = INT of int
| BOOL of bool
| STR of string
| FUN of (t list -> t)
| NONETYPE
