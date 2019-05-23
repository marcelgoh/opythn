(* Typed Python values *)

type cls = {
  name : string;
  super : cls option;
  dict : (string, t) Hashtbl.t;
  [@opaque]
}
[@@deriving show]
(* making obj its own type to appease ppx_deriving *)
and obj = {
  cls : cls;
  dict : (string, t) Hashtbl.t;
  [@opaque]
}
[@@deriving show]
and t =
  Int of int
| Float of float
| Bool of bool
| Str of string
| Fun of string * (t list -> t)  (* functions know their name *)
| Obj of obj
| Class of cls
| Type of string
| None
[@@deriving show]

let str_of_py_val pv =
  Str.global_replace (Str.regexp_string "Py_val.") "" (show pv)
