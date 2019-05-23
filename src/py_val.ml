(* Typed Python values *)

type cls = {
  name : string;
  super : cls option;
  attrs : (string, t) Hashtbl.t;
  [@opaque]
}
[@@deriving show]
(* making obj its own type to appease ppx_deriving *)
and obj = {
  cls : cls;
  fields : (string, t) Hashtbl.t;
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

(* get an attribute from a class *)
let rec get_attr_opt cls id =
  match Hashtbl.find_opt cls.attrs id with
    Some pval -> Some pval
  | None ->
      (match cls.super with
         (* check superclass or fail if no superclass *)
         Some c -> get_attr_opt c id
       | None -> None)

(* get an object's field *)
let get_field_opt obj id =
  match Hashtbl.find_opt obj.fields id with
    Some pval -> Some pval
  | None -> get_attr_opt obj.cls id

