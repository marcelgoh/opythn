(* Typed Python values *)

exception Type_error

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

(* type conversion functions *)
let as_bool = function
  Int i  -> i <> 0
| Bool b -> b
| Float f -> f <> 0.0
| Str s -> s <> ""
| Fun (_, _) -> true
| Obj _ | Class _ | Type _
| None -> false

let as_int = function
  Int i  -> i
| Bool b -> if b then 1 else 0
| Float _ | Str _ | Fun _
| Obj _ | Class _ | Type _ | None ->
    raise Type_error

let as_float = function
  Int i   -> float_of_int i
| Float f -> f
| Bool b  -> if b then 1.0 else 0.0
| Str _ | Fun _ | Obj _ | Class _ | Type _ | None ->
    raise Type_error

let is_float = function
  Float _ -> true
| _       -> false

let is_int = function
  Int _ -> true
| _     -> false

let is_str = function
  Str _ -> true
| _     -> false

(* comparison functions -- TODO: make these better/work with objects *)
let eq pv1 pv2 = (* uses OCaml's structural equality *)
  if is_float pv1 || is_float pv2 then
    as_float pv1 = as_float pv2
  else if is_int pv1 || is_int pv2 then
         as_int pv1 = as_int pv2
       else
         match (pv1, pv2) with
           (Str s1, Str s2) -> s1 = s2
         | (None, None) -> true
         | _ -> raise Type_error

let is pv1 pv2 = (* uses OCaml's physical equality *)
  if is_float pv1 || is_float pv2 then
    as_float pv1 == as_float pv2
  else if is_int pv1 || is_int pv2 then
         as_int pv1 == as_int pv2
       else
         match (pv1, pv2) with
           (Str s1, Str s2) -> s1 == s2
         | (None, None) -> true
         | _ -> raise Type_error

let lt pv1 pv2 = (* uses OCaml's structural comparison *)
  if is_float pv1 || is_float pv2 then
    as_float pv1 < as_float pv2
  else if is_int pv1 || is_int pv2 then
         as_int pv1 < as_int pv2
       else
         match (pv1, pv2) with
           (Str s1, Str s2) -> s1 < s2
         | (None, None) -> true
         | _ -> raise Type_error

let py_in pv1 pv2 =
  match pv2 with
    Int _ | Float _ | Bool _ | Str _ | Fun _
  | Obj _ | Class _ | Type _ | None ->
      raise Type_error (* not iterable *)

