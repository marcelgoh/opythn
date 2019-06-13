(* Typed Python values *)

exception Type_error

module D = DynArray
module H = Hashtbl
module S = Seq

type cls = {
  name : string;
  super : cls option;
  attrs : (string, t) H.t;
  [@opaque]
}
[@@deriving show {with_path = false}]
(* making obj its own type to appease ppx_deriving *)
and obj = {
  cls : cls;
  fields : (string, t) H.t;
  [@opaque]
}
[@@deriving show {with_path = false}]
and t =
  Int of int
| Float of float
| Bool of bool
| Str of string
| Fun of string * (t list -> t)  (* functions know their name *)
| Obj of obj
| Class of cls
| Type of string
| List of (t D.t [@opaque])
| Tuple of t array
| Dict of ((t, t) H.t [@opaque])
| Seq of (t S.t [@opaque])
| None
[@@deriving show {with_path = false}]

let str_of_py_val pv = show pv

(* get an attribute from a class *)
let rec get_attr_opt cls id =
  match H.find_opt cls.attrs id with
    Some pval -> Some pval
  | None ->
      (match cls.super with
         (* check superclass or fail if no superclass *)
         Some c -> get_attr_opt c id
       | None -> None)

(* get an object's field *)
let get_field_opt obj id =
  match H.find_opt obj.fields id with
    Some pval -> Some pval
  | None -> get_attr_opt obj.cls id

(* type conversion functions *)
let as_bool = function
  Int i  -> i <> 0
| Bool b -> b
| Float f -> f <> 0.0
| Type s
| Str s -> s <> ""
| Fun (_, _) -> true
| List darr -> D.length darr <> 0
| Tuple arr -> Array.length arr <> 0
| Dict tbl -> H.length tbl <> 0
(* interpret empty sequences as false *)
| Seq seq ->
   (match seq () with
       Seq.Nil -> false
     | Seq.Cons _ -> true)
| Obj _ | Class _ -> true
| None -> false

let as_int = function
  Int i  -> i
| Bool b -> if b then 1 else 0
| Float f -> int_of_float f
| Str s -> int_of_string s
| Fun _ | List _ | Tuple _ | Dict _ | Seq _
| Obj _ | Class _ | Type _ | None ->
    raise Type_error

let as_float = function
  Int i   -> float_of_int i
| Float f -> f
| Bool b  -> if b then 1.0 else 0.0
| Str s -> float_of_string s
| List _ | Tuple _ | Dict _ | Seq _
| Fun _ | Obj _ | Class _ | Type _ | None ->
    raise Type_error

let as_seq = function
  List darr -> Array.to_seq (D.to_array darr)
| Tuple arr -> Array.to_seq arr
| Dict htbl -> H.to_seq_keys htbl
| Str s ->
    let char_seq = String.to_seq s in
    Seq.map (fun c -> (Str (String.make 1 c))) char_seq
| Seq s -> s
| _ -> raise Type_error

let is_float = function
  Float _ -> true
| _       -> false

let is_int = function
  Int _ -> true
| _     -> false

let is_str = function
  Str _ -> true
| _     -> false

(* comparison functions *)
let rec eq pv1 pv2 = (* uses OCaml's structural equality, usually *)
  if is_float pv1 || is_float pv2 then
    as_float pv1 = as_float pv2
  else if is_int pv1 || is_int pv2 then
         as_int pv1 = as_int pv2
       else
         match (pv1, pv2) with
           (Bool x1, Bool x2) -> x1 = x2
         | (Str x1, Str x2) -> x1 = x2
         | (Type x1, Type x2) -> x1 = x2
         | (List x1, List x2) -> x1 = x2
         | (Tuple x1, Tuple x2) -> x1 = x2
         | (Fun (s1, f1), Fun (s2, f2)) -> s1 = s2 && f1 == f2
         (* change this when __eq()__ added *)
         | (Obj x1, Obj x2) ->
             (match H.find_opt x1.cls.attrs "__eq__" with
                Some (Fun (_, f)) ->
                  as_bool (f [Obj x1; Obj x2])
              | _ -> false)
         | (Class x1, Class x2) -> x1 == x2
         (* might be sketchy, not quite what Python does *)
         | (Seq x1, Seq x2) -> x1 == x2
         | (None, None) -> true
         | (Dict (x1 : (t, t) H.t), Dict (x2 : (t, t) H.t)) ->
             (* check if all values in first list map to same values in second *)
             let f (key : t) (value : t) (b : bool) : bool =
               if b then (
                 match H.find_opt x2 key with
                   Some v -> eq v value
                 | None -> false
               )
               else false
             in
             (* also make sure length of maps same *)
             H.length x1 = H.length x2 && H.fold f x1 true
         | _ -> false

let is pv1 pv2 = (* uses OCaml's physical equality *)
  if is_float pv1 || is_float pv2 then
    as_float pv1 == as_float pv2
  else if is_int pv1 || is_int pv2 then
         as_int pv1 == as_int pv2
       else
         match (pv1, pv2) with
           (Bool x1, Bool x2) -> x1 == x2
         | (Str x1, Str x2)
         | (Type x1, Type x2) -> x1 == x2
         | (List x1, List x2) -> x1 == x2
         | (Tuple x1, Tuple x2) -> x1 == x2
         | (Fun (s1, f1), Fun (s2, f2)) -> s1 = s2 && f1 == f2
         | (Obj x1, Obj x2) -> x1 == x2
         | (Class x1, Class x2) -> x1 == x2
         | (Seq x1, Seq x2) -> x1 == x2
         | (Dict x1, Dict x2) -> x1 == x2
         | (None, None) -> true
         | _ -> false

let lt pv1 pv2 = (* uses OCaml's structural comparison *)
  if is_float pv1 || is_float pv2 then
    as_float pv1 < as_float pv2
  else if is_int pv1 || is_int pv2 then
         as_int pv1 < as_int pv2
       else
         match (pv1, pv2) with
           (Bool b1, Bool b2) -> b1 < b2
         | (Str s1, Str s2) -> s1 < s2
         | (None, None) -> true
         (* do not compare lists, tuples, etc. using < *)
         | _ -> raise Type_error

(* check if pv2 is in pv2 *)
let py_in pv1 pv2 =
  let module Loop =
    struct
      exception Exit
    end
  in
  (* true if s1 is in s2 *)
  let str_in s1 s2 =
    try (
      Str.search_forward (Str.regexp_string s2) s1 0 |> ignore;
      true
    ) with Not_found -> false
  in
  (* filter the Seq.t object to match pv *)
  let in_seq pv seq =
    match (Seq.filter (fun pv' -> eq pv pv') seq) () with
      Nil -> false
    | Cons(_, _) -> true
  in
  match (pv1, pv2) with
    (Str s1, Str s2) -> str_in s1 s2
  | (_, List darr2) ->
      (try
         for i = 0 to D.length darr2 do
           if eq pv1 (D.get darr2 i) then
             raise Loop.Exit
           else ()
         done;
         false
       with Loop.Exit -> true)
  | (_, Tuple arr2) -> Array.exists (eq pv1) arr2
  | (_, Seq seq) -> in_seq pv1 seq
  | (_, Dict htbl) -> in_seq pv2 (H.to_seq_keys htbl)
  | _ ->
      raise Type_error (* not iterable *)

