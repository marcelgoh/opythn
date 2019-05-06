(* Bytecode interpreter *)

open Printf
open Py_val
module D = DynArray
module F = Float
module H = Hashtbl
module S = Stack

exception Runtime_error of string
exception Type_error

type scope = (string, Py_val.t) H.t
type env = {
  locals : scope list;
  globals : scope list;
}

(* type conversion functions *)
let as_bool = function
  Int i  -> i <> 0
| Bool b -> b
| Float f -> f <> 0.0
| Str s -> s <> ""
| Fun f -> true
| None -> false

let as_int = function
  Int i  -> i
| Bool b -> if b then 1 else 0
| Float _ | Str _ | Fun _ | None ->
    raise Type_error

let as_float = function
  Int i   -> float_of_int i
| Float f -> f
| Bool b  -> if b then 1.0 else 0.0
| Str _ | Fun _ | None ->
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

(* python modulo operator *)
let rec int_exp x n =
  if n = 0 then 1
  else
    let x' = int_exp x (n / 2) in
    if n mod 2 = 0 then x' * x'
    else x * x' * x'

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
    Int _ | Float _ | Bool _ | Str _ | Fun _ | None ->
      raise Type_error (* not iterable *)

(* environment functions *)
let rec lookup_scope_list s_list id =
  match s_list with
    [] ->
      raise (Runtime_error (Printf.sprintf "Variable \"%s\" not found: LOAD_GLOBAL" id))
  | s::ss ->
      (match H.find_opt s id with
         Some pv -> pv
       | None -> lookup_scope_list ss id)

let lookup_global envr id = lookup_scope_list envr.globals id

(* run code on virtual stack machine *)
let rec run (c : Bytecode.code) (envr : env) : Py_val.t =
  (* stack machine *)
  let (stack : Py_val.t S.t) = S.create () in
  let s_push pv = S.push pv stack in
  let store_name scope id =
    let tos = S.pop stack in
    H.replace scope id tos
  in
  let module Loop =
    struct
      exception Exit of Py_val.t
    end
  in
  (* loop over instructions *)
  let rec loop idx =
    try (
      if idx >= D.length c then
        (* default behaviour when no RETURN_VALUE is encountered is to return TOS *)
        if S.is_empty stack then Py_val.None else S.pop stack
      else
        let next = ref (idx + 1) in
        (match D.get c idx with
           NOP -> ()
         | POP_TOP -> S.pop stack |> ignore
         | UNARY_NEG ->
             let tos = S.pop stack in
             (try if is_float tos then
                    s_push (Float (-. (as_float tos)))
                  else
                    s_push (Int (- (as_int tos)))
              with Type_error -> raise (Runtime_error "Wrong type: UNARY_NEG"))
         | UNARY_NOT ->
             s_push (Bool (not (as_bool (S.pop stack))))
         | UNARY_BW_COMP ->
             (try s_push (Int (lnot (as_int (S.pop stack))))
              with Type_error -> raise (Runtime_error "Wrong type: UNARY_BW_COMP"))
         | BINARY_ADD ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try
                (match (tos1, tos) with
                   (Str s, Str t) -> s_push (Str (s ^ t))
                 | _              -> if is_float tos1 || is_float tos then
                                       s_push (Float ((as_float tos1) +. (as_float tos)))
                                     else
                                       s_push (Int ((as_int tos1) + (as_int tos))))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_ADD"))
         | BINARY_SUB ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try if is_float tos1 || is_float tos then
                    s_push (Float ((as_float tos1) -. (as_float tos)))
                  else
                    s_push (Int ((as_int tos1) - (as_int tos)))
               with Type_error -> raise (Runtime_error "Type mismatch: BINARY_SUB"))
         | BINARY_MULT ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try if is_float tos1 || is_float tos then
                    s_push (Float ((as_float tos1) *. (as_float tos)))
                  else
                    s_push (Int ((as_int tos1) * (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_MULT"))
         | BINARY_FP_DIV ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (match F.classify_float @@ as_float tos with
                FP_zero -> raise (Runtime_error "Division by zero: BINARY_FP_DIV")
              | _       ->
                (try s_push (Float ((as_float tos1) /. (as_float tos)))
                 with Type_error -> raise (Runtime_error "Type mismatch: BINARY_FP_DIV")))
         | BINARY_INT_DIV ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (match F.classify_float @@ as_float tos with
                FP_zero -> raise (Runtime_error "Division by zero: BINARY_INT_DIV")
              | _       ->
                (try if is_float tos1 || is_float tos then
                       s_push (Float (floor ((as_float tos1) /. (as_float tos))))
                     else
                       s_push (Int (int_of_float (floor ((as_float tos1) /. (as_float tos)))))
                 with Type_error -> raise (Runtime_error "Type mismatch: BINARY_INT_DIV")))
         | BINARY_MOD ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (match F.classify_float @@ as_float tos with
                FP_zero -> raise (Runtime_error "Division by zero: BINARY_MOD")
              | _       ->
                (try if is_float tos1 || is_float tos then
                       (* OPythn modulo differs from in OCaml *)
                       let quot = (floor ((as_float tos1) /. (as_float tos))) in
                       let ans = (as_float tos1) -. quot *. (as_float tos) in
                       s_push (Float ans)
                     else
                       let quot = int_of_float (floor ((as_float tos1) /. (as_float tos))) in
                       let ans = (as_int tos1) - quot * (as_int tos) in
                       s_push (Int ans)
                 with Type_error -> raise (Runtime_error "Type mismatch: BINARY_MOD")))
         | BINARY_EXP ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try if is_float tos1 || is_float tos then
                    let ans = ((as_float tos1) ** (as_float tos)) in
                    match F.classify_float ans with
                      F.FP_nan -> raise (Runtime_error "Got a NaN: BINARY_EXP")
                    | _        -> s_push (Float ans)
                  else
                    if as_int tos < 0 then
                      if as_int tos1 < 0 then
                        s_push (Float (-.(1.0 /. float_of_int ((int_exp (as_int tos1) (as_int tos))))))
                      else
                        s_push (Float (1.0 /. float_of_int ((int_exp (as_int tos1) (as_int tos)))))
                    else
                      if as_int tos1 < 0 then
                        s_push (Int (-(int_exp (as_int tos1) (as_int tos))))
                      else
                        s_push (Int (int_exp (as_int tos1) (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_EXP"))
         | BINARY_LSHIFT ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Int ((as_int tos1) lsl (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_LSHIFT"))
         | BINARY_RSHIFT ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Int ((as_int tos1) asr (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_RSHIFT"))
         | BINARY_BW_AND ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Int ((as_int tos1) land (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_BW_AND"))
         | BINARY_BW_OR ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Int ((as_int tos1) lor (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_BW_OR"))
         | BINARY_BW_XOR ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Int ((as_int tos1) lxor (as_int tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: BINARY_BW_XOR"))
         | COMPARE_EQ ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (eq tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_EQ"))
         | COMPARE_NEQ ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (not (eq tos1 tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_NEQ"))
         | COMPARE_LT ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (lt tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_LT"))
         | COMPARE_GT ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (lt tos tos1)) (* switch order and call lt *)
              with type_error -> raise (Runtime_error "type mismatch: compare_gt"))
         | COMPARE_LEQ ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (not (lt tos tos1))) (* not gt *)
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_LEQ"))
         | COMPARE_GEQ ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (not (lt tos1 tos))) (* not lt *)
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_LEQ"))
         | COMPARE_IS ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (is tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_IS"))
         | COMPARE_IN ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (py_in tos1 tos))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_IN"))
         | COMPARE_NOT_IN ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (not (py_in tos1 tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_NOT_IN"))
         | COMPARE_IS_NOT ->
             let tos = S.pop stack in
             let tos1 = S.pop stack in
             (try s_push (Bool (not (is tos1 tos)))
              with Type_error -> raise (Runtime_error "Type mismatch: COMPARE_IS_NOT"))
         | RETURN_VALUE -> raise (Loop.Exit (S.pop stack)) (* return top of stack *)
         | STORE_LOCAL(n, id) ->
             (match List.nth_opt envr.locals n with
                None -> raise (Runtime_error "Tried to access non-existent scope: STORE_LOCAL")
              | Some scope -> store_name scope id)
         | STORE_GLOBAL id
         | STORE_NAME id -> store_name (List.hd envr.globals) id
         | LOAD_CONST pv -> s_push pv
         | LOAD_LOCAL(n, id) ->
             (match List.nth_opt envr.locals n with
                None -> raise (Runtime_error "Tried to access non-existent scope: LOAD_LOCAL")
              | Some scope ->
                  (match H.find_opt scope id with
                     None -> raise (Runtime_error "Variable \"%s\" not found: LOAD_LOCAL")
                   | Some pv -> s_push pv))
         | LOAD_GLOBAL id
         | LOAD_NAME id -> s_push @@ lookup_global envr id
         | JUMP t -> next := t
         | POP_JUMP_IF_FALSE t -> if as_bool (S.pop stack) then () else next := t
         | JUMP_IF_TRUE_OR_POP t ->
             if as_bool (S.top stack) then next := t
             else S.pop stack |> ignore
         | JUMP_IF_FALSE_OR_POP t ->
             if not (as_bool (S.top stack)) then next := t
             else S.pop stack |> ignore
         | CALL_FUNCTION argc ->
             let arglist = ref [] in
             for _ = 0 to argc - 1 do
               arglist := (S.pop stack) :: !arglist
             done;
             let retval =
               match S.pop stack with
                 Fun f -> f !arglist
               | _     -> raise (Runtime_error "Tried to apply non-function object: CALL_FUNCTION")
             in
             s_push retval
         | MAKE_FUNCTION (params, block) ->
             s_push (Fun
                       (fun args ->
                          if List.length params <> List.length args then
                            raise (Runtime_error "Wrong argument count: MAKE_FUNCTION")
                          else (
                            let new_scope = H.create 5 in
                            List.iter2 (fun param arg -> H.add new_scope param arg) params args;
                            (* run codeblock with new scope pushed onto locals list *)
                            run !block { envr with locals = new_scope :: envr.locals }
                          )))
        );
      loop !next
    )
    with Loop.Exit pv -> pv
  in
  (* start interpreting from the top of instructions *)
  loop 0

(* interpret bytecode instructions, printing result when appropriate *)
let interpret c envr =
  let ret_val : Py_val.t = run c envr in
  match ret_val with
    None -> ()
  | _ ->
    (* try to print top of stack *)
    let pv = lookup_global envr "print" in
    (match pv with
       Fun f -> f [ret_val] |> ignore
     | _     -> ())

(* create a new environment and fill it with built-ins *)
let init_env () : env =
  (* initialise scopes -- two for now *)
  let (built_in_s : scope) = Built_in.table in
  let (global_s : scope) = H.create 5 in
  { locals = [];
    globals = [global_s; built_in_s];
  }

