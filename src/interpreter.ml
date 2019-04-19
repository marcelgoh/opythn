(* bytecode interpreter *)

open Printf
open Py_val
module D = DynArray
module H = Hashtbl
module S = Stack

exception Runtime_error of string
exception Type_error

type scope = (string, Py_val.t) H.t
type env = scope list

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
| Float f -> true
| _       -> false

(* run code on virtual stack machine *)
let run (c : Bytecode.code) (envr : env) : unit =
  (* stack machine *)
  let (stack : Py_val.t S.t) = S.create () in
  let s_push pv = S.push pv stack in
  (* loop over instructions *)
  let rec loop idx =
    if idx >= D.length c then ()
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
                  s_push (Int ((as_int tos1) - (as_int tos)))
            with Type_error -> raise (Runtime_error "Type mismatch: BINARY_MULT"))
       | BINARY_FP_DIV ->
           let tos = S.pop stack in
           let tos1 = S.pop stack in
           (try s_push (Float ((as_float tos1) /. (as_float tos)))
            with Type_error -> raise (Runtime_error "Type mismatch: BINARY_FP_DIV"))
       | BINARY_INT_DIV
       | BINARY_MOD
       | BINARY_EXP
       | BINARY_LSHIFT
       | BINARY_RSHIFT
       | BINARY_BW_AND
       | BINARY_BW_OR
       | BINARY_BW_XOR
       | COMPARE_EQ
       | COMPARE_NEQ
       | COMPARE_LT
       | COMPARE_GT
       | COMPARE_LEQ
       | COMPARE_GEQ
       | COMPARE_IS
       | COMPARE_IN
       | COMPARE_NOT_IN
       | COMPARE_IS_NOT
       | RETURN_VALUE
       | STORE_NAME _
       | LOAD_CONST _
       | LOAD_NAME _
       | JUMP _
       | POP_JUMP_IF_FALSE _
       | JUMP_IF_TRUE_OR_POP _
       | JUMP_IF_FALSE_OR_POP _
       | CALL_FUNCTION _ -> printf "Instruction not yet implemented.\n"
      );
       loop !next in
  (* start interpreting from the top of instructions *)
  loop 0

(* interpret bytecode instructions *)
let interpret (c : Bytecode.code) : unit =
  (* initialise scopes -- two for now *)
  let (built_in_s : scope) = H.create 5 in
  H.add built_in_s "print" (Fun Built_in.print);
  let (global_s : scope) = H.create 5 in
  let envr = [global_s; built_in_s] in
  run c envr
