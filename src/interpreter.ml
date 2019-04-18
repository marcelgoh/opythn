(* bytecode interpreter *)

open Bytecode
open Printf
open Py_val
module D = DynArray
module H = Hashtbl
module S = Stack

exception Runtime_error of string

type scope = (string, Py_val.t) H.t
type env = scope list

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
           (match tos with
              Int i   -> s_push (Int (-i))
              (* cast bools to integers *)
            | Float f -> s_push (Float (-.f))
            | Bool b  -> if b then s_push (Int (-1))
                        else s_push (Int 0)
            | Str _ | Fun _ | None ->
                raise (Runtime_error "Wrong type: UNARY_NEG"))
       | UNARY_NOT ->
           let tos = S.pop stack in
           (match tos with
              Int i   -> if i = 0 then s_push (Bool true)
                         else s_push (Bool false)
            | Float f -> if f = 0.0 then s_push (Bool true)
                         else s_push (Bool false)
            | Bool b  -> if b then s_push (Bool false)
                         else s_push (Bool true)
            | Str s   -> if s = ""  then s_push (Bool true)
                         else s_push (Bool false)
            | Fun _   -> s_push (Bool false)
            | None    -> s_push (Bool true))
       | UNARY_BW_COMP
       | BINARY_ADD
       | BINARY_SUB
       | BINARY_MULT
       | BINARY_FP_DIV
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
