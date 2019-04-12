(* Bytecode representation and compilation *)

open Printf
open Ast
open Py_val
module D = DynArray
module H = Hashtbl

exception Bytecode_error of string

type t =
(* stack operations *)
  NOP | POP_TOP
(* unary operations: TOS = <op> TOS *)
| UNARY_NEG | UNARY_NOT | UNARY_BW_COMP
(* binary operations: TOS = TOS1 <op> TOS *)
| BINARY_ADD     | BINARY_SUB     | BINARY_MULT
| BINARY_FP_DIV  | BINARY_INT_DIV | BINARY_MOD
| BINARY_EXP     | BINARY_LSHIFT  | BINARY_RSHIFT
| BINARY_BW_AND  | BINARY_BW_OR   | BINARY_BW_XOR
(* binary comparison: TOS = TOS1 <comp> TOS *)
| COMPARE_EQ     | COMPARE_NEQ    | COMPARE_LT
| COMPARE_GT     | COMPARE_LEQ    | COMPARE_GEQ
| COMPARE_IS     | COMPARE_IN     | COMPARE_NOT_IN
| COMPARE_IS_NOT
(* operations with arguments *)
| STORE_NAME of (* name : *) string            (* name = TOS *)
| LOAD_CONST of (* value: *) Py_val.t          (* TOS = value *)
| LOAD_NAME of (* name : *) string             (* TOS = name *)
| JUMP of (* target : *) int
| POP_JUMP_IF_FALSE of (* target : *) int
| JUMP_IF_TRUE_OR_POP of (* target : *) int
| JUMP_IF_FALSE_OR_POP of (* target : *) int
| CALL_FUNCTION of (* argc : *) int
[@@deriving show]

(* pretty printable representation of instruction *)
let str_of_instr instr =
  let module S = Str in
  let str = 
    match instr with
      (* instructions with arguments *)
      STORE_NAME s           -> sprintf "STORE_NAME\t\t\"%s\"" s
    | LOAD_CONST pv          -> sprintf "LOAD_CONST\t\t%s" (str_of_py_val pv)
    | LOAD_NAME s            -> sprintf "LOAD_NAME\t\t\"%s\"" s
    | POP_JUMP_IF_FALSE i    -> sprintf "POP_JUMP_IF_FALSE\t%d" i
    | JUMP_IF_TRUE_OR_POP i  -> sprintf "JUMP_IF_TRUE_OR_POP\t%d" i
    | JUMP_IF_FALSE_OR_POP i -> sprintf "JUMP_IF_FALSE_OR_POP\t%d" i
    | CALL_FUNCTION i        -> sprintf "CALL_FUNCTION\t\t%d" i
    | _                      -> show instr
  in S.global_replace (S.regexp_string "Bytecode.") "" str

(* prints a list of instructions in readable format *)
let print_asm instrs =
  for i = 0 to D.length instrs - 1 do
    printf "%d\t%s\n" i (str_of_instr (D.get instrs i))
  done

(* convert an expression to bytecode and add instructions to array *)
let rec compile_expr (arr : t D.t) (e : Ast.expr) : unit =
  match e with
    Var id         -> D.add arr (LOAD_NAME id)
  | IntLit i       -> D.add arr (LOAD_CONST (Int i))
  | BoolLit b      -> D.add arr (LOAD_CONST (Bool b))
  | StrLit s       -> D.add arr (LOAD_CONST (Str s))
  | Call (f, args) -> compile_expr arr f;
                      List.iter (compile_expr arr) args;
                      D.add arr (CALL_FUNCTION (List.length args))
  | Op (o, args)   -> List.iter (compile_expr arr) args;
                      (match o with
                         And       (* TODO: add AND and OR *)
                       | Or     -> D.add arr NOP
                       | Not    -> D.add arr UNARY_NOT
                       | Is     -> D.add arr COMPARE_IS
                       | In     -> D.add arr COMPARE_IN
                       | NotIn  -> D.add arr COMPARE_IN
                       | IsNot  -> D.add arr COMPARE_IS_NOT
                       | Plus   -> D.add arr BINARY_ADD
                       | Minus  -> D.add arr BINARY_SUB
                       | Times  -> D.add arr BINARY_MULT
                       | FpDiv  -> D.add arr BINARY_FP_DIV
                       | IntDiv -> D.add arr BINARY_INT_DIV
                       | Mod    -> D.add arr BINARY_MOD
                       | Exp    -> D.add arr BINARY_EXP
                       | Eq     -> D.add arr COMPARE_EQ
                       | Neq    -> D.add arr COMPARE_NEQ
                       | Lt     -> D.add arr COMPARE_LT
                       | Gt     -> D.add arr COMPARE_GT
                       | Leq    -> D.add arr COMPARE_LEQ
                       | Geq    -> D.add arr COMPARE_GEQ
                       | BwAnd  -> D.add arr BINARY_BW_AND
                       | BwOr   -> D.add arr BINARY_BW_OR
                       | BwComp -> D.add arr UNARY_BW_COMP
                       | BwXor  -> D.add arr BINARY_BW_XOR
                       | LShift -> D.add arr BINARY_LSHIFT
                       | RShift -> D.add arr BINARY_RSHIFT
                       | Neg    -> D.add arr UNARY_NEG)
  | Cond (t,c,e)   -> D.add arr NOP (* TODO: add conditional exprs *)
  | None           -> D.add arr (LOAD_CONST NoneType)

(* convert a statement to bytecode and append instructions to array *)
let compile_stmt (arr : t D.t) (s : Ast.stmt) : unit =
  match s with
    Expr e        -> compile_expr arr e
  | Assign (s, e) -> compile_expr arr e;
                     D.add arr (STORE_NAME s)
  | _      -> D.add arr NOP (* TODO: add other statements *)

(* compile_prog p : Ast.program -> D.t *)
let compile_prog (p : Ast.program) : t D.t =
  let rec iter arr stmts =
    match stmts with
      []    -> ()
    | s::ss -> compile_stmt arr s;
               iter arr ss in
  let instrs = D.create () in
  iter instrs p; instrs
