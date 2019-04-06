(* Structure of the abstract syntax tree *)

type op =
  And | Or | Not  | Is | In | NotIn | IsNot
| Plus   | Minus  | Times | FpDiv
| IntDiv | Mod    | Exp   | Eq
| Neq    | Lt     | Gt    | Leq
| Geq    | BwAnd  | BwOr  | BwComp
| BwXor  | LShift | RShift
[@@deriving show]

type expr =
  Var of string
| IntLit of int
| BoolLit of bool
| StrLit of string
| Call of expr * (expr list)
(* operators are treated the same regardless of arity *)
| Op of op * (expr list)
[@@deriving show]

type stmt =
  Expr of expr
| Assign of string * expr
| If of expr * stmt * (stmt option)
| While of expr * stmt
[@@deriving show]

type program = stmt list
