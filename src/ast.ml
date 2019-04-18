(* Structure of the abstract syntax tree *)

type op =
  And | Or | Not  | Is | In | NotIn | IsNot
| Plus   | Minus  | Times  | FpDiv
| IntDiv | Mod    | Exp    | Eq
| Neq    | Lt     | Gt     | Leq
| Geq    | BwAnd  | BwOr   | BwComp
| BwXor  | LShift | RShift | Neg
[@@deriving show]

type expr =
  Var of string
| IntLit of int
| FloatLit of float
| BoolLit of bool
| StrLit of string
| Call of expr * expr list
(* operators are treated the same regardless of arity *)
| Op of op * expr list
| Cond of expr * expr * expr (* ternary expression *)
| None
[@@deriving show]

type stmt =
  Expr of expr
| Assign of string * expr
| If of expr * stmt list * ((stmt list) option)
| While of expr * stmt list
| Break
| Continue
[@@deriving show]

type program = stmt list
[@@deriving show]

let show prog =
  let module S = Str in
  S.global_replace (S.regexp_string "Ast.") "" (show_program prog)
