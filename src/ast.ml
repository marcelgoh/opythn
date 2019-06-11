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
| Lambda of string list * expr    (* anonymous function *)
| AttrRef of expr * string
| Subscr of expr * expr * expr option
| ListLit of expr list
| DictLit of (expr * expr) list
| TupleLit of expr list
| None
[@@deriving show]

type stmt =
  Expr of expr
| Assign of expr * expr
| If of expr * stmt list * ((stmt list) option)
| While of expr * stmt list
| For of expr * expr * stmt list
| Funcdef of string * string list * stmt list
| Global of string     (* one declaration at a time *)
| Nonlocal of string
| Return of expr       (* return only one expression *)
| Classdef of string * string option * stmt list
| Pass
| Break
| Continue
[@@deriving show]

type program = stmt list
[@@deriving show]

let show prog =
  let module S = Str in
  S.global_replace (S.regexp_string "Ast.") "" (show_program prog)
