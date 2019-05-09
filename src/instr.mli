(* Interface for instructions *)

(* pointer to block of code *)
type 'a block_ptr =
  'a DynArray.t ref
[@opaque]
[@@deriving show]

(* type for single bytecode instruction *)
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
(* return TOS to caller *)
| RETURN_VALUE
(* operations with arguments *)
| STORE_LOCAL of (* depth : *) int * (* name : *) string (* name = TOS *)
| STORE_GLOBAL of (* name : *) string
| LOAD_CONST of (* value : *) Py_val.t                   (* TOS = value *)
| LOAD_LOCAL of (* depth : *) int * (* name : *) string  (* TOS = name *)
| LOAD_GLOBAL of (* name : *) string
| JUMP of (* target : *) int
| POP_JUMP_IF_FALSE of (* target : *) int
| JUMP_IF_TRUE_OR_POP of (* target : *) int
| JUMP_IF_FALSE_OR_POP of (* target : *) int
| CALL_FUNCTION of (* argc : *) int
| MAKE_FUNCTION of (* args : *) string list * (* block : *) t block_ptr
[@@deriving show]

(* prints an array of instructions in readable format *)
val print_instr_array : t DynArray.t -> unit
