(* Instruction datatype *)

open Printf

module D = DynArray

(* pointer to block of code *)
type 'a block_ptr =
  'a D.t ref
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
| STORE_NAME of (* name : *) string                      (* name = TOS *)
| STORE_LOCAL of (* depth : *) int * (* name : *) string
| STORE_GLOBAL of (* name : *) string
| LOAD_CONST of (* value : *) Py_val.t                   (* TOS = value *)
| LOAD_NAME of (* name : *) string                       (* TOS = name *)
| LOAD_LOCAL of (* depth : *) int * (* name : *) string
| LOAD_GLOBAL of (* name : *) string
| JUMP of (* target : *) int
| POP_JUMP_IF_FALSE of (* target : *) int
| JUMP_IF_TRUE_OR_POP of (* target : *) int
| JUMP_IF_FALSE_OR_POP of (* target : *) int
| CALL_FUNCTION of (* argc : *) int
| MAKE_FUNCTION of (* args : *) string list * (* block : *) t block_ptr
[@@deriving show]

let address_of_ptr ptr = 2*(Obj.magic ptr)

(* pretty printable representation of instruction *)
let str_of_instr instr =
  let module S = Str in
  let pretty_list l =
    let rec commacat acc strs =
      match strs with
        [] -> acc
      | s::[] -> s
      | s::ss -> commacat (s ^ "," ^ acc) ss
    in
    "(" ^ (commacat "" l) ^ ")"
  in
  let str =
    match instr with
      (* instructions with arguments *)
      STORE_NAME s           -> sprintf "STORE_NAME\t\t\"%s\"" s
    | STORE_LOCAL(i, s)      -> sprintf "STORE_LOCAL\t\t%d \"%s\"" i s
    | STORE_GLOBAL s         -> sprintf "STORE_GLOBAL\t\t\"%s\"" s
    | LOAD_CONST pv          -> sprintf "LOAD_CONST\t\t%s" (Py_val.str_of_py_val pv)
    | LOAD_NAME s            -> sprintf "LOAD_NAME\t\t\"%s\"" s
    | LOAD_LOCAL(i, s)       -> sprintf "LOAD_LOCAL\t\t%d \"%s\"" i s
    | LOAD_GLOBAL s          -> sprintf "LOAD_GLOBAL\t\t\"%s\"" s
    | JUMP i                 -> sprintf "JUMP\t\t\t%d" i
    | POP_JUMP_IF_FALSE i    -> sprintf "POP_JUMP_IF_FALSE\t%d" i
    | JUMP_IF_TRUE_OR_POP i  -> sprintf "JUMP_IF_TRUE_OR_POP\t%d" i
    | JUMP_IF_FALSE_OR_POP i -> sprintf "JUMP_IF_FALSE_OR_POP\t%d" i
    | CALL_FUNCTION i        -> sprintf "CALL_FUNCTION\t\t%d" i
    | MAKE_FUNCTION(a, ptr)  -> (sprintf "MAKE_FUNCTION\t\t%s " (pretty_list a)) ^
                                (sprintf "<func block: %x>" (address_of_ptr ptr))
    | _                      -> show instr
  in S.global_replace (S.regexp_string "Instr.") "" str

(* prints an array of instructions in readable format *)
let rec print_instr_array instrs =
  let functions = D.create () in
  for i = 0 to D.length instrs - 1 do
    let curr = D.get instrs i in
    (* when functions are encountered, add them to queue to print later *)
    (match curr with
      MAKE_FUNCTION (_,b) -> D.add functions b
    | _ -> ());
    printf "%d\t%s\n" i (str_of_instr curr)
  done;
  for i = 0 to D.length functions - 1 do
    let ptr = D.get functions i in
    printf "\n<func block: %x>:\n" (address_of_ptr ptr);
    print_instr_array !ptr
  done


