(* Interface for bytecode compiler *)

(* alias for a DynArray of Instr.t *)
type code = Instr.t DynArray.t

(* print entire bytecode to console as list of instructions *)
val print_asm : code -> unit

(* compile a program to bytecode *)
val compile_prog : Ast.program -> code
