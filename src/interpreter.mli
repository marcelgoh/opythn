(* interface for bytecode interpreter *)

exception Runtime_error of string

(* reads bytecode and produces desired output in console *)
val interpret : Bytecode.code -> unit
