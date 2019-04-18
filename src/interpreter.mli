(* interface for bytecode interpreter *)

exception Runtime_error of string

val interpret : Bytecode.code -> unit
