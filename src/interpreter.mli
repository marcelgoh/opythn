(* interface for bytecode interpreter *)

open Py_val
module D = DynArray
module H = Hashtbl

exception Interpreter_error of string

type scope
type env

val interpret : Bytecode.code -> unit
