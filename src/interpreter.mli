(* Interface for bytecode interpreter *)

type scope = (string, Py_val.t) Hashtbl.t
type env = {
  cls : Py_val.cls option;
  locals : scope list;
  globals : scope list;
}

exception Runtime_error of string

(* reads bytecode and produces desired output in console *)
val interpret : Bytecode.code -> env -> env

(* create a new environment and fill it with built-ins *)
val init_env : unit -> env
