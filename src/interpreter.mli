(* interface for bytecode interpreter *)

type scope = (string, Py_val.t) Hashtbl.t
type env = {
  local : scope list;
  global : scope list;
}

exception Runtime_error of string

(* reads bytecode and produces desired output in console *)
val interpret : Bytecode.code -> env -> unit

(* create a new environment and fill it with built-ins *)
val init_env : unit -> env
