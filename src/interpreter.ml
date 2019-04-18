(* bytecode interpreter *)

open Ast
open Bytecode
open Printf
open Py_val
module D = DynArray
module H = Hashtbl

exception Interpreter_error of string

type scope = (string, Py_val.t) H.t
type env = scope list

(* run code on virtual stack machine *)
let run (c : Bytecode.code) (envr : env) : unit =
  printf "IRL I'd start to run code.\n"

(* interpret bytecode instructions *)
let interpret (c : Bytecode.code) : unit =
  (* initialise scopes -- two for now *)
  let (built_in_s : scope) = H.create 5 in
  H.add built_in_s "print" (Fun Built_in.print);
  let (global_s : scope) = H.create 5 in
  let envr = [global_s; built_in_s] in
  run c envr
