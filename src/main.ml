(* OPythn main front-end *)

open Printf
open Lexer
open Parser

let str_of_token tok =
  match tok with
    ID s      -> "(ID " ^ s ^ ") "
  | INT i     -> "(INT " ^ (string_of_int i) ^ ") "
  | OP s      -> "(OP " ^ s ^ ") "
  | STR s     -> "(STR " ^ s ^ ") "
  | NEWLINE   -> "(NEWLINE)\n"
  | INDENT    -> "(INDENT) "
  | DEDENT    -> "(DEDENT) "
  | EOF       -> "(EOF) "

let is_eof tok =
  match tok with
    EOF -> true
  | _   -> false

let main () =
  let opy_code = Fileio.str_of_prog_args () in
  match opy_code with
    Some s -> let buffer = Lexing.from_string s in
              let tok = ref (Lexer.read buffer) in
              while not (is_eof !tok) do
               (* printf "%s" (str_of_token !tok); *)
                tok := (Lexer.read buffer)
              done
    | None -> printf "A file must be provided as input.\n"

let _ = main ()
