(* OPythn main front-end *)

open Printf
open Lexer
open Parser

let main () =
  opy_code = Fileio.str_of_prog_args;
  match opy_code with
    Some s -> let buffer = Lexing.from_string s in
              let tokens = Lexer.read buffer in
              printf "%s" s
    | None -> printf "A file must be provided as input.\n"

let _ = main ()
