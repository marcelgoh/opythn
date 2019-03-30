(* OPythn main front-end *)

open Printf
open Lexer
open Parser

let main () =
  let opy_code = Fileio.str_of_prog_args () in
  match opy_code with
    Some s -> let buffer = Lexing.from_string s in
              let tok = ref (Lexer.read buffer) in
              while !tok <> EOF do
                printf "%s" (Token.show !tok);
                tok := (Lexer.read buffer)
              done;
              printf "%s" (Token.show !tok);
    | None -> printf "A file must be provided as input.\n"

let _ = main ()
