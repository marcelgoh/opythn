(* OPythn main front-end *)

open Printf
open Ast

let test_lexer () =
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

let test_parser () =
  let parse_exn buffer = try Parser.file_input Lexer.read buffer with
                    Parser.Error ->
                      printf "%s: syntax error.\n" (Lexer.print_position buffer);
                      [] in
  let opy_code = Fileio.str_of_prog_args () in
    match opy_code with
      Some s -> let buffer = Lexing.from_string s in
                let tree = parse_exn buffer in
                printf "%s\n" (Ast.show tree)
    | None   -> printf "A file must be provided as input.\n"

let _ = printf "************ LEXER OUTPUT ************\n";
        test_lexer();
        printf "************ PARSER OUTPUT ************\n";
        test_parser ()
