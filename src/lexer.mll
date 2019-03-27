(* Lexer definition for OPythn, intended for use with ocamllex *)

{
  open Lexing

  exception Lex_error of string

  indent_levels = Stack.create ();
  read_queue = Queue.create ();

}

let integer = '-'? ['0'-'9'] ['0'-'9']*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*


