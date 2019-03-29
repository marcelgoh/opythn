(* Lexer definition for OPythn, intended for use with ocamllex *)

{
  open Lexing
  open Parser

  exception Lex_error of string

  indent_levels = Stack.create ();
  Stack.push 0 indent_levels;
  read_queue = Queue.create ();

  (* returns number of spaces, where a tab counts as 4 spaces *)
  let count_ws str
    let chars = List.init (String.length str) (String.get str) in
    (* assumes the string only has tabs and spaces *)
    let sum_over acc c = if Char.equal c '\t' then acc + 4 else acc + 1
    List.fold_left sum_over 0 chars

  (* returns the number of numbers in stack greater than n *)
  let pop_and_count stack n =
    let rec iter acc =
      if Stack.top stack < curr then
        (* pop the larger number off the stack and loop again *)
        Stack.pop stack;
        iter (acc + 1)
      else if Stack.top stack = curr then
             (* if number matches curr then this is the correct indentation level *)
             acc
           else raise (Lex_error "Improper indentation") in
    iter 0

}

let integer = '-'? ['1'-'9'] ['0'-'9']* | '-'? '0'*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read_one =
  parse
    integer    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | newline    { Lexing.new_line lexbuf; NEWLINE }
  | whitespace { if lexbuf.lex_curr_p.pos_bol - lexbuf.lex_curr_p.pos_bol = 0 then
                   (* at the start of a line *)
                   let count = count_ws (Lexing.lexeme lexbuf) in
                   let topstack = Stack.top indent_levels in
                     if count > topstack then
                       Stack.push count;
                       INDENT
                     else if count < topstack then
                            let num_dedents = pop_and_count indent_stack count in
                            (* return one dedent and push the rest onto a queue *)
                            for i = 0 to num_dedents - 1 do
                              Queue.enqueue read_queue DEDENT
                            done
                            DEDENT
                          else read_one lexbuf
                 else read_one lexbuf

{
  let read () =
    if Queue.is_empty read_queue then
      read_one ()
    else Queue.take read_queue
}
