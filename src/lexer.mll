(* Lexer definition for OPythn, intended for use with ocamllex *)

{
  open Lexing
  open Parser

  exception Lex_error of string

  (* returns number of spaces, where a tab counts as 4 spaces *)
  let count_ws (str : string) : int =
    let chars = List.init (String.length str) (String.get str) in
    (* assumes the string only has tabs and spaces *)
    let sum_over acc c = if Char.equal c '\t' then acc + 4 else acc + 1 in
    List.fold_left sum_over 0 chars

  (* returns the number of numbers in stack greater than n *)
  let pop_and_count (stack : int Stack.t) (n : int) : int =
    let rec iter acc =
      if Stack.top stack < n then begin
        (* pop the larger number off the stack and loop again *)
        let _ = Stack.pop stack in iter (acc + 1)
      end
      else if Stack.top stack = n then
             (* if number matches curr then this is the correct indentation level *)
             acc
           else raise (Lex_error "Improper indentation") in
    iter 0

  (* stack for indentation levels *)
  let indent_levels = Stack.create ()
  let () = Stack.push 0 indent_levels
  (* queue for dedent tokens *)
  let read_queue = Queue.create ()

}

let integer = '-'? ['1'-'9'] ['0'-'9']* | '-'? '0'*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read_one =
  parse
    integer    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | newline    { Lexing.new_line lexbuf; NEWLINE }
  | id         { ID (Lexing.lexeme lexbuf) }
  | whitespace { if (lexbuf.lex_curr_p.pos_bol - lexbuf.lex_curr_p.pos_bol) = 0 then
                   (* at the start of a line *)
                   let count = count_ws (Lexing.lexeme lexbuf) in
                   let topstack = Stack.top indent_levels in
                     if count > topstack then (
                       Stack.push count indent_levels;
                       INDENT
                     )
                     else if count < topstack then
                            let num_dedents = pop_and_count indent_stack count in
                            (* return one dedent and push the rest onto a queue *)
                            for i = 0 to num_dedents - 1 do
                              Queue.enqueue read_queue DEDENT
                            done;
                            DEDENT
                          else read_one lexbuf (* ignore whitespace otherwise *)
                 else read_one lexbuf }
  | _          { raise (Lex_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

{
  let read lexbuf =
    if Queue.is_empty read_queue then
      read_one lexbuf
    else Queue.take read_queue
}
