(* Lexer definition for OPythn, intended for use with ocamllex *)

{
  open Lexing
  open Parser

  exception Lex_error of string
  exception Indent_error of int * int * int

  (* stack for indentation levels *)
  let indent_levels = Stack.create ()
  let () = Stack.push 0 indent_levels
  (* queue for dedent tokens *)
  let read_queue = Queue.create ()

  (* returns number of spaces, where a tab counts as 4 spaces *)
  let count_ws (str : string) : int =
    let chars = List.init (String.length str) (String.get str) in
    (* assumes the string only has tabs and spaces *)
    let sum_over acc c = if Char.equal c '\t' then acc + 4 else acc + 1 in
    List.fold_left sum_over 0 chars

  (* returns the number of numbers in stack greater than n *)
  let pop_and_count (stack : int Stack.t) (n : int) : int =
    let rec iter acc =
      if Stack.top stack > n then begin
        (* pop the larger number off the stack and loop again *)
        let _ = Stack.pop stack in iter (acc + 1)
      end
      else if Stack.top stack = n then
             (* if number matches curr then this is the correct indentation level *)
             acc
           else raise (Indent_error ((Stack.top stack), n, acc)) in
    iter 0

  (* enqueue enough dedents to reduce indentation level to new_level, modifying
   * the indent stack accordingly
   *)
  let enqueue_dedents (queue : Parser.token Queue.t) stack new_level : unit =
    let num_dedents = pop_and_count stack new_level in
    for i = 1 to num_dedents - 1 do
      (* put all but one DEDENT onto the queue *)
      Queue.add DEDENT queue
    done


}

let integer = '-'? ['1'-'9'] ['0'-'9']* | '-'? '0'*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read_one =
  parse
    integer    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | newline    { if lexbuf.lex_start_p.pos_cnum = lexbuf.lex_start_p.pos_bol then (
                   (* a newline at the beginning of the line resets indentation *)
                   Lexing.new_line lexbuf;
                   enqueue_dedents read_queue indent_levels 0;
                   Queue.add NEWLINE read_queue;
                   DEDENT
                 )
                 else (Lexing.new_line lexbuf; NEWLINE) }
  | id         { ID (Lexing.lexeme lexbuf) }
  | whitespace { if lexbuf.lex_start_p.pos_cnum = lexbuf.lex_start_p.pos_bol then
                   (* at the start of a line *)
                   let count = count_ws (Lexing.lexeme lexbuf) in
                   let topstack = Stack.top indent_levels in
                     if count > topstack then (
                       Stack.push count indent_levels;
                       INDENT
                     )
                     else if count < topstack then (
                            enqueue_dedents read_queue indent_levels count;
                            DEDENT
                          )
                          else read_one lexbuf (* ignore whitespace otherwise *)
                 else read_one lexbuf }
  | '#'        { read_line_comment lexbuf }
  | _          { raise (Lex_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { if Stack.top indent_levels <> 0 then (
                   enqueue_dedents read_queue indent_levels 0;
                   Queue.add EOF read_queue;
                   DEDENT
                 )
                 else EOF }

and read_line_comment =
  parse
    newline { read_one lexbuf }
  | eof     { EOF }
  | _       { read_line_comment lexbuf }

{
  let read lexbuf =
    if Queue.is_empty read_queue then
      read_one lexbuf
    else Queue.take read_queue
}
