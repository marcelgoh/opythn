(* Lexer definition for OPythn, intended for use with ocamllex *)

{
  open Lexing
  open Token

  exception Lex_error of string
  exception Indent_error of int * int * int

  (* print position of buffer *)
  let print_position buffer =
    let pos = buffer.lex_curr_p in
    Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  (* number of brackets *)
  let num_brackets = ref 0
  (* stack for indentation levels *)
  let indent_levels = Stack.create ()
  let () = Stack.push 0 indent_levels
  (* queue for dedent tokens *)
  let read_queue = Queue.create ()
  (* hashtable of keywords and word-like operators*)
  let keyword_table = Hashtbl.create 30
  let _ =
    List.iter (fun (kword, tok) ->
                 Hashtbl.add keyword_table kword tok)
              [ "True", TRUE;   "False", FALSE; "None", NONE;
                "and", AND;     "or", OR;       "not", NOT;
                "is", IS;       "in", IN;
                "if", IF;       "elif", ELIF;   "else", ELSE;
                "while", WHILE; "break", BREAK; "continue", CONTINUE ]

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

  (* converts all escape sequences in string to their OCaml version *)
  let unescape (str : string) : string =
    let buf = Buffer.create 10 in
    let i = ref 0 in
    while !i < String.length str do
      if str.[!i] = '\\' then (
          incr i;
          if !i < String.length str then
            let c = match str.[!i] with
                      '\\' -> '\\'
                    | '"'  -> '"'
                    | '\'' -> '\''
                    | 'b'  -> '\b'
                    | 'n'  -> '\n'
                    | 'r'  -> '\r'
                    | 't'  -> '\t'
                    | _    -> raise (Lex_error ("Undefined escape sequence: \\" ^
                                                (String.make 1 str.[!i]))) in
            Buffer.add_char buf c
      )
      else Buffer.add_char buf str.[!i];
      incr i;
    done;
    Buffer.contents buf
}

let integer = '-'? ['1'-'9'] ['0'-'9']* | '-'? '0'*
let whitespace = [' ' '\t']+
let newline = ['\r' '\n']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let string_char = [^ '\n' '\'' '"' '\\'] | '\\' _

(* consumes a token of the input buffer *)
rule read_one =
  parse
  (* symbolic operators *)
    '+'  { PLUS }    | '-'  { MINUS }  | '*'  { TIMES }  | '/'  { FP_DIV }
  | "//" { INT_DIV } | '%'  { MOD }    | "**" { EXP }    | "==" { EQ }
  | "!=" { NEQ }     | '<'  { LT }     | '>'  { GT }     | "<=" { LEQ }
  | ">=" { GEQ }     | '&'  { BW_AND } | '|'  { BW_OR }  | '~'  { BW_COMP }
  | '^'  { BW_XOR }  | "<<" { LSHIFT } | ">>" { RSHIFT }
  (* delimiters *)
  | '('  { incr num_brackets; LPAREN }   (* ignore newlines and leading whitespace *)
  | '['  { incr num_brackets; LSQUARE }  (* if in bracketed expression *)
  | '{'  { incr num_brackets; LCURLY }
  | ')'  { decr num_brackets; RPAREN }
  | ']'  { decr num_brackets; RSQUARE }
  | '}'  { decr num_brackets; RCURLY }
  | '.'  { DOT }      | ','  { COMMA } | ':'  { COLON }    | ';'   { SEMIC }
  | '='  { ASSIG }    | "+=" { PLUS_A }| "-=" { MINUS_A }  | "*="  { TIMES_A }
  | "/=" { FP_DIV_A } | "//=" { INT_DIV_A } | "%=" { MOD_A }    | "**=" { EXP_A }
  | "&=" { BW_AND_A } | "|=" { BW_OR_A } | "^=" { BW_XOR_A } | "<<=" { LSHIFT_A }
  | ">>" { RSHIFT_A }
  | "not in" { NOT_IN } | "is not" { IS_NOT } (* two-word operators *)
  | integer    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | newline    { Lexing.new_line lexbuf;
                 if !num_brackets = 0 then
                   NEWLINE
                 else read_one lexbuf }
  | id         { let ret_token =
                   let word = Lexing.lexeme lexbuf in
                   (* check for keywords *)
                   try Hashtbl.find keyword_table word
                   with Not_found -> ID word in
                 if (lexbuf.lex_start_p.pos_cnum = lexbuf.lex_start_p.pos_bol &&
                     Stack.top indent_levels <> 0) then (
                   enqueue_dedents read_queue indent_levels 0;
                   Queue.add ret_token read_queue;
                   DEDENT
                 )
                 else ret_token }
  | whitespace { if lexbuf.lex_start_p.pos_cnum = lexbuf.lex_start_p.pos_bol &&
                    !num_brackets = 0 then
                   (* at tke start of a line, not in bracketed expression *)
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
                          else read_one lexbuf (* ignore when indentation equal *)
                 else read_one lexbuf } (* ignore when not at start of line *)
  | '#'        { read_line_comment lexbuf }
  (* strings *)
  | '"' ((string_char | '\'')* as s) '"'
               { STR (unescape s) }
  | '\'' ((string_char | '"')* as s) '\''
               { STR (unescape s) }
  | _          { raise (Lex_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | ";;"       { Queue.add END_REPL read_queue; NEWLINE }
  | eof        { if Stack.top indent_levels <> 0 then (
                   enqueue_dedents read_queue indent_levels 0;
                   Queue.add EOF read_queue;
                   DEDENT
                 )
                 else EOF }

(* handle line comments by ignoring everything until a newline *)
and read_line_comment =
  parse
    newline { read_one lexbuf }
  | eof     { EOF }
  | _       { read_line_comment lexbuf }

{
  let read lexbuf =
    (* if the read queue has tokens in it, no need to consume new token yet *)
    if Queue.is_empty read_queue then
      read_one lexbuf
    else Queue.take read_queue
  let setup_file_input () =
    Queue.add START_FILE read_queue
  let setup_repl_input () =
    Queue.add START_REPL read_queue
}
