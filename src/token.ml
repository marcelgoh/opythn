(* Tokens that are passed from lexer to parser *)
  (* this type has to be named "token", unfortunately *)
type token =
  ID of string | INT of int | OP of string | STR of string
| NEWLINE | INDENT | DEDENT | EOF
(* keywords *)
| TRUE  | FALSE | NONE
| IF    | ELIF  | ELSE
| WHILE | BREAK | CONTINUE
(* word-like operators *)
| AND | OR | NOT | IS | IN
(* symbolic operators *)
| PLUS    | MINUS  | TIMES | FP_DIV
| INT_DIV | MOD    | EXP   | EQ
| NEQ     | LT     | GT    | LEQ
| GEQ     | BW_AND | BW_OR | BW_COMP
| BW_XOR  | LSHIFT | RSHIFT
(* delimiters *)
| LPAREN   | RPAREN   | LSQUARE  | RSQUARE
| LCURLY   | RCURLY   | DOT      | COMMA
| COLON    | SEMIC    | ASSIG    | PLUS_A
| MINUS_A  | TIMES_A  | FP_DIV_A | INT_DIV_A
| MOD_A    | EXP_A    | BW_AND_A | BW_OR_A
| BW_XOR_A | LSHIFT_A | RSHIFT_A
[@@deriving show]

let show tok =
  let module S = Str in
  let str =
    match tok with
      NEWLINE | EOF -> (show_token tok) ^ "\n"
    | _             -> (show_token tok) ^ " " in
  S.global_replace (S.regexp_string "Token.") "" str
