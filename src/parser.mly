(* Parser generator for OPythn, for use with menhir *)

%token <string> ID    (* identifier *)
%token <int> INT
%token <string> OP
%token <string> STR
%token NEWLINE
%token INDENT
%token DEDENT
%token EOF

%start <token> file_input

%%

file_input:
  s = ID  { ID s }
| i = INT { INT i }
| s = OP  { OP s }
| s = STR { STR s }
| NEWLINE { NEWLINE }
| INDENT  { INDENT }
| DEDENT  { DEDENT }
| EOF { EOF }
;

(*
{
  let str_of_token tok =
    match tok with
      ID s      -> "ID " ^ s
    | INT i     -> "INT " ^ (int_of_string i)
    | OP s      -> "OP " ^ s
    | STR s     -> "STR " ^ s
    | NEWLINE   -> "NEWLINE"
    | INDENT    -> "INDENT"
    | DEDENT    -> "DEDENT"
    | EOF       -> "EOF"
}
*)
