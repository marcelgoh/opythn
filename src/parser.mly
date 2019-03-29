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

