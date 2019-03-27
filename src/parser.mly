(* Parser generator for OPythn, for use with menhir *)

%token <string> NAME    (* user-defined name *)
%token <string> KWORD   (* OPythn keyword *)
%token <int> INT
%token <string> OP
%token NEWLINE
%token INDENT
%token DEDENT
%token EOF

%start <unit> file_input

%%

file_input:
  EOF { () }
;
