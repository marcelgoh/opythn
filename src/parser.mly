(* Parser generator for OPythn, for use with menhir *)

%token <string> ID    (* identifier *)
%token <int> INT
%token <string> OP
%token <string> STR
%token NEWLINE
%token INDENT
%token DEDENT
%token EOF

%start <Py_val.t list> file_input

%%

file_input:
  EOF { [NONETYPE] }
;
