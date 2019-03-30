(* Parser generator for OPythn, for use with menhir *)

%token <string> ID
%token <int> INT
%token <string> OP
%token <string> STR
%token NEWLINE %token INDENT %token DEDENT %token EOF
%token TRUE  %token FALSE %token NONE
%token IF    %token ELIF  %token ELSE
%token WHILE %token BREAK %token CONTINUE
%token AND %token OR %token NOT %token IS %token IN
%token PLUS    %token MINUS  %token TIMES %token FP_DIV
%token INT_DIV %token MOD    %token EXP   %token EQ
%token NEQ     %token LT     %token GT    %token LEQ
%token GEQ     %token BW_AND %token BW_OR %token BW_COMP
%token BW_XOR  %token LSHIFT %token RSHIFT
%token LPAREN   %token RPAREN   %token LSQUARE  %token RSQUARE
%token LCURLY   %token RCURLY   %token DOT      %token COMMA
%token COLON    %token SEMIC    %token ASSIG    %token PLUS_A
%token MINUS_A  %token TIMES_A  %token FP_DIV_A %token INT_DIV_A
%token MOD_A    %token EXP_A    %token BW_AND_A %token BW_OR_A
%token BW_XOR_A %token LSHIFT_A %token RSHIFT_A

%start <token> file_input

%%

file_input:
  s = STR { STR s }
