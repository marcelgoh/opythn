(* Parser generator for OPythn, for use with menhir *)

%{
  open Ast
%}

%token <string> ID
%token <int> INT
%token <string> OP
%token <string> STR
%token NEWLINE %token INDENT %token DEDENT %token EOF
(* keywords *)
%token TRUE    %token FALSE  %token NONE
%token IF      %token ELIF   %token ELSE
%token WHILE   %token BREAK  %token CONTINUE
(* word-like operators *)
%token AND %token OR %token NOT %token IS %token IN %token NOT_IN %token IS_NOT
(* symbolic operators TODO: add associativity *)
%token PLUS    %token MINUS  %token TIMES %token FP_DIV
%token INT_DIV %token MOD    %token EXP   %token EQ
%token NEQ     %token LT     %token GT    %token LEQ
%token GEQ     %token BW_AND %token BW_OR %token BW_COMP
%token BW_XOR  %token LSHIFT %token RSHIFT
(* delimiters *)
%token LPAREN   %token RPAREN   %token LSQUARE  %token RSQUARE
%token LCURLY   %token RCURLY   %token DOT      %token COMMA
%token COLON    %token SEMIC    %token ASSIG    %token PLUS_A
%token MINUS_A  %token TIMES_A  %token FP_DIV_A %token INT_DIV_A
%token MOD_A    %token EXP_A    %token BW_AND_A %token BW_OR_A
%token BW_XOR_A %token LSHIFT_A %token RSHIFT_A

(* associativity and precedence *)
%left OR
%left AND
%nonassoc NOT
%left IN NOT_IN IS IS_NOT LT LEQ GT GEQ NEQ EQ
%left BW_OR
%left BW_XOR
%left BW_AND
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES FP_DIV INT_DIV MOD
%right EXP
%right LSQUARE LPAREN DOT

%start <Ast.program> file_input

%%

file_input: EOF { [ Expr(Var "hi") ] }
(*
stmt: simple_stmt | compound_stmt

small_stmt: expr_stmt (* add more stmt types later *)
expr_stmt: testlist_star_expr


compound_stmt: if_stmt | while_stmt
*)
