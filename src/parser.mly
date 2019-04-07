(* Parser generator for OPythn, for use with menhir *)

%{
  open Ast
%}

(* basic tokens *)
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
%nonassoc MINUS
%right LSQUARE LPAREN DOT

%start <Ast.program> file_input

%% (* list of production rules *)

(* start symbols *)
file_input:
  ss = stmt*; EOF { ss }

(* STATEMENTS *)
stmt:
  s = (simple_stmt | compound_stmt) { s }
(* simple statements *)
simple_stmt:
  s = small_stmt; ss = (SEMIC small_stmt)*; SEMIC?; NEWLINE { s :: ss }
small_stmt:
  s = (expr_stmt | flow_stmt) { s }
flow_stmt:
  BREAK { Break }
| CONTINUE { Continue }

(* compound statements *)
compound_stmt:
  s = (if_stmt | while_stmt) { s }
if_stmt:
  IF; cond = (test | cond_expr); COLON;
    pos = suite;
  elifs = elif_stmt*;
  neg = (ELSE COLON suite)? {
    If(cond, (pos :: elifs), neg)
  }
elif_stmt:
  ELIF; cond = (test | cond_expr); COLON; pos = suite {
    If(cond, suite, None)
  }
while_stmt:
  WHILE; cond = (test | cond_expr); COLON; pos = suite {
    While(cond, suite)
  }
suite:
  s = simple_stmt { [s] }
| NEWLINE; INDENT; ss = stmt+; DEDENT { ss }

(* EXPRESSIONS *)
cond_expr:
  e1 = test; IF; cond = test; ELSE; e2 = cond_expr {
    Cond(cond, e1, e2)
  }
test:
  t1 = test; OR; t2 = test { Op(Or, [t1, t2]) }
| t1 = test; AND; t2 = test { Op(And, [t1, t2]) }
| NOT; t = test { Op (Not, [t]) }
| c = comparison { c }
comparison:
  e = expr { e }
| e1 = expr; op = comp_op; e2 = expr { Comp(op, [e1, e2]) }
comp_op:
  LT { Lt }   | GT { Gt } | EQ { Eq }        | LEQ { Leq } | GEQ { Geq }
| NEQ { Neq } | IN { In } | NOT_IN { NotIn } | IS { IS }   | IS_NOT { IsNot }
expr:
  a = atom_expr { a }
| e1 = expr; op = bin_op; e2 = expr { Op(op, [e1, e2]) }
| op = un_op ; e = expr { Op(op, [e]) }
bin_op:
  BW_OR { BwOr }    | BW_XOR { BwXor }   | BW_AND { BwAnd } | LSHIFT { LShift }
| RSHIFT { RShift } | PLUS { Plus }      | MINUS { Minus }  | TIMES { Times }
| FP_DIV { FpDiv }  | INT_DIV { IntDiv } | MOD { Mod }      | EXP { Exp}
un_op:
  MINUS { Neg } | BW_COMP { BwComp }

