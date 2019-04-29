(* Parser generator for OPythn, for use with menhir *)

%{
  exception Parse_error of string

  (* build right-leaning tree of IFs from ELIFs *)
  let rec build_if_tree (cond : Ast.expr) (pos : Ast.stmt list) (elifs : Ast.stmt list)
                        (neg : (Ast.stmt list) option) : Ast.stmt =
    match elifs with
      []                  -> If(cond, pos, neg)
    | (If(c, p, _)::rest) -> If(cond, pos, Some([(build_if_tree c p rest neg)]))
    | (_::rest)           -> raise (Parse_error "Error parsing if-expressions.")
%}

(* basic tokens *)
%token <string> ID
%token <int> INT
%token <float> FLOAT
%token <string> STR
%token NEWLINE %token INDENT %token DEDENT %token EOF
(* keywords *)
%token TRUE    %token FALSE  %token NONE
%token IF      %token ELIF   %token ELSE
%token WHILE   %token BREAK  %token CONTINUE
%token DEF     %token GLOBAL %token NONLOCAL %token RETURN %token LAMBDA
(* word-like operators *)
%token AND %token OR %token NOT %token IS %token IN %token NOT_IN %token IS_NOT
(* symbolic operators *)
%token PLUS    %token MINUS  %token TIMES %token FP_DIV
%token INT_DIV %token MOD    %token EXP   %token EQ
%token NEQ     %token LT     %token GT    %token LEQ
%token GEQ     %token BW_AND %token BW_OR %token BW_COMP
%token BW_XOR  %token LSHIFT %token RSHIFT
(* delimiters *)
%token LPAREN   %token RPAREN(* %token LSQUARE  %token RSQUARE
%token LCURLY   %token RCURLY   %token DOT  *)  %token COMMA
%token COLON    %token SEMIC    %token ASSIG    %token PLUS_A
%token MINUS_A  %token TIMES_A  %token FP_DIV_A %token INT_DIV_A
%token MOD_A    %token EXP_A    %token BW_AND_A %token BW_OR_A
%token BW_XOR_A %token LSHIFT_A %token RSHIFT_A
(* pseudo start-symbols *)
%token START_FILE %token START_REPL

(* associativity and precedence *)
%nonassoc LAMBDA
%left OR
%left AND
%nonassoc NOT
%left IN NOT_IN IS IS_NOT LT LEQ GT GEQ NEQ EQ
%left BW_OR
%left BW_XOR
%left BW_AND
%nonassoc BW_COMP
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES FP_DIV INT_DIV MOD
%right EXP
(* %right LSQUARE LPAREN DOT RPAREN *)

(* type declarations *)
%start <Ast.program> input
%type <Ast.program> file_input
%type <Ast.program> repl_input
%type <Ast.stmt list> stmt
%type <Ast.stmt list> simple_stmt
%type <Ast.stmt list> small_stmts
%type <Ast.stmt> small_stmt
%type <Ast.stmt> flow_stmt
%type <Ast.stmt> compound_stmt
%type <Ast.expr> condition
%type <Ast.stmt list> else_clause
%type <Ast.stmt> if_stmt
%type <Ast.stmt> elif_stmt
%type <Ast.stmt> while_stmt
%type <Ast.stmt list> suite
%type <Ast.stmt list list> deep_suite
%type <Ast.stmt> expr_stmt
%type <Ast.expr> assignable_expr
%type <Ast.expr> cond_expr
%type <Ast.op> comp_op
%type <Ast.expr> expr
%type <Ast.stmt> aug_assign
%type <Ast.expr> primary
%type <Ast.expr> atom
%type <Ast.expr> call
%type <Ast.expr list> argument_list
%type <string list> param_id_list
%type <Ast.stmt> funcdef
%type <Ast.stmt> return_stmt
%type <Ast.stmt> global_stmt
%type <Ast.stmt> nonlocal_stmt

%% (* list of production rules *)

(* start symbols *)
input:
  START_FILE; f = file_input { f }
| START_REPL; r = repl_input; { r }
file_input:
  ss = stmt*; EOF { List.concat ss }
repl_input:
  NEWLINE { [] }
| s = simple_stmt { s }
| s = compound_stmt NEWLINE { [s] }

(* STATEMENTS *)
stmt:
  ss = simple_stmt { ss }
| s = compound_stmt { [s] }
(* simple statements *)
simple_stmt:
  ss = small_stmts; SEMIC?; NEWLINE { ss }
small_stmts:
  s = small_stmt { [s] }
| ss = small_stmts; SEMIC; s = small_stmt { ss @ [s] }
small_stmt:
  s = expr_stmt { s }
| s = flow_stmt { s }
| s = global_stmt { s }
| s = nonlocal_stmt { s }
flow_stmt:
  BREAK { Break }
| CONTINUE { Continue }
| s = return_stmt { s }

(* compound statements *)
compound_stmt:
  s = if_stmt { s }
| s = while_stmt { s }
| s = funcdef { s }
condition:
  t = expr { t }
| c = cond_expr { c }
else_clause:
  ELSE; COLON; s = suite { s }
if_stmt:
  IF; cond = condition; COLON;
    pos = suite;
  elifs = elif_stmt*;
  neg = else_clause? {
    build_if_tree cond pos elifs neg
  }
elif_stmt:
  ELIF; cond = condition; COLON; pos = suite {
    If(cond, pos, None)
  }
while_stmt:
  WHILE; cond = condition; COLON; pos = suite {
    While(cond, pos)
  }
suite:
  ds = deep_suite { List.concat ds }
deep_suite:
  s = simple_stmt; NEWLINE { [s] }
| NEWLINE; INDENT; ss = stmt+; DEDENT { ss }

(* EXPRESSIONS *)
expr_stmt:
  e = expr { Expr e }
| c = cond_expr { Expr c }
| s = ID; ASSIG; e = assignable_expr { Assign(s, e) }
| a = aug_assign { a }
assignable_expr:
  e = expr { e }
| c = cond_expr { c }
cond_expr:
  e1 = expr; IF; cond = expr; ELSE; e2 = assignable_expr {
    Cond(cond, e1, e2)
  }
comp_op:
  LT { Lt }   | GT { Gt }   | EQ { Eq } | LEQ { Leq }
| GEQ { Geq } | NEQ { Neq } | IN { In } | NOT_IN { NotIn }
| IS { Is }   | IS_NOT { IsNot }
expr:
  p = primary { p }
| e1 = expr; BW_OR; e2 = expr { Op(BwOr, [e1; e2]) }
| e1 = expr; BW_XOR; e2 = expr { Op(BwXor, [e1; e2]) }
| e1 = expr; BW_AND; e2 = expr { Op(BwAnd, [e1; e2]) }
| e1 = expr; LSHIFT; e2 = expr { Op(LShift, [e1; e2]) }
| e1 = expr; RSHIFT; e2 = expr { Op(RShift, [e1; e2]) }
| e1 = expr; PLUS; e2 = expr { Op(Plus, [e1; e2]) }
| e1 = expr; MINUS; e2 = expr { Op(Minus, [e1; e2]) }
| e1 = expr; TIMES; e2 = expr { Op(Times, [e1; e2]) }
| e1 = expr; FP_DIV; e2 = expr { Op(FpDiv, [e1; e2]) }
| e1 = expr; INT_DIV; e2 = expr { Op(IntDiv, [e1; e2]) }
| e1 = expr; MOD; e2 = expr { Op(Mod, [e1; e2]) }
| e1 = expr; EXP; e2 = expr { Op(Exp, [e1; e2]) }
| MINUS; e = expr { Op(Neg, [e]) }
| BW_COMP; e = expr { Op(BwComp, [e]) }
(* tests *)
| e1 = expr; OR; e2 = expr { Op(Or, [e1; e2]) }
| e1 = expr; AND; e2 = expr { Op(And, [e1; e2]) }
| NOT; e = expr { Op(Not, [e]) }
(* comparison *)
| e1 = expr; op = comp_op; e2 = expr { Op(op, [e1; e2]) } %prec EQ
(* parenthesised expressions *)
| LPAREN; e = expr; RPAREN { e }
(* lambda expression *)
| LAMBDA; args = param_id_list; COLON; body = expr {
    Lambda(args, body)
  } %prec LAMBDA
aug_assign:
  v = ID; BW_OR_A; e = expr { Assign(v, Op(BwOr, [Var v; e])) }
| v = ID; BW_XOR_A; e = expr { Assign(v, Op(BwXor, [Var v; e])) }
| v = ID; BW_AND_A; e = expr { Assign(v, Op(BwAnd, [Var v; e])) }
| v = ID; LSHIFT_A; e = expr { Assign(v, Op(LShift, [Var v; e])) }
| v = ID; RSHIFT_A; e = expr { Assign(v, Op(RShift, [Var v; e])) }
| v = ID; PLUS_A; e = expr { Assign(v, Op(Plus, [Var v; e])) }
| v = ID; MINUS_A; e = expr { Assign(v, Op(Minus, [Var v; e])) }
| v = ID; TIMES_A; e = expr { Assign(v, Op(Times, [Var v; e])) }
| v = ID; FP_DIV_A; e = expr { Assign(v, Op(FpDiv, [Var v; e])) }
| v = ID; INT_DIV_A; e = expr { Assign(v, Op(IntDiv, [Var v; e])) }
| v = ID; MOD_A; e = expr { Assign(v, Op(Mod, [Var v; e])) }
| v = ID; EXP_A; e = expr { Assign(v, Op(Exp, [Var v; e])) }
primary:
  a = atom { a }
| c = call { c }
atom:
  v = ID { Var v }
| i = INT { IntLit i }
| f = FLOAT { FloatLit f }
| s = STR { StrLit s }
| TRUE { BoolLit true }
| FALSE { BoolLit false }
| NONE { None }
call:
  p = primary; LPAREN; args = argument_list?; RPAREN {
    match args with
      Some l -> Call(p, l)
    | None   -> Call(p, [])
  }
argument_list:
  e = expr { [e] }
| e = expr; COMMA; rest = argument_list { e :: rest }
(* function declarations and statements *)
param_id_list:
  i = ID { [i] }
| i = ID; COMMA; rest = param_id_list { i :: rest }
funcdef:
  DEF; name = ID; LPAREN; args = param_id_list?; RPAREN; COLON; body = suite {
    match args with
      Some a -> Funcdef(name, a, body)
    | None   -> Funcdef(name, [], body)
  }
return_stmt:
  RETURN; e = expr { Return e }
global_stmt:
  GLOBAL; i = ID { Global i }
nonlocal_stmt:
  NONLOCAL; i = ID { Nonlocal i }
