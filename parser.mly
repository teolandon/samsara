%{
  exception Not_bool
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token LEFT_PAREN
%token RIGHT_PAREN
%token PLUS
%token MINUS
%token MULT
%token DIV
%token MOD
%token IF
%token LESS
%token GREATER
%token LESS_EQ
%token GREATER_EQ
%token NAN
%token EOF

%start <Expr.value option> prog
%%

prog:
  | EOF { None }
  | e = expr { Some e }
  ;

expr:
  | c = cond    { c }
  | n = number  { n }
  | b = boolean { b }
  ;

number:
  | i = INT   { EInt i }
  | f = FLOAT { EFloat f }
  | o = opr   { o }
  | NAN   { ENaN }

boolean:
  | b = BOOL { EBool b }
  | c = comp { c }

opr:
  | LEFT_PAREN; operation = operand; a = number; b = number; RIGHT_PAREN
    { operation a b }
  ;

comp:
  | LEFT_PAREN; comp = comparison; a = number; b = number; RIGHT_PAREN
    { comp a b }
  ;

cond:
  | LEFT_PAREN; IF; c = boolean; e1 = expr; e2 = expr; RIGHT_PAREN
    {
      match c with
      | EBool b -> if b then e1 else e2
      | _       -> raise Not_bool
    }

operand:
  | PLUS  { Expr.addition }
  | MINUS { Expr.subtraction }
  | MULT  { Expr.multiplication }
  | DIV   { Expr.division }
  | MOD   { Expr.modulo }

comparison:
  | LESS       { Expr.less }
  | GREATER    { Expr.greater }
  | LESS_EQ    { Expr.less_eq }
  | GREATER_EQ { Expr.greater_eq }
