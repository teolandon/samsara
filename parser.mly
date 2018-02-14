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
%token EOF

%start <Expr.value option> prog
%%

prog:
  | EOF { None }
  | e = expr { Some e }
  ;

expr:
  | c = comp   { Expr.val_of_bool c }
  | n = number  { Expr.val_of_num n }
  | b = boolean { b }
  ;

number:
  | i = INT   { `Int i }
  | f = FLOAT { `Float f }
  | o = opr   { o }

boolean:
  | b = BOOL { `Bool b }

opr:
  | LEFT_PAREN; operation = operand; a = number; b = number; RIGHT_PAREN
    { operation a b }
  ;

comp:
  | LEFT_PAREN; comp = comparison; a = number; b = number; RIGHT_PAREN
    { comp a b }
  ;

operand:
  | PLUS  { ( Expr.addition ) }
  | MINUS { ( Expr.subtraction ) }
  | MULT  { ( Expr.multiplication ) }
  | DIV   { ( Expr.division ) }
  | MOD   { ( Expr.modulo ) }

comparison:
  | LESS       { ( Expr.less ) }
  | GREATER    { ( Expr.greater ) }
  | LESS_EQ    { ( Expr.less_eq ) }
  | GREATER_EQ { ( Expr.greater_eq ) }
