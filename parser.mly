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
%token THEN
%token ELSE
%token LESS
%token GREATER
%token LESS_EQ
%token GREATER_EQ
%token NAN
%token EOF

%nonassoc LESS GREATER LESS_EQ GREATER_EQ
%nonassoc ELSE
%left MOD
%left MINUS PLUS
%left MULT DIV

%start <Expr.value option> prog
%%

prog:
  | EOF { None }
  | e = expr EOF { Some e }
  ;

expr:
  | e = exp  { e }
  | LEFT_PAREN; e = exp; RIGHT_PAREN  { e }

exp:
  | c = cond    { c }
  | n = number  { n }
  | b = boolean { b }
  ;

number:
  | i = INT   { EInt i }
  | f = FLOAT { EFloat f }
  | o = opr   { o }
  | NAN       { Expr.ENaN }

boolean:
  | b = BOOL { EBool b }
  | c = comp { c }

opr:
  | a = expr; operation = operator; b = expr;
    { operation a b }
  ;
%inline operator:
  | PLUS  { Expr.addition }
  | MINUS { Expr.subtraction }
  | MULT  { Expr.multiplication }
  | DIV   { Expr.division }
  | MOD   { Expr.modulo }

comp:
  | a = expr; comp = comparison; b = expr
    { comp a b }
  ;
%inline comparison:
  | LESS       { Expr.less }
  | GREATER    { Expr.greater }
  | LESS_EQ    { Expr.less_eq }
  | GREATER_EQ { Expr.greater_eq }

cond:
  | IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr
    {
      match c with
      | EBool b -> if b then e1 else e2
      | _       -> raise Not_bool
    }
