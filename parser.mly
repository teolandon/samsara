%{
  open Expr
%}
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NAN

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

%token <string> ID
%token ASSIGN
%token LET
%token IN
%token FUN
%token ARROW
%token APPLY
%token FIX

%token EOF

%nonassoc ELSE IN ARROW
%nonassoc LESS GREATER LESS_EQ GREATER_EQ
%left MOD
%left MINUS PLUS
%left MULT DIV
%left APPLY

%start <Expr.expr option> prog
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
  | l = letbind { l }
  | f = defun   { f }
  | f = fixfun  { f }
  | a = appl    { a }
  | i = ID      { `EId i }
  ;

number:
  | i = INT   { `EInt i }
  | f = FLOAT { `EFloat f }
  | o = opr   { o }
  | NAN       { `ENaN }

boolean:
  | b = BOOL { `EBool b }
  | c = comp { c }

opr:
  | a = expr; operation = operator; b = expr;
    { `EOpr (operation, a, b) }
  ;
%inline operator:
  | PLUS  { EPlus }
  | MINUS { EMinus }
  | MULT  { EMult }
  | DIV   { EDiv }
  | MOD   { EMod }

comp:
  | a = expr; comp = comparison; b = expr
    { `EComp (comp, a, b) }
  ;
%inline comparison:
  | LESS       { ELess }
  | GREATER    { EGreater }
  | LESS_EQ    { ELessEq }
  | GREATER_EQ { EGreaterEq }

cond:
  | IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr
    { `EIf (c, e1, e2) }

letbind:
  | LET; id = ID; ASSIGN; e1 = expr; IN; e2 = expr
    { `ELet (id, e1, e2) }

defun:
  | FUN; id = ID; ARROW; e = expr
    {`EFun (id, e)}

fixfun:
  | FIX; func = ID; id = ID; ARROW; e = expr
    {`EFix (func, id, e)}

appl:
  | e1 = expr; APPLY; e2 = expr
    { `EAppl (e1, e2) }
