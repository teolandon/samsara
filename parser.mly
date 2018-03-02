%{
  open Expr
%}
%token T_NUM
%token T_BOOL
%token T_UNIT
%token COLON
%token COMMA

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NAN
%token UNIT

%token LEFT_PAREN
%token RIGHT_PAREN

%token PLUS
%token MINUS
%token STAR
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
%token TYPECHAIN
%token APPLY
%token FIX

%token EOF

%right TYPECHAIN
%nonassoc ELSE IN ARROW
%nonassoc LESS GREATER LESS_EQ GREATER_EQ
%left MOD
%left MINUS PLUS
%left STAR DIV
%left APPLY

%start <Expr.expr option> prog
%%

prog:
  | EOF { None }
  | e = expr EOF { Some e }
  ;

expr:
  | e = exp  { e }
  | LEFT_PAREN; e1 = exp; COMMA; e2 = exp; RIGHT_PAREN  { `EPair (e1, e2) }
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
  | UNIT        { `EUnit }
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
  | STAR { EMult }
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
  | LET; id = ID; COLON; typ = typeset; ASSIGN; e1 = expr; IN; e2 = expr
    { `ELet (id, typ, e1, e2) }

defun:
  | FUN; LEFT_PAREN; id = ID; COLON; typ = typeset; RIGHT_PAREN; COLON;
    functype = typeset; ARROW; e = expr
      {`EFun (functype, id, typ, e)}
  | FUN; UNIT; COLON; functype = typeset; ARROW; e = expr
      {`EFun (functype, "", TUnit, e)}

fixfun:
  | FIX; func = ID; LEFT_PAREN; id = ID; COLON; typ = typeset; RIGHT_PAREN;
    COLON; functype = typeset; ARROW; e = expr
      {`EFix (func, functype, id, typ, e)}

appl:
  | e1 = expr; APPLY; e2 = expr
    { `EAppl (e1, e2) }

typeset:
  | t1 = typeset; TYPECHAIN; t2 = typeset { TChain (t1, t2) }
  | LEFT_PAREN; t1 = typeset; STAR; t2 = typeset; RIGHT_PAREN
    { TPair (t1, t2) }
  | T_NUM  { TNum }
  | T_BOOL { TBool }
  | T_UNIT { TUnit }
