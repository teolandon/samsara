%{
  open Expr
%}
%token STOP

%token T_NUM
%token T_BOOL
%token T_UNIT
%token COLON
%token COMMA

%token REF
%token DEREF
%token ASSIGN_REF
%token SEQ
%token WHILE
%token DO
%token END

%token NEW
%token ARRAY
%token LENGTH

%token HD
%token TL
%token CONS
%token EMPTY
%token NEW_LIST

%token FST
%token SND

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NAN
%token UNIT

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK

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

%nonassoc IN
%right TYPECHAIN SEQ
%nonassoc REF ASSIGN_REF
%right CONS
%nonassoc ELSE ARROW
%nonassoc LESS GREATER LESS_EQ GREATER_EQ
%left MOD
%left MINUS PLUS
%left STAR DIV
%left APPLY
%nonassoc DEREF
%nonassoc LEFT_BRACK

%start <Expr.expr option> prog
%%

prog:
  | EOF { None }
  | e = expr STOP? EOF { Some e }
  ;

expr:
  | e = exp       { e }
  | LEFT_PAREN; e = exp; RIGHT_PAREN  { e }

exp:
  | r = refs    { r }
  | a = arrays  { a }
  | w = wloop   { w }
  | c = cond    { c }
  | n = number  { n }
  | b = boolean { b }
  | l = letbind { l }
  | f = defun   { f }
  | f = fixfun  { f }
  | a = appl    { a }
  | p = pairs   { p }
  | l = lists   { l }
  | i = ID      { EId i }
  | UNIT        { EUnit }
  ;

wloop:
  | WHILE; e1 = expr; DO; e2 = expr; END { EWhile (e1, e2) }

refs:
  | REF; e = expr   { ERef e }
  | DEREF; e = expr { EDeref e }
  | e1 = expr; ASSIGN_REF; e2 = expr { EAssign (e1, e2) }
  | e1 = expr; SEQ; e2 = expr { ESeq (e1, e2) }

arrays:
  | NEW; t = typeset; LEFT_BRACK; e = expr; RIGHT_BRACK; { ENewArray (t, e) }
  | e1 = expr; LEFT_BRACK; e2 = expr; RIGHT_BRACK; { EArrayRef (e1, e2) }
  | LENGTH; APPLY; e = expr { ELength e }

lists:
  | NEW_LIST; t = id_typeset?
    { ENewList (match t with None -> TInfer | Some t -> t) }
  | e1 = expr; CONS; e2 = expr   { ECons (e1, e2) }
  | HD; APPLY; e = expr          { EHead e }
  | TL; APPLY; e = expr          { ETail e }
  | EMPTY; APPLY; e = expr       { EEmpty e }

pairs:
  | FST; APPLY; e = expr { EFst e }
  | SND; APPLY; e = expr { ESnd e }
  | LEFT_PAREN; e1 = expr; COMMA; e2 = expr; RIGHT_PAREN  { EPair (e1, e2) }

number:
  | i = INT   { EInt i }
  | f = FLOAT { EFloat f }
  | o = opr   { o }
  | NAN       { ENaN }

boolean:
  | b = BOOL { EBool b }
  | c = comp { c }

opr:
  | a = expr; operation = operator; b = expr;
    { EOpr (operation, a, b) }
  ;
%inline operator:
  | PLUS  { EPlus }
  | MINUS { EMinus }
  | STAR  { EMult }
  | DIV   { EDiv }
  | MOD   { EMod }

comp:
  | a = expr; comp = comparison; b = expr
    { EComp (comp, a, b) }
  ;
%inline comparison:
  | LESS       { ELess }
  | GREATER    { EGreater }
  | LESS_EQ    { ELessEq }
  | GREATER_EQ { EGreaterEq }

cond:
  | IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr { EIf (c, e1, e2) }

letbind:
  | LET; id = ID; typ = id_typeset?; ASSIGN; e1 = expr; IN; e2 = expr
    {
      match typ with
      | None     -> ELet (id, TInfer, e1, e2)
      | Some typ -> ELet (id, typ, e1, e2)
    }

id_typeset:
  | COLON; typ=typeset { typ }

defun:
  | FUN; arg_type = funarg; functype = id_typeset?; ARROW; e = expr
    {
      let functype =
        match functype with
        | None          -> TInfer
        | Some functype -> functype
      in
      match arg_type with (id, typ) ->
      EFun (functype, id, typ, e)
    }
  | FUN; UNIT; functype = id_typeset?; ARROW; e = expr
    {
      match functype with
      | None          -> EFun (TInfer, "", TUnit, e)
      | Some functype -> EFun (functype, "", TUnit, e)
    }

fixfun:
  | FIX; func = ID; arg_type = funarg; functype = id_typeset?;
    ARROW; e = expr
    {
      let functype =
        match functype with
        | None          -> TInfer
        | Some functype -> functype
      in
      match arg_type with (id, typ) ->
      EFix (func, functype, id, typ, e)
    }

funarg:
  | id = ID { (id, TInfer) }
  | LEFT_PAREN; id = ID; COLON; typ = typeset; RIGHT_PAREN { (id, typ) }

appl:
  | e1 = expr; APPLY; e2 = expr { EAppl (e1, e2) }

typeset:
  | t1 = typeset; TYPECHAIN; t2 = typeset { TChain (t1, t2) }
  | LEFT_PAREN; t1 = typeset; STAR; t2 = typeset; RIGHT_PAREN
    { TPair (t1, t2) }
  | LEFT_BRACK; t = typeset; RIGHT_BRACK { TList t }
  | ARRAY; LESS; t = typeset; GREATER { TArray t }
  | LESS; t = typeset; GREATER { TRef t }
  | T_NUM  { TNum }
  | T_BOOL { TBool }
  | T_UNIT { TUnit }
