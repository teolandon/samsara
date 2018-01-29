%token <int> INT
%token LEFT_PAREN
%token RIGHT_PAREN
%token PLUS
%token EOF

%start <int option> code
%%

code:
  | EOF { None }
  | e = expr { Some e }

expr:
  | a = addition { a }
  | b = INT { b }
  ;

addition:
  | LEFT_PAREN; PLUS; a = expr; b = expr; RIGHT_PAREN
    { a + b }
  ;
