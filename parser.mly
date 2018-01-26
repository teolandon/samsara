%token <int> INT
%token LEFT_PAREN
%token RIGHT_PAREN
%token PLUS
%token EOF

%start <int option> expr
%%

expr:
  | EOF { None }
  | a = addition { Some a }
  ;

addition:
  | LEFT_PAREN; PLUS; a = INT; b = INT; RIGHT_PAREN
    { a + b }
  ;
