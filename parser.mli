(** Raised when constructing an AST and syntax is wrong *)
exception Invalid_token of string

(** Raised when evaluating an AST and an illegal expression is evaluated *)
exception Invalid_expr of string

(** Represents the tokens for arithmetic operations *)
type eop = EPlus | EMinus | EMult | EDiv | EMod

(** Represents the tokens for comparison operations *)
type ecomp = ELess | ELessEq | EGreater | EGreaterEq

(** Represents the tokens that make up the Samsara syntax *)
type token = ELeftParen | ERightParen | EIf | EOp of eop
           | EComp of ecomp | EBool of bool | EInt of int
           | EFloat of float

(** AST numbers, simplified values *)
type number = ELitInt of int | ELitFloat of float

(** Represents the possible nodes of an AST, a literal that holds
 *  an int, and the sum of two ASTs.
 *)
type ast = ENum of number
         | EBool of bool
         | EIf      of (ast * ast * ast)
         | ENumExpr of ((number->number->number) * ast * ast)
         | EComp    of ((number->number->bool) * ast * ast)

val print_tokens : token list -> unit

(** Evaluates the given AST and returns the simplified ast value of its
 * evaluation
 *)
val evaluateAST : ast -> ast

(** Computes an AST according to the given token list,
 *  raising any exceptions if the syntax is wrong
 *)
val computeAST : token list -> ast

(** Equivalent to (evaluateAST (computeAST tokenList)) *)
val createAndEvaluate : token list -> ast

(** Prints an AST using indentation to show branches *)
val printAST : ast -> unit
