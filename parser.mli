(** Represents the tokens for arithmetic operations *)
type eop = EPlus | EMinus | EMult | EDiv | EMod

(** Represents the tokens that make up the Samsara syntax *)
type token = ELeftParen | ERightParen | EOp of eop | EInt of int

(** Represents the possible nodes of an AST, a literal that holds
 *  an int, and the sum of two ASTs.
 *)
type ast = ELit of int | EExpr of ((int->int->int) * ast * ast)

(** Raised when constructing the AST and syntax is wrong *)
exception Invalid_token of string

(** Evaluates the given AST and returns the int value of
 *  its evaluation
 *)
val evaluateAST : ast -> int

(** Computes an AST according to the given token list,
 *  raising any exceptions if the syntax is wrong
 *)
val computeAST : token list -> ast

(** Equivalent to (evaluateAST (computeAST tokenList)) *)
val createAndEvaluate : token list -> int

(** Prints an AST using indentation to show branches *)
val printAST : ast -> unit
