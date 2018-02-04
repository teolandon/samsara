(** Raised when an unexpected character is present in
 *  the file being lexed
 *)
exception Lexing_error of string

(** Lexes a file specified by the given string, and
 *  returns a token list with the tokens that correspond
 *  to the contents of the file
 *)
val lex : string -> Parser.token list
