{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let comp_of_str str =
  match str with
  | "<" -> LESS
  | ">" -> GREATER
  | "<=" -> LESS_EQ
  | ">=" -> GREATER_EQ
  | _    -> raise (SyntaxError "Unexpected comparison operator")
}

let int = ['0'-'9']+
let float = ['0'-'9']+ ['.'] ['0'-'9']+

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let comp = ['<' '>'] ['=']?

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | comp     { comp_of_str (Lexing.lexeme lexbuf) }
  | "true"   { BOOL true }
  | "false"  { BOOL false }
  | "NaN"    { NAN }
  | "if"     { IF }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { MULT }
  | '/'      { DIV }
  | '%'      { MOD }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
