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

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "<="     { LESS_EQ }
  | ">="     { GREATER_EQ }
  | "->"     { TYPECHAIN }
  | "<"      { LESS }
  | ">"      { GREATER }
  | "true"   { BOOL true }
  | "false"  { BOOL false }
  | "NaN"    { NAN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "let"    { LET }
  | "fun"    { FUN }
  | "=>"     { ARROW }
  | "<-"     { APPLY }
  | "fix"    { FIX }
  | "num"    { T_NUM }
  | "bool"   { T_BOOL }
  | "hd"     { HD }
  | "tl"     { TL }
  | "empty"  { EMPTY }
  | "while"  { WHILE }
  | "do"     { DO }
  | "end"    { END }
  | "new"    { NEW }
  | "array"  { ARRAY }
  | "length" { LENGTH }
  | "[]"     { NEW_LIST }
  | "["      { LEFT_BRACK }
  | "]"      { RIGHT_BRACK }
  | "::"     { CONS }
  | ":="     { ASSIGN_REF }
  | ";;"     { STOP }
  | ";"      { SEQ }
  | "ref"    { REF }
  | "!"      { DEREF }
  | ":"      { COLON }
  | "="      { ASSIGN }
  | "in"     { IN }
  | "unit"   { T_UNIT }
  | "fst"    { FST }
  | "snd"    { SND }
  | "()"     { UNIT }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { STAR }
  | '/'      { DIV }
  | '%'      { MOD }
  | ','      { COMMA }
  | id       { ID (Lexing.lexeme lexbuf) }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
