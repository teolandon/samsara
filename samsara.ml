(* Main file *)

open Arg
open Printf
open Lexer
open Lexing

let file = ref ""

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  printf "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.expr Lexer.read lexbuf with
    | SyntaxError msg ->
        fprintf stderr "%a: %s\n" print_position lexbuf msg;
        Some 0
    | Parser.Error ->
        fprintf stderr "%a: syntax error\n" print_position lexbuf;
        exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
    | Some addition ->
        printf "%d\n" addition;
        parse_and_print lexbuf
    | None ->
        printf "EOF reached\n";

let loop filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in inx;;

Arg.parse [] loop ""
