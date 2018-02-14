open Printf
open Lexer
open Lexing

let lex_flag   = ref false
let parse_flag = ref false

let files = ref []

let addFile filename =
  files := filename :: !files

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  printf "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
    | SyntaxError msg ->
        printf "%a: %s\n" print_position lexbuf msg;
        None
    | Parser.Error ->
        printf "%a: parser error\n" print_position lexbuf;
        None
    | Expr.Expr_error str ->
        printf "Invalid expression: %s\n" str;
        None

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
    | Some expr ->
        print_endline (Expr.string_of_value expr);
        parse_and_print lexbuf
    | None -> ()

let string_of_token token =
  match token with
  | Parser.EOF -> ""
  | Parser.PLUS -> "+"
  | Parser.MINUS -> "-"
  | Parser.MULT -> "*"
  | Parser.DIV -> "/"
  | Parser.MOD -> "%"
  | Parser.LEFT_PAREN -> "("
  | Parser.RIGHT_PAREN -> ")"
  | Parser.INT i -> string_of_int i
  | Parser.FLOAT f -> string_of_float f
  | Parser.BOOL b  -> string_of_bool b
  | Parser.IF      -> "if"
  | Parser.LESS -> "<"
  | Parser.LESS_EQ -> "<="
  | Parser.GREATER -> ">"
  | Parser.GREATER_EQ -> ">="
  | Parser.NAN -> "NaN"

let print_lex_stream filename =
  let rec run lexbuf =
    try
      let tok = Lexer.read lexbuf in
      match tok with
      | Parser.EOF -> printf "\n"
      | _          -> printf "%s " (string_of_token tok); run lexbuf
    with
    | SyntaxError msg ->
        printf "%a: %s\n" print_position lexbuf msg
  in
  let in_f = open_in filename in
  let lexbuf = Lexing.from_channel in_f in
  printf "%s:\n\t" filename;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  run lexbuf;
  close_in in_f;;

let loop filename =
  let in_f = open_in filename in
  let lexbuf = Lexing.from_channel in_f in
  printf "%s:\n\t" filename;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in in_f;;

let rec loop_files files func =
  match files with
  | [] -> ()
  | (f::fs) -> func f; loop_files fs func

let usageMsg = "Usage: samsara [-lex] [-parse] FILE..."

let speclist = [
  ("-lex", Arg.Set lex_flag, "prints the lexx'd list of tokens");
  ("-parse", Arg.Set parse_flag, "prints the parsed AST");
  ("--help", Arg.Unit (fun () -> ()), ""); (* Supresses flag *)
]

let main () =
  Arg.parse speclist addFile usageMsg;
  let files = (List.rev !files) in
  match (!lex_flag, !parse_flag) with
  | (false, false) -> loop_files     files loop
  | (true , false) -> loop_files     files print_lex_stream
  | (_, true)      -> ()

let () = main ()
