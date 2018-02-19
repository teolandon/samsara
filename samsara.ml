open Printf
open Lexer
open Lexing

let lex_flag   = ref false
let parse_flag = ref false

let files = ref []

let addFile filename =
  files := filename :: !files

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
  | Parser.THEN    -> "then"
  | Parser.ELSE    -> "else"
  | Parser.LESS -> "<"
  | Parser.LESS_EQ -> "<="
  | Parser.GREATER -> ">"
  | Parser.GREATER_EQ -> ">="
  | Parser.LET    -> "let"
  | Parser.IN     -> "in"
  | Parser.ASSIGN -> "="
  | Parser.ID str -> "id:" ^ str
  | Parser.FUN    -> "fun"
  | Parser.ARROW  -> "->"
  | Parser.APPLY  -> "<-"
  | Parser.NAN    -> "NaN"

let position_str lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "(%d:%d)"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
    | SyntaxError msg ->
        printf "Lexing Error: %s: %s" (position_str lexbuf) msg;
        None
    | Parser.Error ->
        printf "Syntax error %s: parser error" (position_str lexbuf);
        None
    | Expr.Expr_error str ->
        printf "Invalid expression: %s" str;
        None

let print_evaluated lexbuf =
  match parse_with_error lexbuf with
  | Some expr -> Expr.string_of_value (Expr.evaluate_value expr);
  | None -> ""

let print_non_evaluated lexbuf =
  match parse_with_error lexbuf with
  | Some expr -> Expr.string_of_value expr;
  | None -> ""

let parsed_str filename =
  let in_f = open_in filename in
  let lexbuf = Lexing.from_channel in_f in
  printf "%s:\n\t" filename;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  printf "%s\n" (print_non_evaluated lexbuf);
  close_in in_f;;

let lexed_str filename =
  let rec run lexbuf curr_str =
    let tok = Lexer.read lexbuf in
    match tok with
    | Parser.EOF -> curr_str
    | _          ->
        let new_str = sprintf "%s%s " curr_str (string_of_token tok) in
        run lexbuf new_str
  in
  let lex_string lexbuf =
    let tok = Lexer.read lexbuf in
    match tok with
    | Parser.EOF -> "[No tokens]"
    | _          -> run lexbuf ((string_of_token tok) ^ " ")
  in
  let in_f = open_in filename in
  let lexbuf = Lexing.from_channel in_f in
  printf "%s:\n\t" filename;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let lexxd_str =
    try
      lex_string lexbuf
    with
    | SyntaxError msg ->
        sprintf "Lexing error %s: %s" (position_str lexbuf) msg
    | Parser.Error ->
        sprintf "Syntax error %s: parser error" (position_str lexbuf)
    | Expr.Expr_error str ->
        sprintf "Invalid expression: %s" str
  in
  printf "%s\n" lexxd_str;
  close_in in_f;;

let loop filename =
  let in_f = open_in filename in
  let lexbuf = Lexing.from_channel in_f in
  printf "%s:\n\t" filename;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  printf "%s\n" (print_evaluated lexbuf);
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
  | (true , false) -> loop_files     files lexed_str
  | (_, true)      -> loop_files     files parsed_str

let () = main ()
