open Printf
open Lexer
open Lexing

let stdin_flag   = ref false
let lex_flag   = ref false
let parse_flag = ref false
let step_flag = ref false

let files = ref []

let addFile filename =
  files := filename :: !files

let position_str lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "(%d:%d)"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let str_of_error error lexbuf =
  match error with
    | SyntaxError msg ->
        sprintf "Lexing Error: %s: %s" (position_str lexbuf) msg
    | Parser.Error ->
        sprintf "Syntax error %s: parser error" (position_str lexbuf)
    | Expr.Expr_error str ->
        sprintf "Invalid expression: %s" str
    | _ -> "Unknown error occured"

let failWith error lexbuf =
  match error with
    | SyntaxError msg ->
        printf "Lexing Error: %s: %s" (position_str lexbuf) msg
    | Parser.Error ->
        printf "Syntax error %s: parser error" (position_str lexbuf)
    | Expr.Expr_error str ->
        printf "Invalid expression: %s" str
    | _ -> printf "Unknown error occured"

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
  | Parser.FIX    -> "fix"
  | Parser.NAN    -> "NaN"

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
    | _ as err -> failWith err lexbuf; None

let evaluated lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr -> Expr.string_of_value (Expr.evaluate_value expr);
    | None -> ""
  with
    | _ as err -> str_of_error err lexbuf

let non_evaluated lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr -> Expr.string_of_value expr;
    | None -> ""
  with
    | _ as err -> str_of_error err lexbuf

let step_and_print lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr -> Expr.string_of_value (Expr.evaluate_print_steps expr);
    | None      -> ""
  with
    | _ as err -> str_of_error err lexbuf

let lexxd_str lexbuf =
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
  try
    lex_string lexbuf
  with
  | SyntaxError msg ->
      sprintf "Lexing error %s: %s" (position_str lexbuf) msg
  | Parser.Error ->
      sprintf "Syntax error %s: parser error" (position_str lexbuf)
  | Expr.Expr_error str ->
      sprintf "Invalid expression: %s" str

type named_chan = {
  name : string;
  chan : in_channel;
}

let read_and_apply_func named_chan func =
  match named_chan with {name=filename;chan=in_chan} ->
  let lexbuf = Lexing.from_channel in_chan in
  printf "%s:\n" filename;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  printf "%s\n" (func lexbuf)

let evaluate in_chan =
  read_and_apply_func in_chan evaluated

let parsed_str in_chan =
  read_and_apply_func in_chan non_evaluated

let lexed_str in_chan =
  read_and_apply_func in_chan lexxd_str

let step in_chan =
  read_and_apply_func in_chan step_and_print

let rec loop_files files func =
  match files with
  | [] -> ()
  | (f::fs) ->
      let in_c = open_in f in
      func {name=f;chan=in_c};
      close_in in_c;
      loop_files fs func

let read_stdin func =
  func {name="stdin";chan=stdin}

let usageMsg = "Usage: samsara [-lex] [-parse] FILE..."

let speclist = [
  ("-lex", Arg.Set lex_flag, "prints the lexx'd list of tokens");
  ("-parse", Arg.Set parse_flag, "prints the parsed AST");
  ("-step", Arg.Set step_flag, "prints step-by-step evaluation");
  ("-stdin", Arg.Set stdin_flag, "parses from stdin instead of files");
  ("--help", Arg.Unit (fun () -> ()), ""); (* Supresses flag *)
]

let main () =
  Arg.parse speclist addFile usageMsg;
  let files = (List.rev !files) in
  let read_function =
    match (!stdin_flag, files) with
    | (true, _) | (_, []) -> read_stdin
    | _                   -> loop_files files
  in
  match (!lex_flag, !parse_flag, !step_flag) with
  | (_, _, true) -> read_function step
  | (_, true, _) -> read_function parsed_str
  | (true, _, _) -> read_function lexed_str
  | _            -> read_function evaluate

let () = main ()
