open Printf
open Lexing

exception Trailing_chars

let stdin_flag = ref false
let repl_flag  = ref false
let lex_flag   = ref false
let parse_flag = ref false
let step_flag  = ref false
let type_flag  = ref false

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
    | Lexer.SyntaxError msg ->
        sprintf "Lexing Error: %s: %s" (position_str lexbuf) msg
    | Parser.Error ->
        sprintf "Syntax error %s: parser error" (position_str lexbuf)
    | Expr.Expr_error str ->
        sprintf "Invalid expression: %s" str
    | Expr.Type_error str ->
        sprintf "Type error: %s" str
    | Expr.Merge_error (e1, e2) ->
        sprintf "Couldn't merge %s and %s"
                (Expr.string_of_type e1)
                (Expr.string_of_type e2)
    | _ -> "Unknown error occured"

let failWith error lexbuf =
  printf "%s\n" (str_of_error error lexbuf)

let string_of_token token =
  match token with
  | Parser.EOF -> ""
  | Parser.PLUS -> "+"
  | Parser.MINUS -> "-"
  | Parser.STAR -> "*"
  | Parser.DIV -> "/"
  | Parser.MOD -> "%"
  | Parser.LEFT_PAREN -> "("
  | Parser.RIGHT_PAREN -> ")"
  | Parser.LEFT_BRACK -> "["
  | Parser.RIGHT_BRACK -> "]"
  | Parser.UNIT -> "()"
  | Parser.T_UNIT -> "unit"
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
  | Parser.ARROW  -> "=>"
  | Parser.TYPECHAIN  -> "->"
  | Parser.APPLY  -> "<-"
  | Parser.FIX    -> "fix"
  | Parser.NAN    -> "NaN"
  | Parser.T_NUM  -> "num"
  | Parser.T_BOOL -> "bool"
  | Parser.COLON  -> ":"
  | Parser.COMMA  -> ","
  | Parser.FST    -> "fst"
  | Parser.SND    -> "snd"
  | Parser.HD     -> "hd"
  | Parser.TL     -> "tl"
  | Parser.CONS   -> "::"
  | Parser.EMPTY  -> "empty"
  | Parser.NEW_LIST -> "[]"
  | Parser.SEQ      -> ";"
  | Parser.REF      -> "ref"
  | Parser.DEREF    -> "!"
  | Parser.ASSIGN_REF -> ":="
  | Parser.WHILE    -> "while"
  | Parser.DO       -> "do"
  | Parser.END      -> "end"
  | Parser.NEW      -> "new"
  | Parser.ARRAY    -> "array"
  | Parser.LENGTH   -> "length"
  | Parser.STOP     -> ";;"
  | Parser.PRINT    -> "print"

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
    | _ as err -> failWith err lexbuf; None

let type_and_evaluated lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr ->
        ignore(Expr.typecheck expr);
        let (typ, (_, value)) = Expr.evaluate_value [] expr in
        (Expr.string_of_type typ, Expr.string_of_expr value)
    | None -> ("", "")
  with
    | _ as err -> ("error", str_of_error err lexbuf)

let evaluated lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr ->
        ignore(Expr.typecheck expr);
        Expr.string_of_expr (snd (snd (Expr.evaluate_value [] expr)));
    | None -> ""
  with
    | _ as err -> str_of_error err lexbuf

let typechecked lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr -> Expr.string_of_type (Expr.typecheck expr);
    | None -> ""
  with
    | _ as err -> str_of_error err lexbuf

let non_evaluated lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr -> Expr.string_of_expr expr;
    | None -> ""
  with
    | _ as err -> str_of_error err lexbuf

let step_and_print lexbuf =
  try
    match parse_with_error lexbuf with
    | Some expr ->
        Expr.string_of_expr (snd (Expr.evaluate_print_steps [] expr));
    | None      -> ""
  with
    | _ as err -> str_of_error err lexbuf

let lexxd_str lexbuf =
  let rec loop lexbuf buf =
    let tok = Lexer.read lexbuf in
    match tok with
    | Parser.EOF -> Buffer.contents buf
    | _          ->
        (Buffer.add_string buf (string_of_token tok));
        (Buffer.add_string buf " ");
        loop lexbuf buf
  in
  let lex_string lexbuf =
    let tok = Lexer.read lexbuf in
    match tok with
    | Parser.EOF -> "[No tokens]"
    | _          ->
        let buf = Buffer.create 256 in
        (Buffer.add_string buf (string_of_token tok));
        (Buffer.add_string buf " ");
        loop lexbuf buf
  in
  try
    lex_string lexbuf
  with
  | error -> str_of_error error lexbuf

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

let rec loop_files files func =
  let loop_h files = loop_files files func in
  let func = (fun in_chan -> read_and_apply_func in_chan func) in
  match files with
  | [] -> ()
  | (f::fs) ->
      let in_c = open_in f in
      func {name=f; chan=in_c};
      close_in in_c;
      loop_h fs

let read_stdin func =
  let func = (fun in_chan -> read_and_apply_func in_chan func) in
  func {name="stdin"; chan=stdin}

let usageMsg = "Usage: samsara.native [-lex] [-parse] [-step] [-stdin] [-type] [-repl] [-help] [FILE...]"

let speclist = [
  ("-lex",   Arg.Set lex_flag, "prints the lexx'd list of tokens");
  ("-parse", Arg.Set parse_flag, "prints the parsed AST");
  ("-step",  Arg.Set step_flag, "prints step-by-step evaluation");
  ("-stdin", Arg.Set stdin_flag, "parses from stdin instead of files");
  ("-type",  Arg.Set type_flag, "only typechecks");
  ("-repl",  Arg.Set repl_flag, "enables the repl");
  ("--help", Arg.Unit (fun () -> ()), ""); (* Supresses flag *)
]

let repl_loop () =
  let set_prompt () = Ledit.set_prompt "$ " in
  let unset_prompt () = Ledit.set_prompt "  " in
  set_prompt ();
  let buf = Buffer.create 256 in
  let input () =
    Ledit.input_char stdin
  in
  let flush () =
    while not (Ledit.input_char stdin = "\n") do () done
  in
  let rec exit_loop c =
    match c with
    | ";" ->
        Buffer.add_string buf c;
        flush ();
        set_prompt ();
        Some (Buffer.contents buf)
    | _   ->
        Buffer.add_string buf c;
        None
  in
  let rec main_loop c =
    match c with
    | ";" ->
        Buffer.add_string buf c;
        (match exit_loop (input ()) with
        | None   -> main_loop (input ())
        | Some s -> s
        )
    | "\n" ->
        Buffer.add_string buf c;
        unset_prompt ();
        main_loop (input ())
    | _    ->
        Buffer.add_string buf c;
        main_loop (input ())
  in
  main_loop (Ledit.input_char stdin)

let repl () =
  let quit_loop = ref false in
  try
    while not !quit_loop do
      let str = repl_loop () in
      match str with
      | "exit;;" | "exit\n;;" -> quit_loop := true
      | ""     -> ()
      | _      ->
        let lexbuf = Lexing.from_string str in
        let (typ, value) = (type_and_evaluated lexbuf) in
        printf "t: %s = %s\n" typ value;
        print_endline "" (* Flush the buffer *)
    done
  with End_of_file -> print_newline ()

let main () =
  Arg.parse speclist addFile usageMsg;
  let files = (List.rev !files) in
  let read_function =
    match (!repl_flag, !stdin_flag, files) with
    | (true, _, _) | (_, false, []) -> (fun _ -> repl ())
    | (_, true, _)                  -> read_stdin
    | _                             -> loop_files files
  in
  match (!lex_flag, !parse_flag, !step_flag, !type_flag) with
  | (_, _, _, true) -> read_function typechecked
  | (_, _, true, _) -> read_function step_and_print
  | (_, true, _, _) -> read_function non_evaluated
  | (true, _, _, _) -> read_function lexxd_str
  | _               -> read_function evaluated

let () = main ()
