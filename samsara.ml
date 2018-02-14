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
        printf "%a: syntax error\n" print_position lexbuf;
        None
    | Expr.LOL ->
        print_endline "Some expression error";
        None

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
    | Some expr ->
        print_endline (Expr.string_of_value expr);
        parse_and_print lexbuf
    | None -> ()

let loop filename =
  let in_f = open_in filename in
  let lexbuf = Lexing.from_channel in_f in
  printf "%s:\n\t" filename;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in in_f;;

let rec loop_files files =
  match files with
  | [] -> ()
  | (f::fs) -> loop f; loop_files fs

let usageMsg = "Usage: samsara [-lex] [-parse] FILE..."

let speclist = [
  ("-lex", Arg.Set lex_flag, "prints the lexx'd list of tokens");
  ("-parse", Arg.Set parse_flag, "prints the parsed AST");
  ("--help", Arg.Unit (fun () -> ()), ""); (* Supresses flag *)
]

let main () =
  Arg.parse speclist addFile usageMsg;
  let files = (List.rev !files) in
  loop_files files
  (* match (!lex_flag, !parse_flag) with *)
  (* | (false, false) -> evaluate_files     files *)
  (* | (true , false) -> print_tokens_files files *)
  (* | (_, true)      -> print_AST_files    files *)

let () = main ()
