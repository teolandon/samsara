(* Main file *)
open Printf

(* Mode flags *)

let lex_flag   = ref false
let parse_flag = ref false

let files = ref []

let addFile filename =
  files := filename :: !files

let evaluate file =
  printf "%s:\n\t" file;
  try
    let tokens = Lexer.lex file in
    let result = Parser.createAndEvaluate tokens in
    match result with
    | Parser.ENum (Parser.ELitInt   a) -> printf "%d\n" a
    | Parser.ENum (Parser.ELitFloat f) -> printf "%f\n" f
    | Parser.ENum Parser.ELitNaN       -> printf "NaN\n"
    | Parser.EBool b    -> printf "%b\n" b
    | _ -> printf "Tree not completely evaluated"
  with
  | Parser.Invalid_token str -> printf "Parser error: %s\n" str
  | Parser.Invalid_expr  str -> printf "Invalid expression: %s\n" str
  | Lexer.Lexing_error   str -> printf "Lexing error: %s\n" str

let print_tokens file =
  let rec print_tokens_rec tokens =
    match tokens with
    | []       -> printf "[No tokens]\n"
    | [t]      -> printf "%s\n" (Parser.string_of_token t)
    | (t::ts) ->
        printf "%s " (Parser.string_of_token t);
        print_tokens_rec ts
  in
  printf "%s:\n\t" file;
  try
    let tokens = Lexer.lex file in
    print_tokens_rec tokens
  with
  | Lexer.Lexing_error str -> printf "Lexing error: %s\n" str

let print_AST file =
  printf "%s:\n\t" file;
  try
    let tokens = Lexer.lex file in
    let ast = Parser.computeAST tokens in
    Parser.printAST ast
  with
  | _ -> print_endline "SOMETHING_ELSE"


let rec file_loop files func =
  match files with
  | []       -> ()
  | (f::fs) ->
      func f;
      file_loop fs func

let evaluate_files files =
  file_loop files evaluate

let print_tokens_files files =
  file_loop files print_tokens

let print_AST_files files =
  file_loop files print_AST

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
  | (false, false) -> evaluate_files     files
  | (true , false) -> print_tokens_files files
  | (_, true)      -> print_AST_files    files

let () = main ()
