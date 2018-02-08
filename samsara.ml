(* Main file *)
open Printf

let files = ref []

let addFile filename =
  files := filename :: !files

let compile file =
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

let rec compileFiles files =
  match files with
  | []       -> ()
  | (f::fs) ->
      compile f;
      compileFiles fs

let main () =
  Arg.parse [] addFile "this";
  compileFiles (List.rev !files)
  (* let myAST = Parser.computeAST () in *)
  (* let result = Parser.simplifyAST myAST in *)
  (* Parser.printAST myAST; *)
  (* printf "\n\nResult: %d\n" result *)

let () = main ()
