(* Parsing *)
open Printf

type token =
  | ELeftParen | ERightParen
  | EPlus
  | EInt of int

exception Invalid_token of string

let string_of_token t =
  match t with
  | ELeftParen -> "("
  | ERightParen -> ")"
  | EPlus -> "+"
  | EInt a -> string_of_int a

let tokenList: token list ref = ref []

let addToken t =
  tokenList := t :: !tokenList

let rec print_tokens_h token_list =
  match token_list with
  | [] -> ()
  | (t::ts) -> print_endline (string_of_token t); print_tokens_h ts

let print_tokens () =
  print_tokens_h (List.rev !tokenList)

type ast =
  | ELit of int
  | ESum of (ast * ast)

let rec computeAST_h listRef =
  let readToken () =
    match !listRef with
    | []      -> raise (Invalid_token "Expression not valid")
    | (t::ts) -> listRef := ts; t
  in
  match readToken () with
  | EInt x -> ((ELit x), !listRef)
  | ELeftParen ->
      (match readToken () with
      | EPlus ->
          let (tree1, _) = computeAST_h listRef in
          let (tree2, _) = computeAST_h listRef in
          let sum = ESum (tree1, tree2) in
          (match readToken () with
          | ERightParen -> (sum, !listRef)
          | _           ->
              raise (Invalid_token "Addition expresion not closed")
          )
      | _ -> raise (Invalid_token "No operator found after paren")
      )
  | _ -> raise (Invalid_token "Expression not valid")

let rec evaluateAST tree =
  match tree with
  | ELit a              -> a
  | ESum (tree1, tree2) -> (evaluateAST tree1) + (evaluateAST tree2)

let computeAST tokenList =
  let listRef = ref tokenList in
  let (tree, remainingTokens) = computeAST_h listRef in
  match remainingTokens with
  | [] -> tree
  | _  -> raise (Invalid_token "Program not valid: trailing characters")

let createAndEvaluate tokenList =
  let tree = computeAST tokenList in
  evaluateAST tree

let rec printAST_h tree depth =
  let indent = String.make depth ' ' in
  match tree with
  | ELit a -> printf "%s%d\n" indent a
  | ESum (tree1, tree2) ->
      printf "%sSum of:\n" indent;
      printAST_h tree1 (depth+1);
      printAST_h tree2 (depth+1);
      printf "%s:fo muS\n" indent

let printAST tree =
  printAST_h tree 0
