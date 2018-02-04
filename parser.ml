(* Parsing *)
open Printf

type token =
  | ELeftParen | ERightParen
  | EPlus
  | EInt of int

exception Invalid_token

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

let rec computeAST_h tokenList =
  match tokenList with
  | ((EInt x):ts) -> ((ELit x), ts)
  | ELeftParen ->
      (match readToken () with
      | EPlus ->
          let tree1 = computeAST_h listRef in
          let tree2 = computeAST_h listRef in
          let sum = ESum (tree1, tree2) in
          (match readToken () with
          | ERightParen -> sum
          | _           -> raise Invalid_token
          )
      | _ -> raise Invalid_token
      )
  | _ -> raise Invalid_token

let computeAST () =
  let listRef = ref (List.rev !tokenList) in
  computeAST_h listRef

let rec printAST_h tree depth =
  let indent = String.make depth ' ' in
  match tree with
  | ELit a -> printf "%s%d\n" indent a
  | ESum (tree1, tree2) ->
      printf "%sSum of:\n" indent;
      printAST_h tree1 (depth+1);
      printAST_h tree2 (depth+1);
      printf "%s:fo muS\n" indent

let rec simplifyAST tree =
  match tree with
  | ELit a              -> a
  | ESum (tree1, tree2) -> (simplifyAST tree1) + (simplifyAST tree2)

let createAndEvaluate tokenList =
  computeAST

let printAST tree =
  printAST_h tree 0
