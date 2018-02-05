(* Parsing *)
open Printf

type eop =
  | EPlus
  | EMinus
  | EMult
  | EDiv
  | EMod

let op_of_eop (opr:eop) =
  match opr with
  | EPlus -> ( + )
  | EMinus -> ( - )
  | EMult -> ( * )
  | EDiv -> ( / )
  | EMod -> ( mod )

let string_of_eop (opr:eop) =
  match opr with
  | EPlus -> "+"
  | EMinus -> "-"
  | EMult -> "*"
  | EDiv -> "/"
  | EMod -> "%"

type token =
  | ELeftParen | ERightParen
  | EOp of eop
  | EInt of int

type ast =
  | ELit of int
  | EExpr of ((int->int->int) * ast * ast)

exception Invalid_token of string

let string_of_token t =
  match t with
  | ELeftParen -> "("
  | ERightParen -> ")"
  | EOp op -> string_of_eop op
  | EInt a -> string_of_int a

let rec print_tokens_h token_list =
  match token_list with
  | [] -> ()
  | (t::ts) -> print_endline (string_of_token t); print_tokens_h ts

let rec computeAST_h tokenList =
  let splitList tokens =
    match tokens with
    | []      -> raise (Invalid_token "Expression not valid")
    | (t::ts) -> (t, ts)
  in
  let readOp tokens =
    let (t, ts) = splitList tokens in
    match t with
    | EOp a ->
        let op = op_of_eop a in
        let (tree1, ts) = computeAST_h ts in
        let (tree2, ts) = computeAST_h ts in
        let sum = EExpr (op, tree1, tree2) in
        (match splitList ts with
        | (ERightParen, remaining) -> (sum, remaining)
        | _ -> raise (Invalid_token "Addition expression not closed")
        )
    | _ -> raise (Invalid_token "No + after open paren")
  in
  let (t, ts) = splitList tokenList in
  match t with
  | EInt x -> ((ELit x), ts)
  | ELeftParen ->
      readOp ts
  | _ -> raise (Invalid_token "Expression not valid")

let rec evaluateAST tree =
  match tree with
  | ELit a              -> a
  | EExpr (op, tree1, tree2) -> op (evaluateAST tree1) (evaluateAST tree2)

let computeAST tokenList =
  let (tree, remainingTokens) = computeAST_h tokenList in
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
  | EExpr (op, tree1, tree2) ->
      printf "%sExpr of:\n" indent;
      printAST_h tree1 (depth+1);
      printAST_h tree2 (depth+1);
      printf "%s:fo rpxE\n" indent

let printAST tree =
  printAST_h tree 0
