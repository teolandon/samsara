(* Parsing *)
open Printf

exception Invalid_token of string
exception Invalid_expr  of string

type eop =
  | EPlus
  | EMinus
  | EMult
  | EDiv
  | EMod

type ecomp =
  | ELess
  | EGreater

let string_of_ecomp (comp:ecomp) =
  match comp with
  | ELess    -> "<"
  | EGreater -> ">"

let comp_of_ecomp (comp:ecomp) =
  match comp with
  | ELess    -> ( < )
  | EGreater -> ( > )

let safe_division int1 int2 =
  match int2 with
  | 0 -> raise (Invalid_expr "Divide by 0")
  | _ -> int1 / int2

let op_of_eop (opr:eop) =
  match opr with
  | EPlus -> ( + )
  | EMinus -> ( - )
  | EMult -> ( * )
  | EDiv -> ( safe_division )
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
  | EIf
  | EOp   of eop
  | EComp of ecomp
  | EBool of bool
  | EInt  of int

let string_of_token t =
  match t with
  | ELeftParen  -> "("
  | ERightParen -> ")"
  | EOp   op -> string_of_eop op
  | EComp op -> string_of_ecomp op
  | EBool b  -> string_of_bool b
  | EInt  a  -> string_of_int a
  | EIf      -> "if"

type literal =
  | EInt  of int
  | EBool of bool

type ast =
  | ELit      of literal
  | EIf       of (ast * ast * ast)
  | EIntExpr  of ((int->int->int)  * ast * ast)
  | EComp     of ((int->int->bool) * ast * ast)
  (* | EBoolExpr of ((bool->bool->bool) * ast * ast) *)

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
    let (result, remaining) =
      match t with
      | EOp a ->
          let op = op_of_eop a in
          let (tree1, ts) = computeAST_h ts in
          let (tree2, ts) = computeAST_h ts in
          let result = EIntExpr (op, tree1, tree2) in
          (result, ts)
      | EComp a ->
          let comp = comp_of_ecomp a in
          let (tree1, ts) = computeAST_h ts in
          let (tree2, ts) = computeAST_h ts in
          let result = EComp (comp, tree1, tree2) in
          (result, ts)
      | EIf ->
          let (cond, ts)  = computeAST_h ts in
          let (tree1, ts) = computeAST_h ts in
          let (tree2, ts) = computeAST_h ts in
          let result = EIf (cond, tree1, tree2) in
          (result, ts)
      | _ -> raise (Invalid_token "Invalid expression after open paren")
    in
    match splitList remaining with
    | (ERightParen, rest) -> (result, rest)
    | _ -> raise (Invalid_token "Addition expression not closed")
  in
  let (t, ts) = splitList tokenList in
  match t with
  | EInt  x    -> ((ELit (EInt x)), ts)
  | EBool b    -> ((ELit (EBool b)), ts)
  | ELeftParen -> readOp ts
  | _          -> raise (Invalid_token "Expression not valid")

let rec evaluateAST tree:(ast) =
  match tree with
  | EIntExpr (op, tree1, tree2) ->
      let ev1 = evaluateAST tree1 in
      let ev2 = evaluateAST tree2 in
      (match (ev1, ev2) with
      | (ELit (EInt a), ELit (EInt b)) -> ELit (EInt (op a b))
      | _ ->
          raise (Invalid_expr "Integer operation takes integers only")
      )
  | EComp (op, tree1, tree2) ->
      let ev1 = evaluateAST tree1 in
      let ev2 = evaluateAST tree2 in
      (match (ev1, ev2) with
      | (ELit (EInt a), ELit (EInt b)) -> ELit (EBool (op a b))
      | _ ->
          raise (Invalid_expr "Comparison only takes integers")
      )
  | EIf (cond, tree1, tree2) ->
      let cond = evaluateAST cond  in
      (match cond with
      | ELit (EBool true)  -> evaluateAST tree1
      | ELit (EBool false) -> evaluateAST tree2
      | _ -> raise (Invalid_expr "\"if\" did not receive a boolean as condition")
      )
  | ELit _ as lit -> lit

let computeAST tokenList =
  let (tree, remainingTokens) = computeAST_h tokenList in
  match remainingTokens with
  | [] -> tree
  | _  -> raise (Invalid_token "Program not valid: trailing characters")

let createAndEvaluate tokenList =
  let tree = computeAST tokenList in
  evaluateAST tree

(* let rec printAST_h tree depth = *)
(*   let indent = String.make depth ' ' in *)
(*   match tree with *)
(*   | ELit a -> printf "%s%d\n" indent a *)
(*   | EIntExpr (op, tree1, tree2) -> *)
(*       printf "%sExpr of:\n" indent; *)
(*       printAST_h tree1 (depth+1); *)
(*       printAST_h tree2 (depth+1); *)
(*       printf "%s:fo rpxE\n" indent *)

let printAST tree =
  print_endline "PrintAST not implemented yet"
