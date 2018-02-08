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
  | ELessEq
  | EGreater
  | EGreaterEq

let string_of_ecomp (comp:ecomp) =
  match comp with
  | ELess      -> "<"
  | ELessEq    -> "<="
  | EGreater   -> ">"
  | EGreaterEq -> ">="

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
  | EOp    of eop
  | EComp  of ecomp
  | EBool  of bool
  | EInt   of int
  | EFloat of float
  | ENaN

let string_of_token t =
  match t with
  | ELeftParen  -> "("
  | ERightParen -> ")"
  | EOp    op -> string_of_eop op
  | EComp  op -> string_of_ecomp op
  | EBool  b  -> string_of_bool b
  | EInt   a  -> string_of_int a
  | EFloat f  -> string_of_float f
  | EIf       -> "if"
  | ENaN      -> "NaN"

type number =
  | ELitInt   of int
  | ELitFloat of float
  | ELitNaN

type ast =
  | ENum      of number
  | EBool     of bool
  | EIf       of (ast * ast * ast)
  | ENumExpr  of ((number->number->number) * ast * ast)
  | EComp     of ((number->number->bool)   * ast * ast)
  (* | EBoolExpr of ((bool->bool->bool) * ast * ast) *)

let comp_helper num1 num2 int_comp float_comp =
  match (num1, num2) with
  | (_, ELitNaN) | (ELitNaN, _) -> false
  | (ELitInt   a, ELitInt   b) -> int_comp a b
  | (ELitFloat a, ELitFloat b) -> float_comp a b
  | (ELitInt   a, ELitFloat b) -> float_comp (float_of_int a) b
  | (ELitFloat a, ELitInt   b) -> float_comp a (float_of_int b)

let less_than num1 num2 =
  comp_helper num1 num2 ( < ) ( < )

let greater_than num1 num2 =
  comp_helper num1 num2 ( > ) ( > )

let less_than_eq num1 num2 =
  comp_helper num1 num2 ( <= ) ( <= )

let greater_than_eq num1 num2 =
  comp_helper num1 num2 ( >= ) ( >= )

let comp_of_ecomp (comp:ecomp) =
  match comp with
  | ELess      -> less_than
  | ELessEq    -> less_than_eq
  | EGreater   -> greater_than
  | EGreaterEq -> greater_than_eq

let opr_helper num1 num2 int_opr float_opr =
  match (num1, num2) with
  | (_, ELitNaN) | (ELitNaN, _) -> ELitNaN
  | (ELitInt   a, ELitInt   b)  -> ELitInt (int_opr a b)
  | (ELitFloat a, ELitFloat b)  -> ELitFloat (float_opr a b)
  | (ELitInt   a, ELitFloat b)  -> ELitFloat (float_opr (float_of_int a) b)
  | (ELitFloat a, ELitInt   b)  -> ELitFloat (float_opr a (float_of_int b))

let plus num1 num2 =
  opr_helper num1 num2 ( + ) ( +. )

let minus num1 num2 =
  opr_helper num1 num2 ( - ) ( -. )

let times num1 num2 =
  opr_helper num1 num2 ( * ) ( *. )

let division num1 num2 =
  match (num1, num2) with
  | (ELitInt 0, ELitInt 0)
  | (ELitFloat 0.0, ELitInt 0)
  | (ELitInt 0, ELitFloat 0.0)
  | (ELitFloat 0.0, ELitFloat 0.0) -> ELitNaN
  | (_, ELitInt 0) | (_, ELitFloat 0.0) ->
      raise (Invalid_expr "Divide by 0")
  | _ -> opr_helper num1 num2 ( / ) ( /. )

let modulo num1 num2 =
  let raise_exc dummy1 dummy2 =
    raise (Invalid_expr "Modulo not supported with floats")
  in
  opr_helper num1 num2 ( mod ) ( raise_exc )

let comp_of_ecomp (comp:ecomp) =
  match comp with
  | ELess      -> less_than
  | ELessEq    -> less_than_eq
  | EGreater   -> greater_than
  | EGreaterEq -> greater_than_eq

let op_of_eop (opr:eop) =
  match opr with
  | EPlus -> ( plus )
  | EMinus -> ( minus )
  | EMult -> ( times )
  | EDiv -> ( division )
  | EMod -> ( modulo )

let rec print_tokens token_list =
  match token_list with
  | [] -> ()
  | (t::ts) -> print_endline (string_of_token t); print_tokens ts

let rec computeAST_h tokenList =
  let splitList tokens =
    match tokens with
    | []      -> raise (Invalid_token
    "Expression not valid, probably missing parens")
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
          let result = ENumExpr (op, tree1, tree2) in
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
  | EInt   x   -> ((ENum (ELitInt x)), ts)
  | EFloat f   -> ((ENum (ELitFloat f)), ts)
  | ENaN       -> ((ENum ELitNaN), ts)
  | EBool  b   -> (EBool b, ts)
  | ELeftParen -> readOp ts
  | _          -> raise (Invalid_token "Expression not valid")

let rec evaluateAST tree:(ast) =
  match tree with
  | ENumExpr (op, tree1, tree2) ->
      let ev1 = evaluateAST tree1 in
      let ev2 = evaluateAST tree2 in
      (match (ev1, ev2) with
      | (ENum a, ENum b) -> ENum (op a b)
      | _ ->
          raise (Invalid_expr "Integer operation takes integers only")
      )
  | EComp (op, tree1, tree2) ->
      let ev1 = evaluateAST tree1 in
      let ev2 = evaluateAST tree2 in
      (match (ev1, ev2) with
      | (ENum a, ENum b) -> EBool (op a b)
      | _ ->
          raise (Invalid_expr "Comparison only takes integers")
      )
  | EIf (cond, tree1, tree2) ->
      let cond = evaluateAST cond  in
      (match cond with
      | EBool true  -> evaluateAST tree1
      | EBool false -> evaluateAST tree2
      | _ -> raise (Invalid_expr "\"if\" did not receive a boolean as condition")
      )
  | EBool _ as b   -> b
  | ENum  _ as num -> num

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
(*   | ENum (ELitInt a) -> printf "%s%d\n" indent a *)
(*   | ENum (ELitFloat a) -> printf "%s%f\n" indent a *)
(*   | ENumExpr (op, tree1, tree2) -> *)
(*       printf "%sExpr of:\n" indent; *)
(*       printAST_h tree1 (depth+1); *)
(*       printAST_h tree2 (depth+1); *)
(*       printf "%s:fo rpxE\n" indent *)

let printAST tree =
  print_endline "PrintAST not implemented yet"
