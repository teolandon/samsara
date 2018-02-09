open Printf

(* Exception types and some exceptions for them *)

exception Invalid_token of string
exception Invalid_expr  of string

let missing_paren_err =
  Invalid_expr "Expression not valid, probably missing parens"

let invalid_op_err =
  Invalid_token "Invalid operation after open paren"

let invalid_exp_err =
  Invalid_token "Invalid expression"

let num_op_err =
  Invalid_expr "Number operation takes numbers only"

let divide_by_zero_err =
  (Invalid_expr "Divide by 0")

let comp_err =
  Invalid_expr "Comparison only takes numbers"

let if_err =
  Invalid_expr "\"if\" did not receive a boolean as condition"

let trail_err =
  Invalid_token "Program not valid: trailing characters"

(* AST types *)

type number =
  | ELitInt   of int
  | ELitFloat of float
  | ELitNaN

type ast =
  | ENum     of number
  | EBool    of bool
  | EIfExpr  of (ast * ast * ast)
  | ENumExpr of ((number->number->number) * ast * ast)
  | ENumComp of ((number->number->bool)   * ast * ast)

(* Tokens to be created by lexing *)

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

type token =
  | ELeftParen | ERightParen
  | EIf
  | EOp    of eop
  | EComp  of ecomp
  | EBool  of bool
  | EInt   of int
  | EFloat of float
  | ENaN

(* Property calculators for tokens *)

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

(********************************)
(*****Operator returns for*******)
(******op and comp tokens********)
(********************************)

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
      raise divide_by_zero_err
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

(* End of comp returns *)

let rec print_tokens token_list =
  match token_list with
  | [] -> ()
  | (t::ts) -> print_endline (string_of_token t); print_tokens ts

let rec computeAST_h tokenList =
  let splitList tokens =
    match tokens with
    | []      -> raise missing_paren_err
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
          let result = ENumComp (comp, tree1, tree2) in
          (result, ts)
      | EIf ->
          let (cond, ts)  = computeAST_h ts in
          let (tree1, ts) = computeAST_h ts in
          let (tree2, ts) = computeAST_h ts in
          let result = EIfExpr (cond, tree1, tree2) in
          (result, ts)
      | _ -> raise invalid_op_err
    in
    match splitList remaining with
    | (ERightParen, rest) -> (result, rest)
    | _ -> raise missing_paren_err
  in
  let (t, ts) = splitList tokenList in
  match t with
  | EInt   x   -> ((ENum (ELitInt x)), ts)
  | EFloat f   -> ((ENum (ELitFloat f)), ts)
  | ENaN       -> ((ENum ELitNaN), ts)
  | EBool  b   -> (EBool b, ts)
  | ELeftParen -> readOp ts
  | _          -> raise invalid_exp_err

let computeAST tokenList =
  let (tree, remainingTokens) = computeAST_h tokenList in
  match remainingTokens with
  | [] -> tree
  | _  -> raise trail_err

let rec evaluateAST tree:(ast) =
  match tree with
  | ENumExpr (op, tree1, tree2) ->
      let ev1 = evaluateAST tree1 in
      let ev2 = evaluateAST tree2 in
      (match (ev1, ev2) with
      | (ENum a, ENum b) -> ENum (op a b)
      | _ ->
          raise num_op_err
      )
  | ENumComp (op, tree1, tree2) ->
      let ev1 = evaluateAST tree1 in
      let ev2 = evaluateAST tree2 in
      (match (ev1, ev2) with
      | (ENum a, ENum b) -> EBool (op a b)
      | _ ->
          raise comp_err
      )
  | EIfExpr (cond, tree1, tree2) ->
      let cond = evaluateAST cond  in
      (match cond with
      | EBool true  -> evaluateAST tree1
      | EBool false -> evaluateAST tree2
      | _ -> raise if_err
      )
  | EBool _ as b   -> b
  | ENum  _ as num -> num

let createAndEvaluate tokenList =
  let tree = computeAST tokenList in
  evaluateAST tree
