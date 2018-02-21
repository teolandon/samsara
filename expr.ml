exception Expr_error of string

let invalid_opr =
  Expr_error "Invalid operation, only accepts numbers"

let invalid_comp =
  Expr_error "Invalid comparison, only accepts numbers"

let div_by_zero =
  Expr_error "Divide by zero"

let if_type_mismatch =
  Expr_error "Type mismatch, if accepts booleans as first argument"

let modulo_error =
  Expr_error "Modulo operation only accepts integers"

type opr =
  | EPlus | EMinus | EMult | EDiv | EMod
and  comp =
  | ELess | EGreater | ELessEq | EGreaterEq
and value =
  | EBool  of bool
  | EInt   of int
  | EFloat of float
  | ENaN
  | EOpr  of (opr * value * value)
  | EComp of (comp * value * value)
  | EIf   of (value * value * value)
  | ELet  of (string * value * value)
  | EId   of string
  | EFun  of (string * value)
  | EAppl of (value * value)
  | EFix  of (string * string * value)

let opr_helper (int_opr:int->int->int) (float_opr:float->float->float) num1 num2 =
  match (num1, num2) with
  | (EInt   a, EInt b)   -> EInt   (int_opr a b)
  | (EFloat a, EInt b)   -> EFloat (float_opr a (float_of_int b))
  | (EInt a, EFloat b)   -> EFloat (float_opr (float_of_int a) b)
  | (EFloat a, EFloat b) -> EFloat (float_opr a b)
  | (ENaN, _) | (_, ENaN) -> ENaN
  | _                     -> raise invalid_opr

let is_zero n =
  match n with
  | EInt 0 | EFloat 0.0 -> true
  | _                   -> false

let fun_of_opr opr =
  match opr with
  | EPlus -> opr_helper ( + ) ( +. )
  | EMinus -> opr_helper ( - ) ( -. )
  | EMult -> opr_helper ( * ) ( *. )
  | EDiv -> fun num1 num2 ->
      (match (num1, num2) with
      | (num1, num2) when (is_zero num1 && is_zero num2) -> ENaN
      | (num1, num2) when is_zero num2  -> raise div_by_zero
      | _ -> opr_helper ( / ) ( /. ) num1 num2
      )
  | EMod -> fun num1 num2 ->
      match (num1, num2) with
      | (EInt a, EInt b) -> EInt (a mod b)
      | _                -> raise modulo_error

let comp_helper comp num1 num2 =
  let result =
    match (num1, num2) with
    | (EInt a, EInt b)      -> comp (float_of_int a) (float_of_int b)
    | (EFloat a, EInt b)    -> comp a (float_of_int b)
    | (EInt a, EFloat b)    -> comp (float_of_int a) b
    | (EFloat a, EFloat b)  -> comp a b
    | (ENaN, _) | (_, ENaN) -> false
    | _                     -> raise invalid_comp
  in EBool result

let fun_of_comp comp =
  match comp with
  | ELess      -> comp_helper ( < )
  | EGreater   -> comp_helper ( > )
  | ELessEq    -> comp_helper ( <= )
  | EGreaterEq -> comp_helper ( >= )

let string_of_opr opr =
  match opr with
  | EPlus -> "+"
  | EMinus -> "-"
  | EMult -> "*"
  | EDiv -> "/"
  | EMod -> "%"

let string_of_comp comp =
  match comp with
  | ELess -> "<"
  | EGreater -> ">"
  | ELessEq -> "<="
  | EGreaterEq -> ">="

let rec string_of_value expr =
  match expr with
  | EInt a   -> string_of_int a
  | EFloat f -> string_of_float f
  | ENaN     -> "NaN"
  | EBool b  -> string_of_bool b
  | EOpr (opr, value1, value2) ->
      Printf.sprintf "(%s %s %s)"
              (string_of_opr opr)
              (string_of_value value1)
              (string_of_value value2)
  | EComp (comp, value1, value2) ->
      Printf.sprintf "(%s %s %s)"
              (string_of_comp comp)
              (string_of_value value1)
              (string_of_value value2)
  | EIf (value1, value2, value3) ->
      Printf.sprintf "(if %s then %s else %s)"
              (string_of_value value1)
              (string_of_value value2)
              (string_of_value value3)
  | ELet (id, value1, value2) ->
      Printf.sprintf "(let %s = %s in %s)"
              id
              (string_of_value value1)
              (string_of_value value2)
  | EId id -> id
  | EFun  (id, expr) ->
      Printf.sprintf "(fun %s -> %s)" id (string_of_value expr)
  | EFix  (name, id, expr) ->
      Printf.sprintf "(fix %s %s -> %s)" name id (string_of_value expr)
  | EAppl (value1, value2) -> "(" ^ (string_of_value value1) ^ " <- " ^ (string_of_value value2) ^ ")"

let rec subst value str expr =
  let subst expr =
    subst value str expr
  in
  match expr with
  | EId id when str = id -> value
  | EOpr  (opr, expr1, expr2)   -> EOpr (opr, subst expr1, subst expr2)
  | EComp (comp, expr1, expr2)  -> EComp (comp, subst expr1, subst expr2)
  | EIf   (expr1, expr2, expr3) -> EIf (subst expr1, subst expr2, subst expr3)
  | EAppl (expr1, expr2)        -> EAppl (subst expr1, subst expr2)
  | ELet  (id, expr1, expr2) when id <> str ->
      ELet (id, subst expr1, subst expr2)
  | EFun  (id, expr) when id <> str ->
      EFun (id, subst expr)
  | EFix  (name, id, expr) when id <> str && id <> name ->
      EFix (name, id, subst expr)
  | _ -> expr

let rec evaluate_value value =
  match value with
  | EOpr (opr, value1, value2) ->
      let func = fun_of_opr opr in
      let num1 = evaluate_value value1 in
      let num2 = evaluate_value value2 in
      func num1 num2
  | EComp (comp, value1, value2) ->
      let func = fun_of_comp comp in
      let num1 = evaluate_value value1 in
      let num2 = evaluate_value value2 in
      func num1 num2
  | EIf (value1, value2, value3) ->
      let value1 = evaluate_value value1 in
      (match value1 with
      | EBool true -> evaluate_value value2
      | EBool false -> evaluate_value value3
      | _           -> raise if_type_mismatch
      )
  | ELet (id, value1, value2) ->
      evaluate_value (subst value1 id value2)
  | EAppl (func, arg) ->
      (match evaluate_value func with
      | EFun (id, expr) -> evaluate_value (subst arg id expr)
      | EFix (name, id, expr) as fixed ->
          let new_fix = subst fixed name (subst arg id expr) in
          evaluate_value new_fix
      | _ -> raise (Expr_error "LOL")
      )
  | some_val -> some_val
