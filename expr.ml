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
and expr = [
  | `EBool  of bool
  | `EInt   of int
  | `EFloat of float
  | `ENaN
  | `EFun  of (string * expr)
  | `EFix  of (string * string * expr)
  | `EOpr  of (opr * expr * expr)
  | `EComp of (comp * expr * expr)
  | `EIf   of (expr * expr * expr)
  | `ELet  of (string * expr * expr)
  | `EId   of string
  | `EAppl of (expr * expr)
]

let is_value (expr:expr) =
  match expr with
  | `EBool _ | `EInt _ | `EFloat _ | `ENaN
  | `EFun _ | `EFix _ -> true
  | _ -> false

let opr_helper (int_opr:int->int->int) (float_opr:float->float->float) num1 num2 =
  match (num1, num2) with
  | (`EInt   a, `EInt b)   -> `EInt   (int_opr a b)
  | (`EFloat a, `EInt b)   -> `EFloat (float_opr a (float_of_int b))
  | (`EInt a, `EFloat b)   -> `EFloat (float_opr (float_of_int a) b)
  | (`EFloat a, `EFloat b) -> `EFloat (float_opr a b)
  | (`ENaN, _) | (_, `ENaN) -> `ENaN
  | _                     -> raise invalid_opr

let is_zero n =
  match n with
  | `EInt 0 | `EFloat 0.0 -> true
  | _                   -> false

let fun_of_opr opr =
  match opr with
  | EPlus -> opr_helper ( + ) ( +. )
  | EMinus -> opr_helper ( - ) ( -. )
  | EMult -> opr_helper ( * ) ( *. )
  | EDiv -> fun num1 num2 ->
      (match (num1, num2) with
      | (num1, num2) when (is_zero num1 && is_zero num2) -> `ENaN
      | (num1, num2) when is_zero num2  -> raise div_by_zero
      | _ -> opr_helper ( / ) ( /. ) num1 num2
      )
  | EMod -> fun num1 num2 ->
      match (num1, num2) with
      | (`EInt a, `EInt b) -> `EInt (a mod b)
      | _                -> raise modulo_error

let comp_helper comp num1 num2 =
  let result =
    match (num1, num2) with
    | (`EInt a, `EInt b)      -> comp (float_of_int a) (float_of_int b)
    | (`EFloat a, `EInt b)    -> comp a (float_of_int b)
    | (`EInt a, `EFloat b)    -> comp (float_of_int a) b
    | (`EFloat a, `EFloat b)  -> comp a b
    | (`ENaN, _) | (_, `ENaN) -> false
    | _                     -> raise invalid_comp
  in `EBool result

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
  | `EInt a   -> string_of_int a
  | `EFloat f -> string_of_float f
  | `ENaN     -> "NaN"
  | `EBool b  -> string_of_bool b
  | `EOpr (opr, value1, value2) ->
      Printf.sprintf "(%s %s %s)"
              (string_of_opr opr)
              (string_of_value value1)
              (string_of_value value2)
  | `EComp (comp, value1, value2) ->
      Printf.sprintf "(%s %s %s)"
              (string_of_comp comp)
              (string_of_value value1)
              (string_of_value value2)
  | `EIf (value1, value2, value3) ->
      Printf.sprintf "(if %s then %s else %s)"
              (string_of_value value1)
              (string_of_value value2)
              (string_of_value value3)
  | `ELet (id, value1, value2) ->
      Printf.sprintf "(let %s = %s in %s)"
              id
              (string_of_value value1)
              (string_of_value value2)
  | `EId id -> id
  | `EFun  (id, expr) ->
      Printf.sprintf "(fun %s -> %s)" id (string_of_value expr)
  | `EFix  (name, id, expr) ->
      Printf.sprintf "(fix %s %s -> %s)" name id (string_of_value expr)
  | `EAppl (value1, value2) -> "(" ^ (string_of_value value1) ^ " <- " ^ (string_of_value value2) ^ ")"

let rec subst value str expr =
  let subst expr =
    subst value str expr
  in
  match expr with
  | `EId id when str = id -> value
  | `EOpr  (opr, expr1, expr2)   -> `EOpr (opr, subst expr1, subst expr2)
  | `EComp (comp, expr1, expr2)  -> `EComp (comp, subst expr1, subst expr2)
  | `EIf   (expr1, expr2, expr3) -> `EIf (subst expr1, subst expr2, subst expr3)
  | `EAppl (expr1, expr2)        -> `EAppl (subst expr1, subst expr2)
  | `ELet  (id, expr1, expr2) when id <> str ->
      `ELet (id, subst expr1, subst expr2)
  | `EFun  (id, expr) when id <> str ->
      `EFun (id, subst expr)
  | `EFix  (name, id, expr) when id <> str && id <> name ->
      `EFix (name, id, subst expr)
  | _ -> expr

let rec step (expr:expr) =
  match expr with
  | `EOpr (opr, expr1, expr2) when not (is_value expr1) ->
      `EOpr (opr, step expr1, expr2)
  | `EOpr (opr, expr1, expr2) when not (is_value expr2) ->
      `EOpr (opr, expr1, step expr2)
  | `EOpr (opr, expr1, expr2) ->
      let func = fun_of_opr opr in
      func expr1 expr2
  | `EComp (comp, expr1, expr2) when not (is_value expr1) ->
      `EComp (comp, step expr1, expr2)
  | `EComp (comp, expr1, expr2) when not (is_value expr2) ->
      `EComp (comp, expr1, step expr2)
  | `EComp (comp, expr1, expr2) ->
      let func = fun_of_comp comp in
      func expr1 expr2
  | `EIf (expr1, expr2, expr3) when not (is_value expr1) ->
      `EIf (step expr1, expr2, expr3)
  | `EIf (expr1, expr2, expr3) ->
      (match expr1 with
      | `EBool true -> expr2
      | `EBool false -> expr3
      | _           -> raise if_type_mismatch
      )
  | `ELet (id, value1, value2) ->
      subst value1 id value2
  | `EAppl (func, arg) when not (is_value func) ->
      `EAppl (step func, arg)
  | `EAppl (func, arg) ->
      (match func with
      | `EFun (id, expr) -> subst arg id expr
      | `EFix (name, id, expr) as fixed ->
          subst fixed name (subst arg id expr)
      | _ -> raise (Expr_error "LOL")
      )
  | value when is_value value -> value
  | _                         -> raise (Expr_error "Invalid type injection")

let rec evaluate_value value =
  match value with
  | value when is_value value -> value
  | some_val -> evaluate_value (step some_val)
