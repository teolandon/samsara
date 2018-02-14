exception Expr_error of string

let invalid_opr =
  Expr_error "Invalid operation, only accepts numbers"

let invalid_comp =
  Expr_error "Invalid comparison, only accepts numbers"

let div_by_zero =
  Expr_error "Divide by zero"

let modulo_error =
  Expr_error "Modulo operation only accepts integers"

type value =
  | EBool of bool
  | EInt of int
  | EFloat of float
  | ENaN

let opr_helper num1 num2 (int_opr:int->int->int) (float_opr:float->float->float) =
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

let addition num1 num2 =
  opr_helper num1 num2 ( + ) ( +. )

let subtraction num1 num2 =
  opr_helper num1 num2 ( - ) ( -. )

let multiplication num1 num2 =
  opr_helper num1 num2 ( * ) ( *. )

let division num1 num2 =
  match (num1, num2) with
  | (num1, num2) when (is_zero num1 && is_zero num2) -> ENaN
  | (num1, num2) when is_zero num2  -> raise div_by_zero
  | _ -> opr_helper num1 num2 ( / ) ( /. )

let modulo num1 num2 =
  match (num1, num2) with
  | (EInt a, EInt b) -> EInt (a mod b)
  | _                -> raise modulo_error

let comp_helper num1 num2 comp =
  let result =
    match (num1, num2) with
    | (EInt a, EInt b)      -> comp (float_of_int a) (float_of_int b)
    | (EFloat a, EInt b)    -> comp a (float_of_int b)
    | (EInt a, EFloat b)    -> comp (float_of_int a) b
    | (EFloat a, EFloat b)  -> comp a b
    | (ENaN, _) | (_, ENaN) -> false
    | _                     -> raise invalid_comp
  in EBool result

let less num1 num2 =
  comp_helper num1 num2 ( < )

let greater num1 num2 =
  comp_helper num1 num2 ( > )

let less_eq num1 num2 =
  comp_helper num1 num2 ( <= )

let greater_eq num1 num2 =
  comp_helper num1 num2 ( >= )

let string_of_value expr =
  match expr with
  | EInt a   -> string_of_int a
  | EFloat f -> string_of_float f
  | ENaN     -> "NaN"
  | EBool b  -> string_of_bool b
