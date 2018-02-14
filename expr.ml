exception LOL

type num = [
  | `Int of int
  | `Float of float
]

type value = [
  | num
  | `Bool of bool
]

let val_of_num (n:num):value =
  match n with
  | `Int a -> `Int a
  | `Float a -> `Float a

let val_of_bool (b:[`Bool of bool]):value =
  match b with
  | `Bool b -> `Bool b

type operation  = num -> num -> num
type comparison = num -> num -> [`Bool of bool]

let opr_helper (num1:num) (num2:num) (int_opr:int->int->int) (float_opr:float->float->float) =
  match (num1, num2) with
  | (`Int   a, `Int b)   -> `Int   (int_opr a b)
  | (`Float a, `Int b)   -> `Float (float_opr a (float_of_int b))
  | (`Int a, `Float b)   -> `Float (float_opr (float_of_int a) b)
  | (`Float a, `Float b) -> `Float (float_opr a b)

let is_zero (n:num) =
  match n with
  | `Int 0 | `Float 0.0 -> true
  | _                 -> false

let addition num1 num2 =
  opr_helper num1 num2 ( + ) ( +. )

let subtraction num1 num2 =
  opr_helper num1 num2 ( - ) ( -. )

let multiplication num1 num2 =
  opr_helper num1 num2 ( * ) ( *. )

let division (num1:num) (num2:num):num =
  match (num1, num2) with
  | (num1, num2) when (is_zero num1 || is_zero num2) -> raise LOL
  | _ -> opr_helper num1 num2 ( / ) ( /. )

let modulo (num1:num) (num2:num):num =
  match (num1, num2) with
  | (`Int a, `Int b) -> `Int (a mod b)
  | _ -> raise LOL

let comp_helper num1 num2 comp : [`Bool of bool] =
  let result =
    match (num1, num2) with
    | (`Int a, `Int b)     -> comp (float_of_int a) (float_of_int b)
    | (`Float a, `Int b)   -> comp a (float_of_int b)
    | (`Int a, `Float b)   -> comp (float_of_int a) b
    | (`Float a, `Float b) -> comp a b
  in `Bool result

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
  | `Int a   -> string_of_int a
  | `Float f -> string_of_float f
  | `Bool b  -> string_of_bool b
