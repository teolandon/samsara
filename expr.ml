exception Expr_error of string
exception Type_error of string

let invalid_opr =
  Expr_error "Invalid operation, only accepts numbers"

let invalid_comp =
  Expr_error "Invalid comparison, only accepts numbers"

let div_by_zero =
  Expr_error "Divide by zero"

let modulo_error =
  Expr_error "Modulo operation only accepts integers"

let if_type_mismatch =
  Type_error "Type mismatch, if accepts booleans as first argument"

let generic_type_err =
  Type_error "Type mismatch"

type typ =
  | TBool | TNum | TUnit | TPair of (typ * typ) | TChain of (typ * typ)
and opr =
  | EPlus | EMinus | EMult | EDiv | EMod
and  comp =
  | ELess | EGreater | ELessEq | EGreaterEq
and expr = [
  | `EUnit
  | `EBool  of bool
  | `EInt   of int
  | `EFloat of float
  | `ENaN
  | `EFun  of (typ * string * typ * expr)
  | `EFix  of (string * typ * string * typ * expr)
  | `EOpr  of (opr * expr * expr)
  | `EComp of (comp * expr * expr)
  | `EIf   of (expr * expr * expr)
  | `ELet  of (string * typ * expr * expr)
  | `EId   of string
  | `EAppl of (expr * expr)
  | `EPair of (expr * expr)
  | `EFst  of expr
  | `ESnd  of expr
]

let rec is_value (expr:expr) =
  match expr with
  | `EBool _ | `EInt _ | `EFloat _ | `ENaN | `EUnit
  | `EFun _ | `EFix _ -> true
  | `EPair (e1, e2) -> is_value e1 && is_value e2
  | _ -> false

let rec final_type typ =
  match typ with
  | TChain (_, rest) -> final_type rest
  | _               -> typ

type context = (string * typ) list

let get_type context id =
  List.assoc id context

let add_bind (context:context) (id:string) (typ:typ) =
  try
    ignore(get_type context id);
    let new_list = List.remove_assoc id context in
    (id, typ) :: new_list
  with
    Not_found -> (id, typ) :: context

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

let rec string_of_type typ =
  match typ with
  | TUnit -> "unit"
  | TBool -> "bool"
  | TNum  -> "num"
  | TPair  (t1, t2)  ->
      "(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ")"
  | TChain (t1, t2)  -> (string_of_type t1) ^ "->" ^ (string_of_type t2)

let rec string_of_value expr =
  match expr with
  | `EUnit -> "()"
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
  | `ELet (id, typ, value1, value2) ->
      Printf.sprintf "(let %s:%s = %s in %s)"
              id (string_of_type typ)
              (string_of_value value1)
              (string_of_value value2)
  | `EId id -> id
  | `EFun  (functype, id, vartype, expr) ->
      Printf.sprintf "(fun (%s:%s) : %s => %s)"
                     id (string_of_type vartype) (string_of_type functype)
                     (string_of_value expr)
  | `EFix  (name, functype, id, vartype, expr) ->
      Printf.sprintf "(fix %s (%s:%s) : %s => %s)"
                     name id (string_of_type vartype) (string_of_type functype)
                     (string_of_value expr)
  | `EAppl (value1, value2) ->
      "(" ^ (string_of_value value1) ^ " <- " ^ (string_of_value value2) ^ ")"
  | `EPair (value1, value2) ->
      "(" ^ (string_of_value value1) ^ ", " ^ (string_of_value value2) ^ ")"
  | `EFst expr -> "fst" ^ (string_of_value expr)
  | `ESnd expr -> "snd" ^ (string_of_value expr)

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
  | `ELet  (id, typ, expr1, expr2) when id <> str ->
      `ELet (id, typ, subst expr1, subst expr2)
  | `EFun  (functype, id, vartype, expr) when id <> str ->
      `EFun (functype, id, vartype, subst expr)
  | `EFix  (name, functype, id, vartype, expr) when id <> str && id <> name ->
      `EFix (name, functype, id, vartype, subst expr)
  | `EPair  (expr1, expr2) ->
      `EPair (subst expr1, subst expr2)
  | `EFst expr -> `EFst (subst expr)
  | `ESnd expr -> `ESnd (subst expr)
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
  | `ELet (id, _, expr1, expr2) -> subst expr1 id expr2
  | `EAppl (func, arg) when not (is_value func) ->
      `EAppl (step func, arg)
  | `EAppl (func, arg) ->
      (match func with
      | `EFun (functype, id, vartype, expr) -> subst arg id expr
      | `EFix (name, functype, id, vartype, expr) as fixed ->
          subst fixed name (subst arg id expr)
      | _ -> raise (Expr_error "LOL")
      )
  | `EPair (expr1, expr2) when not (is_value expr1) ->
      `EPair (step expr1, expr2)
  | `EPair (expr1, expr2) when not (is_value expr2) ->
      `EPair (expr1, step expr2)
  | `EFst expr when not (is_value expr) -> `EFst (step expr)
  | `EFst expr ->
      (match expr with
      | `EPair (expr1, expr2) -> expr1
      | _                     -> raise generic_type_err
      )
  | `ESnd expr when not (is_value expr) -> `ESnd (step expr)
  | `ESnd expr ->
      (match expr with
      | `EPair (expr1, expr2) -> expr2
      | _                     -> raise generic_type_err
      )
  | value as v -> v

let rec evaluate_value value =
  match value with
  | value when is_value value -> value
  | some_val -> evaluate_value (step some_val)

let rec evaluate_print_steps value =
  match value with
  | value when is_value value -> value
  | some_val ->
      let result = (step some_val) in
      print_endline (string_of_value value);
      evaluate_print_steps result

let rec typecheck context expr =
  match expr with
  | `EUnit -> TUnit
  | `EInt _ | `EFloat _ | `ENaN -> TNum
  | `EBool _                  -> TBool
  | `EOpr (opr, expr1, expr2) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      if (t1, t2) = (TNum, TNum) then
        TNum
      else
        raise generic_type_err
  | `EComp (comp, expr1, expr2) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      if (t1, t2) = (TNum, TNum) then
        TBool
      else
        raise generic_type_err
  | `EIf (expr1, expr2, expr3) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      let t3 = typecheck context expr3 in
      if t1 = TBool && t2 = t3 then
        t2
      else
        raise generic_type_err
  | `ELet (id, typ, expr1, expr2) ->
      let new_context = add_bind context id typ in
      if typ = (typecheck context expr1) then
        typecheck new_context expr2
      else
        raise generic_type_err
  | `EId id -> get_type context id
  | `EFun (functype, id, vartype, expr) ->
      let new_context = add_bind context id vartype in
      if functype = (typecheck new_context expr) then
        TChain (vartype, functype)
      else
        raise generic_type_err
  | `EFix  (name, functype, id, vartype, expr) ->
      let new_context = add_bind context id vartype in
      let new_context = add_bind new_context name functype in
      if functype = (typecheck new_context expr) then
        TChain (vartype, functype)
      else
        raise generic_type_err
  | `EAppl (expr1, expr2) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      (match t1 with
      | TChain (argtype, ret_type) ->
          if t2 = argtype then
            ret_type
          else
            raise generic_type_err
      | _ -> raise generic_type_err
      )
  | `EPair (expr1, expr2) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      TPair (t1, t2)
  | `EFst expr ->
      let t = typecheck context expr in
      (match t with
      | TPair (t1, t2) -> t1
      | _              -> raise generic_type_err
      )
  | `ESnd expr ->
      let t = typecheck context expr in
      (match t with
      | TPair (t1, t2) -> t2
      | _              -> raise generic_type_err
      )
