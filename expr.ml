(* Types of errors, Expr_error is a runtime error,
 * Type_error is a typecheck error
 *)
exception Expr_error of string
exception Type_error of string

(* Common runtime errors *)

let invalid_opr =
  Expr_error "Invalid operation, only accepts numbers"

let invalid_comp =
  Expr_error "Invalid comparison, only accepts numbers"

let div_by_zero =
  Expr_error "Divide by zero"

let modulo_error =
  Expr_error "Modulo operation only accepts integers"

(* Common typecheck errors *)

let if_type_mismatch =
  Type_error "Type mismatch, if accepts booleans as first argument"

let generic_type_err =
  Type_error "Type mismatch"

let pair_type_error func =
  Type_error (Printf.sprintf
    "Type mismatch in %s expression: Argument not a pair" func)

let list_type_error func =
  Type_error (Printf.sprintf
    "Type mismatch in %s expression: Argument not a list" func)

let opr_type_error opr =
  Type_error
    (Printf.sprintf "Operator %s only accepts numbers" opr)

let comp_type_error comp =
  Type_error
    (Printf.sprintf "Comparator %s only accepts numbers" comp)

let if_type_mismatch =
  Type_error
    "Type mismatch in if expression: The two cases do not have the same type"

let if_not_bool =
  Type_error
    "Type mismatch in if expression: Condition is not of boolean type"

let let_type_mismatch =
  Type_error
    "Type mismatch in let bind: Label and expressio not of the same type"

let fun_type_mismatch =
  Type_error
    ("Type mismatch in function declaration: " ^
    "Label and expression not of the same type")

let appl_type_mismatch =
  Type_error
    ("Type mismatch in function application: " ^
    "Function does not accept the given type of argument")

let appl_not_func =
  Type_error
    ("Type mismatch in function application: " ^
    "Expression treated as function but not of a function type")

let list_type_mismatch =
  Type_error
    ("Type mismatch in cons expression: " ^
    "Argument and list not of the same type")

let cons_not_list =
  Type_error
    ("Type mismatch in cons expression: " ^
    "Second argument not a list")

(* Samsara types *)
type typ =
  | TBool | TNum | TUnit
  | TList  of typ         (* List are homogenous *)
  | TPair  of (typ * typ) (* Pairs of two types  *)
  | TChain of (typ * typ) (* A TChain (t1, t2) means t1->t2, a function
                           * that takes type t1 and returns a type t2 *)

(* Helper types for expressions *)
type opr =
  | EPlus | EMinus | EMult | EDiv | EMod
type  comp =
  | ELess | EGreater | ELessEq | EGreaterEq

(* Samsara Expressions that form an AST *)
type expr = [
  | `EUnit
  | `EBool  of bool
  | `EInt   of int
  | `EFloat of float
  | `ENaN
  | `EFun   of (typ * string * typ * expr)
  | `EFix   of (string * typ * string * typ * expr)
  | `EOpr   of (opr * expr * expr)
  | `EComp  of (comp * expr * expr)
  | `EIf    of (expr * expr * expr)
  | `ELet   of (string * typ * expr * expr)
  | `EId    of string
  | `EAppl  of (expr * expr)
  | `EPair  of (expr * expr)
  | `EFst   of expr
  | `ESnd   of expr
  | `ENewList of typ
  | `ECons  of (expr * expr)
  | `EHead  of expr
  | `ETail  of expr
  | `EEmpty of expr
]

(* is_value specifies what expressions are values that
 * cannot be evaluated into something simpler
 *)
let rec is_value (expr:expr) =
  match expr with
  | `EBool _ | `EInt _ | `EFloat _ | `ENaN | `EUnit
  | `EFun _ | `EFix _ -> true
  | `EPair (e1, e2) -> is_value e1 && is_value e2
  | `ENewList _ -> true
  | `ECons  (e1, e2) -> is_value e1 && is_value e2
  | _ -> false

(* Context type that stores an association list for
 * a typechecking context, meaning bindings of labels
 * to types
 *)
type context = (string * typ) list

(* get_type context id looks up the label id in the
 * association list context, and returns its type
 *)
let get_type context id =
  List.assoc id context

(* add_bind context id typ adds the association pair
 * (id, typ) in the context given, and replaces id's
 * previous type association, if it exists
 *)
let add_bind (context:context) (id:string) (typ:typ) =
  try
    ignore(get_type context id);
    let new_list = List.remove_assoc id context in
    (id, typ) :: new_list
  with
    Not_found -> (id, typ) :: context

(* Operators and Comparatos *)

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
  | TList t -> "[" ^ (string_of_type t) ^ "]"

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
  | `EFst expr -> "(fst <- " ^ (string_of_value expr) ^ ")"
  | `ESnd expr -> "(snd <- " ^ (string_of_value expr) ^ ")"
  | `ENewList typ -> "[]:" ^ (string_of_type typ)
  | `ECons (expr1, expr2) ->
      "(" ^ (string_of_value expr1) ^ "::" ^ (string_of_value expr2) ^ ")"
  | `EHead expr -> "(hd <- " ^ (string_of_value expr) ^ ")"
  | `ETail expr -> "(tl <- " ^ (string_of_value expr) ^ ")"
  | `EEmpty expr -> "(empty <- " ^ (string_of_value expr) ^ ")"

(* subst value str expr substitutes any instances of the label
 * str with the value value, in the expression expr recursively
 * to cover the whole AST of the expression expr.
 *
 * If the expression expr is a let bind or a function declaration
 * that binds the label str, replacement does not advance, to
 * keep the concept of scopes.
 *)
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
  | `EHead expr -> `EHead (subst expr)
  | `ETail expr -> `ETail (subst expr)
  | `ECons (e1, e2) -> `ECons (subst e1, subst e2)
  | `EEmpty e -> `EEmpty (subst e)
  | _ -> expr

(* step expr evaluates the expression expr using small-step semantics.
 * Any expression expr will be simplified one step. If expr is a value,
 * then expr is returned as-is.
 *)
let rec step (expr:expr) =
  match expr with
  | value when is_value value -> value
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
      | _ -> raise (Expr_error "Function application needs a function")
      )
  | `EPair (expr1, expr2) when not (is_value expr1) ->
      `EPair (step expr1, expr2)
  | `EPair (expr1, expr2) when not (is_value expr2) ->
      `EPair (expr1, step expr2)
  | `EFst expr when not (is_value expr) -> `EFst (step expr)
  | `EFst expr ->
      (match expr with
      | `EPair (expr1, expr2) -> expr1
      | _                     -> raise (pair_type_error "fst")
      )
  | `ESnd expr when not (is_value expr) -> `ESnd (step expr)
  | `ESnd expr ->
      (match expr with
      | `EPair (expr1, expr2) -> expr2
      | _                     -> raise (pair_type_error "snd")
      )
  | `EHead expr when not (is_value expr) -> `EHead (step expr)
  | `EHead expr ->
      (match expr with
      | `ECons (expr1, expr2) -> expr1
      | _                  -> raise (list_type_error "hd")
      )
  | `ETail expr when not (is_value expr) -> `ETail (step expr)
  | `ETail expr ->
      (match expr with
      | `ECons (expr1, expr2) -> expr2
      | _                  -> raise (list_type_error "tl")
      )
  | `ECons (expr1, expr2) when not (is_value expr1) ->
      `ECons (step expr1, expr2)
  | `ECons (expr1, expr2) when not (is_value expr2) ->
      `ECons (expr1, step expr2)
  | `EEmpty expr when not (is_value expr) -> `EEmpty (step expr)
  | `EEmpty expr ->
      (match expr with
      | `ENewList _ -> `EBool true
      | _           -> `EBool false
      )
  | _ -> raise generic_type_err

(* evaluate_value expr calls step on expr repeatedly until
 * the result is a value, when it returns the fully evaluated form
 * of expr.
 *)
let rec evaluate_value expr =
  match expr with
  | value when is_value value -> value
  | some_val -> evaluate_value (step some_val)

(* Same as evaluate_value, but prints out each step. *)
let rec evaluate_print_steps value =
  match value with
  | value when is_value value -> value
  | some_val ->
      let result = (step some_val) in
      print_endline (string_of_value value);
      evaluate_print_steps result

(* typecheck context expr typechecks the expression expr
 * with the type context context.
 *)
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
        raise (opr_type_error (string_of_opr opr))
  | `EComp (comp, expr1, expr2) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      if (t1, t2) = (TNum, TNum) then
        TBool
      else
        raise (comp_type_error (string_of_comp comp))
  | `EIf (expr1, expr2, expr3) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      let t3 = typecheck context expr3 in
      if t1 = TBool then
        if t2 = t3 then
          t2
        else
          raise if_type_mismatch
      else
        raise if_not_bool
  | `ELet (id, typ, expr1, expr2) ->
      let new_context = add_bind context id typ in
      if typ = (typecheck context expr1) then
        typecheck new_context expr2
      else
        raise let_type_mismatch
  | `EId id -> get_type context id
  | `EFun (functype, id, vartype, expr) ->
      let new_context = add_bind context id vartype in
      if functype = (typecheck new_context expr) then
        TChain (vartype, functype)
      else
        raise fun_type_mismatch
  | `EFix  (name, functype, id, vartype, expr) ->
      let new_context = add_bind context id vartype in
      let new_context =
        add_bind new_context name (TChain (vartype, functype))
      in
      if functype = (typecheck new_context expr) then
        TChain (vartype, functype)
      else
        raise fun_type_mismatch
  | `EAppl (expr1, expr2) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      (match t1 with
      | TChain (argtype, ret_type) ->
          if t2 = argtype then
            ret_type
          else
            raise appl_type_mismatch
      | _ -> raise appl_not_func
      )
  | `EPair (expr1, expr2) ->
      let t1 = typecheck context expr1 in
      let t2 = typecheck context expr2 in
      TPair (t1, t2)
  | `EFst expr ->
      let t = typecheck context expr in
      (match t with
      | TPair (t1, t2) -> t1
      | _              -> raise (pair_type_error "fst")
      )
  | `ESnd expr ->
      let t = typecheck context expr in
      (match t with
      | TPair (t1, t2) -> t2
      | _              -> raise (pair_type_error "snd")
      )
  | `ENewList typ -> TList typ
  | `ECons (expr1, expr2) ->
      (match (typecheck context expr2) with
      | TList t as list_type ->
          if (typecheck context expr1) = t then
            list_type
          else
            raise list_type_mismatch
      | _ -> raise cons_not_list
      )
  | `EHead expr ->
      (match typecheck context expr with
      | TList typ -> typ
      | _         -> raise (list_type_error "hd")
      )
  | `ETail expr ->
      (match typecheck context expr with
      | TList typ as list_type -> list_type
      | _                      -> raise (list_type_error "tl")
      )
  | `EEmpty expr -> TBool
