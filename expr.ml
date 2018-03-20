(* used for generic printing *)
let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

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

let let_type_mismatch e1 e2 t1 t2 =
  Type_error
    ("Type mismatch in let bind: Label and expression not of the same type" ^
    "\n" ^ e1 ^ " -- " ^ e2 ^
    "\n" ^ t1 ^ " -- " ^ t2)

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
  | TRef   of typ         (* Reference to a variable *)
  | TArray of typ         (* Reference to a C-like array *)
  | TList  of typ         (* List are homogenous *)
  | TPair  of (typ * typ) (* Pairs of two types  *)
  | TChain of (typ * typ) (* A TChain (t1, t2) means t1->t2, a function
                           * that takes type t1 and returns a type t2 *)
  | TGeneric of int       (* Temporary types, to be replaced during type *)
  | TInfer                (* inference *)

(* Error to be raised when merging of a type and a constraint is not
 * possible
 *)
exception Merge_error of (typ * typ)

(* Helper types for expressions *)
type opr =
  | EPlus | EMinus | EMult | EDiv | EMod
type  comp =
  | ELess | EGreater | ELessEq | EGreaterEq

(* Samsara Expressions that form an AST *)
type expr =
  | EUnit
  | EBool  of bool
  | EInt   of int
  | EFloat of float
  | ENaN
  | EFun   of (typ * string * typ * expr)
  | EFix   of (string * typ * string * typ * expr)
  | EOpr   of (opr * expr * expr)
  | EComp  of (comp * expr * expr)
  | EIf    of (expr * expr * expr)
  | ELet   of (string * typ * expr * expr)
  | EId    of string
  | EAppl  of (expr * expr)
  | EPair  of (expr * expr)
  | EFst   of expr
  | ESnd   of expr
  | ENewList  of typ
  | ECons     of (expr * expr)
  | EHead     of expr
  | ETail     of expr
  | EEmpty    of expr
  | ERef      of expr
  | EAssign   of (expr * expr)
  | EDeref    of expr
  | ESeq      of (expr * expr)
  | EWhile    of (expr * expr)
  | ENewArray of (typ * expr)
  | EArrayRef of (expr * expr)
  | ELength   of expr
  | EPrint    of expr

  | EPtr      of (typ * int)
  | EArrayPtr of (typ * int * int)

(* is_value specifies what expressions are values that
 * cannot be evaluated into something simpler
 *)
let rec is_value (expr:expr) =
  match expr with
  | EBool _ | EInt _ | EFloat _ | ENaN | EUnit
  | EFun _ | EFix _
  | EPtr _ | EArrayPtr _ -> true
  | EPair (e1, e2) -> is_value e1 && is_value e2
  | ENewList _ -> true
  | ECons  (e1, e2) -> is_value e1 && is_value e2
  | _ -> false

(* Operators and Comparatos *)

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

let rec string_of_type typ =
  match typ with
  | TUnit -> "unit"
  | TBool -> "bool"
  | TNum  -> "num"
  | TRef t   -> Printf.sprintf "<%s>" (string_of_type t)
  | TArray t -> Printf.sprintf "array<%s>" (string_of_type t)
  | TPair  (t1, t2)  ->
      "(" ^ (string_of_type t1) ^ " * " ^ (string_of_type t2) ^ ")"
  | TChain (t1, t2)  -> (string_of_type t1) ^ "->" ^ (string_of_type t2)
  | TList t -> "[" ^ (string_of_type t) ^ "]"
  | TGeneric i -> Printf.sprintf "`%c" (String.get alphabet i)
  | TInfer -> "inferred"

let rec string_of_expr expr =
  match expr with
  | EUnit -> "()"
  | EInt a   -> string_of_int a
  | EFloat f -> string_of_float f
  | ENaN     -> "NaN"
  | EBool b  -> string_of_bool b
  | EOpr (opr, value1, value2) ->
      Printf.sprintf "(%s %s %s)"
              (string_of_opr opr)
              (string_of_expr value1)
              (string_of_expr value2)
  | EComp (comp, value1, value2) ->
      Printf.sprintf "(%s %s %s)"
              (string_of_comp comp)
              (string_of_expr value1)
              (string_of_expr value2)
  | EIf (value1, value2, value3) ->
      Printf.sprintf "(if %s then %s else %s)"
              (string_of_expr value1)
              (string_of_expr value2)
              (string_of_expr value3)
  | ELet (id, typ, value1, value2) ->
      Printf.sprintf "(let %s:%s = %s in %s)"
              id (string_of_type typ)
              (string_of_expr value1)
              (string_of_expr value2)
  | EId id -> id
  | EFun  (functype, id, vartype, expr) ->
      Printf.sprintf "(fun (%s:%s) : %s => %s)"
                     id (string_of_type vartype) (string_of_type functype)
                     (string_of_expr expr)
  | EFix  (name, functype, id, vartype, expr) ->
      Printf.sprintf "(fix %s (%s:%s) : %s => %s)"
                     name id (string_of_type vartype) (string_of_type functype)
                     (string_of_expr expr)
  | EAppl (value1, value2) ->
      "(" ^ (string_of_expr value1) ^ " <- " ^ (string_of_expr value2) ^ ")"
  | EPair (value1, value2) ->
      "(" ^ (string_of_expr value1) ^ ", " ^ (string_of_expr value2) ^ ")"
  | EFst expr -> "(fst <- " ^ (string_of_expr expr) ^ ")"
  | ESnd expr -> "(snd <- " ^ (string_of_expr expr) ^ ")"
  | ENewList typ -> "[]:" ^ (string_of_type typ)
  | ECons (expr1, expr2) ->
      "(" ^ (string_of_expr expr1) ^ "::" ^ (string_of_expr expr2) ^ ")"
  | EHead expr -> "(hd <- " ^ (string_of_expr expr) ^ ")"
  | ETail expr -> "(tl <- " ^ (string_of_expr expr) ^ ")"
  | EEmpty expr -> "(empty <- " ^ (string_of_expr expr) ^ ")"
  | ERef expr -> Printf.sprintf "ref %s" (string_of_expr expr)
  | EAssign (e1, e2) ->
      Printf.sprintf "%s := %s" (string_of_expr e1) (string_of_expr e2)
  | EDeref expr -> Printf.sprintf "!%s" (string_of_expr expr)
  | ESeq (e1, e2) ->
      Printf.sprintf "%s; %s" (string_of_expr e1) (string_of_expr e2)
  | ENewArray (typ, cap) ->
      Printf.sprintf "new %s[%s]" (string_of_type typ) (string_of_expr cap)
  | EArrayRef (arr, index) ->
      Printf.sprintf "%s[%s]" (string_of_expr arr) (string_of_expr index)
  | EPtr (_, num) -> Printf.sprintf "Ptr(%08x)" num
  | EArrayPtr (t, addr, cap) ->
      Printf.sprintf "Arr(%08x - %08x)" addr (addr+cap)
  | EWhile (e1, e2) ->
      Printf.sprintf "while %s do %s end"
                     (string_of_expr e1) (string_of_expr e2)
  | ELength e -> Printf.sprintf "length <- %s" (string_of_expr e)
  | EPrint  e -> Printf.sprintf "print <- %s"  (string_of_expr e)

(* Environtment type as an assoc list of pointer
 * addresses, mapping to values.
 *)
type environment = (int * expr) list

let string_of_env env =
  let rec helper env =
    match env with
    | []     -> "]"
    | [(addr, expr)] ->
        Printf.sprintf "(%08x, %s)]" addr (string_of_expr expr)
    | (e::es) -> match e with (addr, expr) ->
        Printf.sprintf "(%08x, %s), %s" addr (string_of_expr expr) (helper es)
  in
  "[" ^ (helper env)

(* get_val env addr looks up the address addr in the
 * association list env, and returns its value
 *)
let get_val env addr =
  List.assoc addr env

let address_count =
  Random.self_init ();
  ref (Random.int 173741824)

let get_addr () =
  let ret = !address_count in
  address_count := ret + 1;
  ret

let assign env addr expr =
  if not (is_value expr) then
    raise (Expr_error "Cannot store reference to non-value. Check compiler")
  else
    try
      ignore(get_val env addr);
      let new_list = List.remove_assoc addr env in
      (addr, expr) :: new_list
    with
      Not_found -> (addr, expr) :: env

(* malloc env addr expr adds the association pair
 * (addr, expr) in the environment given, and replaces addr's
 * previous expression association, if it exists.
 *
 * The expression expr given has to be a value.
 *)
let malloc typ env expr =
  if not (is_value expr) then
    raise (Expr_error "Cannot store reference to non-value. Check compiler")
  else
    let addr = get_addr () in
    let new_env = assign env addr expr in
    (new_env, EPtr (typ, addr))

let malloc_array cap typ =
  let addr = !address_count in
  address_count := addr + cap;
  EArrayPtr (typ, addr, cap)

(* Context type that stores an association list for
 * a typechecking context, meaning bindings of labels
 * to types. It also has a generic count, to aid in the
 * generation of new Generic types.
 *)
type context = {
  ctx:           (string * typ) list;
  generic_count: int
}

let new_context () = {ctx = []; generic_count = 0}

(* Creates a new generic in a context, increases its generic count.
 * Returns the new generic type
 *)
let new_generic context =
  match context with {ctx; generic_count} ->
    ({context with generic_count = generic_count + 1}, TGeneric generic_count)

(* Returns a type old_t with any of its TGeneric i subtypes replaced
 * with the replacement type.
 *)
let rec replace_generic i replacement old_t =
  let replace_h =
    replace_generic i replacement
  in
  match old_t with
  | TGeneric index when i = index -> replacement
  | TRef   t -> TRef (replace_h t)
  | TArray t -> TArray (replace_h t)
  | TList  t -> TList (replace_h t)
  | TPair  (t1, t2) -> TPair (replace_h t1, replace_h t2)
  | TChain (t1, t2) -> TChain (replace_h t1, replace_h t2)
  | _ -> old_t

(* Replaces all instances of the TGeneric i in context with the
 * type typ
 *)
let assign_generic context i typ =
  match context with {ctx; generic_count} ->
    let new_ctx =
      List.map
      (fun (id, old_typ) -> (id, (replace_generic i typ old_typ)))
      ctx
    in
    {ctx=new_ctx; generic_count}

(* Merges the type t1 into t2 in the context context. The current constraint
 * rules only allow generics to be merged into other types, and nothing else.
 * Higher order types are also allowed to merge, as long as the types making
 * them also follow the constraint rules.
 *)
let rec type_merge context t1 t2 =
  try
    match (t1, t2) with
    | (TGeneric i, t_other) | (t_other, TGeneric i) ->
        (assign_generic context i t_other, t_other)
    | (TRef t1, TRef t2) ->
        let (new_context, return_type) = type_merge context t1 t2 in
        (new_context, TRef return_type)
    | (TArray t1, TArray t2) ->
        let (new_context, return_type) = type_merge context t1 t2 in
        (new_context, TArray return_type)
    | (TList t1, TList t2) ->
        let (new_context, return_type) = type_merge context t1 t2 in
        (new_context, TList return_type)
    | (TPair (t1, t2), TPair (t3, t4)) ->
        let (new_context, return_type1) = type_merge context     t1 t3 in
        let (new_context, return_type2) = type_merge new_context t2 t4 in
        (new_context, TPair (return_type1, return_type2))
    | (TChain (t1, t2), TChain (t3, t4)) ->
        let (new_context, return_type1) = type_merge context     t1 t3 in
        let (new_context, return_type2) = type_merge new_context t2 t4 in
        (new_context, TChain (return_type1, return_type2))
    | (t1, t2) when t1 = t2 -> (context, t2)
    | _                     -> raise (Merge_error (t1, t2))
  with Merge_error _ -> raise (Merge_error (t1, t2))

(* get_type context id looks up the label id in the
 * association list context, and returns its type
 *)
let get_type context id =
  match context with {ctx; _} ->
  List.assoc id ctx

(* add_bind context id typ adds the association pair
 * (id, typ) in the context given, and replaces id's
 * previous type association, if it exists
 *)
let add_bind (context:context) (id:string) (typ:typ) =
  match context with {ctx; generic_count} ->
  try
    ignore(get_type context id);
    let new_list = List.remove_assoc id ctx in
    {ctx = (id, typ) :: new_list; generic_count}
  with
    Not_found -> {ctx = (id, typ) :: ctx; generic_count}

(* merge_bind context id typ adds the association pair
 * (id, typ) in the context given, and merges id's
 * two types if a binding already exists
 *)
let merge_bind context id typ =
  let old_type = get_type context id in
  let (new_context, merged_type) = type_merge context old_type typ in
  new_context

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
  | EId id when str = id -> value
  | EOpr  (opr, expr1, expr2)   -> EOpr (opr, subst expr1, subst expr2)
  | EComp (comp, expr1, expr2)  -> EComp (comp, subst expr1, subst expr2)
  | EIf   (expr1, expr2, expr3) -> EIf (subst expr1, subst expr2, subst expr3)
  | EAppl (expr1, expr2)        -> EAppl (subst expr1, subst expr2)
  | ELet  (id, typ, expr1, expr2) when id <> str ->
      ELet (id, typ, subst expr1, subst expr2)
  | ELet  (id, typ, expr1, expr2) ->
      ELet (id, typ, subst expr1, expr2)
  | EFun  (functype, id, vartype, expr) when id <> str ->
      EFun (functype, id, vartype, subst expr)
  | EFix  (name, functype, id, vartype, expr) when id <> str && id <> name ->
      EFix (name, functype, id, vartype, subst expr)
  | EPair  (expr1, expr2) ->
      EPair (subst expr1, subst expr2)
  | EFst expr -> EFst (subst expr)
  | ESnd expr -> ESnd (subst expr)
  | EHead expr -> EHead (subst expr)
  | ETail expr -> ETail (subst expr)
  | ECons (e1, e2) -> ECons (subst e1, subst e2)
  | EEmpty e -> EEmpty (subst e)
  | ERef e -> ERef (subst e)
  | EAssign (e1, e2) -> EAssign (subst e1, subst e2)
  | EDeref e -> EDeref (subst e)
  | ESeq (e1, e2) -> ESeq (subst e1, subst e2)
  | ENewArray (typ, cap) -> ENewArray (typ, subst cap)
  | EArrayRef (arr, index) -> EArrayRef (subst arr, subst index)
  | EWhile (e1, e2) -> EWhile (subst e1, subst e2)
  | ELength e -> ELength (subst e)
  | EPrint  e -> EPrint  (subst e)
  | _ -> expr

(* typecheck_h context expr t_constraint typechecks the expression expr
 * with the type context context, and merges the found type with t_constraint.
 * To discover the type of an expression, pass an empty context, an expression
 * and a generic type as t_constraint.
 *
 * NOTE: In the future, all calls to typecheck_h (or tcheck_h) are going to
 * be wrappen in `try with` blocks to raise the correct error messages instead
 * of the lower-level Merge errors that arise due to the merging at the end of
 * this function.
 *)
let rec typecheck_h context expr t_constraint =
  let context_ref = ref context in (* helps keep return values clean *)

  (* The following are helper functions that automatically update
   * the context reference after running a function.
   *)
  let new_generic () =
    let (new_context, gen) = new_generic !context_ref in
    context_ref := new_context; gen
  in
  let merge_bind_h id typ =
    context_ref := merge_bind !context_ref id typ
  in
  let tcheck_h expr constr =
    match typecheck_h !context_ref expr constr with
    | (new_context, typ) -> context_ref := new_context; typ
  in

  (* this let-bind allows for the pattern matched expression
   * to simply return a type instead of returning the (context * type)
   * tuple everywhere. Not as functional, but seems more convenient to
   * me
   *)
  let return_type =
    match expr with
    | EUnit  -> TUnit
    | EInt _ | EFloat _ | ENaN -> TNum
    | EBool _                  -> TBool

    | EOpr (opr, expr1, expr2) ->
        (try
          (* These tcheck_h calls can be ignored, they're only used
           * to check the expressions against the TNum constraint,
           * an operation will always return a value of type TNum
           *)
          ignore(tcheck_h expr1 TNum);
          ignore(tcheck_h expr2 TNum);
          TNum
        with
          Merge_error _ -> raise (opr_type_error (string_of_opr opr))
        )
    | EComp (comp, expr1, expr2) ->
        (try
          ignore(tcheck_h expr1 TNum);
          ignore(tcheck_h expr2 TNum);
          TBool
        with
          Merge_error _ -> raise (comp_type_error (string_of_comp comp))
        )

    | EIf (expr1, expr2, expr3) ->
        begin try
          ignore(tcheck_h expr1 TBool);
        with
          Merge_error _ -> raise if_not_bool
        end;
        (* Here, tcheck_h's return type is used to be later
         * checked against the type of expr3, as a constraint
         *)
        let t2 = tcheck_h expr2 t_constraint in
        (* begin try *)
          tcheck_h expr3 t2
         (* with Merge_error _ -> raise if_type_mismatch *)
        (* end *)

    | ELet (id, typ, expr1, expr2) ->
        let t1 =
          (* try *)
            tcheck_h expr1 typ
          (* with Merge_error (t1, t2) -> *)
          (*   raise (let_type_mismatch *)
          (*         (string_of_value expr1) (string_of_value expr2) *)
          (*         (string_of_type t1) (string_of_type t2)) *)
        in
        (* the inferred type of expr1 is assigned to the id given *)
        let new_context = add_bind !context_ref id t1 in
        begin
          (* the new context is used in typechecking expr2, since the
           * binding of id will be used in there *)
          match typecheck_h new_context expr2 t_constraint with (_, t) -> t
          (* the new context, however, is not applied to the context ref,
           * since the id is bound to the particular type only in this
           * let's scope
           *)
        end

    | EId id ->
        (* If an ID is typechecked against a constraint, it attempts to
         * merge the ID's current type with the constraint, so as to allow
         * inferred ID types to be merged into whatever constraint they need
         * to fill. If this fails, an error is thrown
         *)
        let t = get_type !context_ref id in
        let (new_context, merged) = type_merge !context_ref t_constraint t in
        context_ref := new_context;
        merge_bind_h id merged; merged

    | EFun (functype, id, vartype, expr) ->
        (* binds id to vartype in a new context, and typechecks the
         * expression given against that context, since id will be used
         * as a variable
         *)
        let new_context = !context_ref in
        let new_context = add_bind new_context id vartype in
        let (new_context, t_func) =
          typecheck_h new_context expr functype
        in
        (* After typechecking the function body, we inquire for
         * the function argument's type in the context returned,
         * since it might have been inferred somewhere in the body's
         * typechecking process
         *)
        let final_vartype = get_type new_context id in
        (* finally, the id is removed from the context, so as to prevent
         * any nested function declarations with the same ids for arguments
         * from mixing the inferred types
         *)
        let new_context =
          { new_context with
              ctx = List.remove_assoc id new_context.ctx
          }
        in
        context_ref := new_context;
        TChain (final_vartype, t_func)
    | EFix (name, functype, id, vartype, expr) ->
        let new_context = !context_ref in
        let new_context = add_bind new_context id vartype in
        let new_context =
          add_bind new_context name (TChain (vartype, functype))
        in
        let (new_context, t_func) =
          typecheck_h new_context expr functype
        in
        let final_vartype = get_type new_context id in
        (* Same procedure as in EFun, but with an added id to be removed *)
        let context_list = new_context.ctx in
        let context_list = List.remove_assoc id context_list in
        let context_list = List.remove_assoc name context_list in
        let new_context = { new_context with ctx = context_list } in
        context_ref := new_context;
        TChain (final_vartype, t_func)

    | EAppl (expr1, expr2) ->
        (* Another way of writing this rule would be to produce a
         * single generic and pass it as a constraint to expr2, and
         * as a part of an TChain for a constraint in expr1
         *)
        let gen = new_generic () in
        (match gen with
        | _ -> ()
        );
        let argtype = tcheck_h expr2 gen in
        let functype = tcheck_h expr1 (TChain (argtype, t_constraint)) in
        (match functype with
        | TChain (_, ret_type) -> ret_type
        | _ -> assert false (* Should never happen, error would be thrown *)
        )

    | EPair (expr1, expr2) ->
        let t1 = tcheck_h expr1 (new_generic ()) in
        let t2 = tcheck_h expr2 (new_generic ()) in
        TPair (t1, t2)

    | EFst expr ->
        let t = tcheck_h expr (TPair (t_constraint, new_generic ())) in
        (match t with
        | TPair (t1, t2) -> t1
        | _              -> raise (pair_type_error "fst")
        )
    | ESnd expr ->
        let t = tcheck_h expr (TPair (new_generic (), t_constraint)) in
        (match t with
        | TPair (t1, t2) -> t2
        | _              -> raise (pair_type_error "snd")
        )

    | ENewList typ -> TList typ
    | ECons (expr1, expr2) ->
        (* Since a cons will result in a list of the same type as expr2,
         * expr2 is typechecked against t_constraint, and if it is actually
         * a list of type t, expr1 is typechecked against the constraint t
         *)
        let t = tcheck_h expr1 (new_generic ()) in
        tcheck_h expr2 (TList t)

    | EHead expr ->
        (match tcheck_h expr (TList t_constraint) with
        | TList typ -> typ
        | _         -> raise (list_type_error "hd")
        )
    | ETail expr ->
        tcheck_h expr t_constraint

    | EEmpty expr ->
        ignore(tcheck_h expr (TList (new_generic ())));
        TBool
    | ERef expr ->
        TRef (tcheck_h expr (new_generic ()))

    | EAssign (expr1, expr2) ->
        let t2 = tcheck_h expr2 (new_generic ()) in
        ignore(tcheck_h expr1 (TRef t2));
        TUnit

    | EDeref expr ->
        (match tcheck_h expr (TRef (t_constraint)) with
        | TRef finaltype -> finaltype
        | _ -> assert false
        )

    | ESeq (expr1, expr2) ->
        ignore(tcheck_h expr1 (new_generic ()));
        tcheck_h expr2 t_constraint

    | EWhile (expr1, expr2) ->
        ignore(tcheck_h expr1 TBool); TUnit
    | ENewArray (typ, cap) -> ignore(tcheck_h cap TNum); TArray typ

    | EArrayRef (arr, index) ->
        ignore(tcheck_h index TNum);
        let t = tcheck_h arr (TArray (new_generic ())) in
        begin match t with (* only matching to extract inner type *)
        | TArray t -> TRef t
        | _ -> assert false
        end
        (* Now that I think about it, constraints could be represented
         * with polymorphic types, and that would make pattern matching
         * this unnecessary *)

    | ELength arr ->
        ignore(tcheck_h arr (TArray (new_generic ())));
        TNum

    | EPrint arr ->
        ignore(tcheck_h arr (new_generic ()));
        TNum

    | EArrayPtr (t, _, _) -> TArray t
    | EPtr (t, _) -> TRef t
  in
  type_merge !context_ref return_type t_constraint

(* Converts all TInfer's to TGenerics and populates a context with
 * the produced generics
 *)
let create_generics context expr =
  let context_ref = ref context in
  let rec loop expr =
    let new_generic () =
      match new_generic !context_ref with (new_context, generic) ->
        context_ref := new_context; generic
    in
    match expr with
      | ELet (id, typ, expr1, expr2) when typ = TInfer ->
          let gen = new_generic () in ELet (id, gen, loop expr1, loop expr2)
      | EFun (functype, id, vartype, expr) when functype = TInfer ->
          let gen = new_generic () in loop (EFun (gen, id, vartype, expr))
      | EFun (functype, id, vartype, expr) when vartype = TInfer ->
          let gen = new_generic () in loop (EFun (functype, id, gen, expr))
      | EFix (name, functype, id, vartype, expr) when functype = TInfer ->
          let gen = new_generic () in loop (EFix (name, gen, id, vartype, expr))
      | EFix (name, functype, id, vartype, expr) when vartype = TInfer ->
          let gen = new_generic () in loop (EFix (name, functype, id, gen, expr))
      | ENewList typ when typ = TInfer ->
          let gen = new_generic () in ENewList gen
      | ENewArray (typ, cap) when typ = TInfer ->
          let gen = new_generic () in ENewArray (gen, loop cap)

      | ELet (id, typ, expr1, expr2) -> ELet (id, typ, loop expr1, loop expr2)
      | EFun (functype, id, vartype, expr) ->
          EFun (functype, id, vartype, loop expr)
      | EFix (name, functype, id, vartype, expr) ->
          EFix (name, functype, id, vartype, loop expr)
      | ENewArray (typ, cap) -> ENewArray (typ, loop cap)
      | EOpr (opr, e1, e2)   -> EOpr (opr, loop e1, loop e2)
      | EComp (comp, e1, e2) -> EComp (comp, loop e1, loop e2)
      | EIf (e1, e2, e3)     -> EIf (loop e1, loop e2, loop e3)
      | EAppl (e1, e2) -> EAppl (loop e1, loop e2)
      | EPair (e1, e2) -> EPair (loop e1, loop e2)
      | EFst expr -> EFst (loop expr)
      | ESnd expr -> ESnd (loop expr)
      | ECons (e1, e2) -> ECons (loop e1, loop e2)
      | EHead expr -> EHead (loop expr)
      | ETail expr -> ETail (loop expr)
      | EEmpty expr -> EEmpty (loop expr)
      | ERef expr -> ERef (loop expr)
      | EAssign (e1, e2) -> EAssign (loop e1, loop e2)
      | EDeref expr -> EDeref (loop expr)
      | ESeq (e1, e2) -> ESeq (loop e1, loop e2)
      | EWhile (e1, e2) -> EWhile (loop e1, loop e2)
      | EArrayRef (e1, e2) -> EArrayRef (loop e1, loop e2)
      | ELength expr -> ELength (loop expr)
      | EPrint  expr -> EPrint  (loop expr)
      | _ -> expr
  in
  let ret = loop expr in
  (!context_ref, ret)

(* Front-face for typecheck_h *)
let typecheck expr =
  let context = new_context () in
  let (context, expr) = create_generics context expr in
  let (context, gen) = new_generic context in
  match typecheck_h context expr gen with (final_context, typ) -> typ

(* step expr evaluates the expression expr using small-step semantics.
 * Any expression expr will be simplified one step. If expr is a value,
 * then expr is returned as-is.
 *)
let rec step (env:environment) (expr:expr) =
  let env_ref = ref env in
  let step_h expr =
    let (new_env, stepped) = step !env_ref expr in
    env_ref := new_env; stepped
  in
  let stepped =
    match expr with
    | value when is_value value -> value
    | EOpr (opr, expr1, expr2) when not (is_value expr1) ->
        EOpr (opr, step_h expr1, expr2)
    | EOpr (opr, expr1, expr2) when not (is_value expr2) ->
        EOpr (opr, expr1, step_h expr2)
    | EOpr (opr, expr1, expr2) ->
        let func = fun_of_opr opr in
        func expr1 expr2
    | EComp (comp, expr1, expr2) when not (is_value expr1) ->
        EComp (comp, step_h expr1, expr2)
    | EComp (comp, expr1, expr2) when not (is_value expr2) ->
        EComp (comp, expr1, step_h expr2)
    | EComp (comp, expr1, expr2) ->
        let func = fun_of_comp comp in
        func expr1 expr2
    | EIf (expr1, expr2, expr3) when not (is_value expr1) ->
        EIf (step_h expr1, expr2, expr3)
    | EIf (expr1, expr2, expr3) ->
        (match expr1 with
        | EBool true -> expr2
        | EBool false -> expr3
        | _           -> raise if_type_mismatch
        )
    | ELet (id, t, expr1, expr2) when not (is_value expr1) ->
        ELet (id, t, step_h expr1, expr2)
    | ELet (id, _, expr1, expr2) ->
        subst expr1 id expr2
    | EAppl (func, arg) when not (is_value func) ->
        EAppl (step_h func, arg)
    | EAppl (func, arg) ->
        (match func with
        | EFun (functype, id, vartype, expr) -> subst arg id expr
        | EFix (name, functype, id, vartype, expr) as fixed ->
            subst fixed name (subst arg id expr)
        | _ -> raise (Expr_error "Function application needs a function")
        )
    | EPair (expr1, expr2) when not (is_value expr1) ->
        EPair (step_h expr1, expr2)
    | EPair (expr1, expr2) when not (is_value expr2) ->
        EPair (expr1, step_h expr2)
    | EFst expr when not (is_value expr) -> EFst (step_h expr)
    | EFst expr ->
        (match expr with
        | EPair (expr1, expr2) -> expr1
        | _                     -> raise (pair_type_error "fst")
        )
    | ESnd expr when not (is_value expr) -> ESnd (step_h expr)
    | ESnd expr ->
        (match expr with
        | EPair (expr1, expr2) -> expr2
        | _                     -> raise (pair_type_error "snd")
        )
    | EHead expr when not (is_value expr) -> EHead (step_h expr)
    | EHead expr ->
        (match expr with
        | ECons (expr1, expr2) -> expr1
        | _                  -> raise (list_type_error "hd")
        )
    | ETail expr when not (is_value expr) -> ETail (step_h expr)
    | ETail expr ->
        (match expr with
        | ECons (expr1, expr2) -> expr2
        | _                  -> raise (list_type_error "tl")
        )
    | ECons (expr1, expr2) when not (is_value expr1) ->
        ECons (step_h expr1, expr2)
    | ECons (expr1, expr2) when not (is_value expr2) ->
        ECons (expr1, step_h expr2)
    | EEmpty expr when not (is_value expr) -> EEmpty (step_h expr)
    | EEmpty expr ->
        (match expr with
        | ENewList _ -> EBool true
        | _           -> EBool false
        )
    | ERef expr when not (is_value expr) ->
        ERef (step_h expr)
    | ERef expr ->
        let (new_env, ptr) = malloc (typecheck expr) !env_ref expr in
        env_ref := new_env; ptr
    | EAssign (ref, expr) ->
        (match (ref, expr) with
        | (r, e) when not (is_value e) -> EAssign (r, step_h e)
        | (EArrayRef (arr, addr), _) when not (is_value arr) ->
          EAssign (EArrayRef (step_h arr, addr), expr)
        | (EArrayRef (arr, addr), _) when not (is_value addr) ->
          EAssign (EArrayRef (arr, step_h addr), expr)
        | (EArrayRef (EArrayPtr (_, addr, cap), EInt i), e) ->
            if i < cap then
              (env_ref := assign !env_ref (addr + i) expr; EUnit)
            else
              raise (Expr_error "Array index out of bounds")
        | (EPtr (_, addr), _) -> env_ref := assign !env_ref addr expr; EUnit
        | (r, e) when not (is_value r) -> EAssign (step_h ref, expr)
        | _ -> raise generic_type_err
        )
    | EDeref ptr ->
        (match ptr with
        | EPtr (_, addr) -> get_val !env_ref addr
        | EArrayPtr (_, addr, cap) -> get_val !env_ref addr
        | ref_expr when not (is_value ref_expr) -> EDeref (step_h ptr)
        | _ -> raise generic_type_err
        )
    | ESeq (expr1, expr2) when not (is_value expr1) ->
        ESeq (step_h expr1, expr2)
    | ESeq (expr1, expr2) -> expr2
    | ENewArray (typ, cap) when not (is_value cap) ->
        ENewArray (typ, step_h cap)
    | ENewArray (typ, cap) ->
        (match cap with
        | EInt i -> malloc_array i typ
        | _      -> raise generic_type_err
        )
    | EArrayRef (arr, index) when not (is_value arr) ->
        EArrayRef (step_h arr, index)
    | EArrayRef (arr, index) when not (is_value index) ->
        EArrayRef (arr, step_h index)
    | EArrayRef (arr, index) ->
        (match (arr, index) with
        | (EArrayPtr (t, addr, cap), EInt i) ->
            if i < cap then
              EPtr (t, addr + i)
            else
              raise generic_type_err
        | _                             -> raise generic_type_err
        )
    | EWhile  (e1, e2) as wloop -> EIf (e1, ESeq (e2, wloop), EUnit)
    | ELength e ->
        (match e with
        | exp when not (is_value exp) -> ELength (step_h e)
        | EArrayPtr (_, _, cap) -> EInt cap
        | _ -> raise generic_type_err
        )

    | EPrint e when not (is_value e) -> EPrint (step_h e)
    | EPrint e ->
        ignore(print_endline (string_of_expr e)); EUnit
    | _ -> raise generic_type_err
  in
  (!env_ref, stepped)

(* evaluate_value expr calls step on expr repeatedly until
 * the result is a value, when it returns the fully evaluated form
 * of expr. Firstly typechecks.
 *)
let evaluate_value env expr =
  let rec loop env expr =
    match expr with
    | value when is_value value -> (env, value)
    | some_val ->
        let (new_env, stepped) = step env some_val in
        loop new_env stepped
  in
  let typ = typecheck expr in
  (typ, loop env expr)

(* Same as evaluate_value, but prints out each step. *)
let evaluate_print_steps env value =
  let rec loop env value =
    match value with
    | value when is_value value -> (env, value)
    | some_val ->
        let (new_env, stepped) = step env some_val in
        ignore(Printf.printf "%s | %s\n" (string_of_expr stepped)
          (string_of_env new_env));
        loop new_env stepped
  in
  ignore(typecheck value);
  loop env value;
