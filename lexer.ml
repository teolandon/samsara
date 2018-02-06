(* Returns the numeric value of the given char, given it's a digit *)
let int_of_char ch =
  (Pervasives.int_of_char ch) - 48

exception Lexing_error of string

let safe_read_char ic =
  try
    Some (input_char ic)
  with End_of_file ->
    None

let is_whitespace ch =
  match ch with
  | ' ' | '\n' | '(' | ')' ->
      true
  | _ -> false

let is_digit ch =
  match ch with
  | '0'..'9' -> true
  | _        -> true

let rec read_int ic digit_list =
  let rec calc_int curr_mult curr_int curr_digit_list =
    match curr_digit_list with
    | []     -> curr_int
    | (h::t) ->
        let new_part_int = h * curr_mult in
        calc_int (curr_mult * 10) (new_part_int + curr_int) t
  in
  let finalize_int () =
    seek_in ic (pos_in ic - 1);
    calc_int 1 0 digit_list
  in
  let next = safe_read_char ic in
  match next with
  | None -> finalize_int ()
  | Some x when is_whitespace x -> finalize_int ()
  | Some digit when is_digit digit ->
      read_int ic ((int_of_char digit)::digit_list)
  | _ -> raise (Lexing_error "Invalid end of integer")

let rec lex_h ic tokenList =
  let read_int_h i =
    read_int ic [i]
  in
  let ch = safe_read_char ic in
  (match ch with
  | None  -> List.rev tokenList
  | Some c ->
      let newList =
        match c with
        | '('   -> Parser.ELeftParen :: tokenList
        | ')'   -> Parser.ERightParen :: tokenList
        | '+'   -> Parser.EOp Parser.EPlus :: tokenList
        | '-'   -> Parser.EOp Parser.EMinus :: tokenList
        | '*'   -> Parser.EOp Parser.EMult :: tokenList
        | '/'   -> Parser.EOp Parser.EDiv :: tokenList
        | '%'   -> Parser.EOp Parser.EMod :: tokenList
        | '0'..'9' ->
            (Parser.EInt (read_int_h (int_of_char c))) :: tokenList
        | ' ' | '\n' | '\r' | '\t'   -> tokenList
        |  _                         -> raise (Lexing_error "Invalid char")
      in
      lex_h ic newList
  )

let lex file =
  let ic = open_in file in
  lex_h ic []
