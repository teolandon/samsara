(* Returns the numeric value of the given char, given it's a digit *)
let int_of_char ch =
  (Pervasives.int_of_char ch) - 48

exception Lexing_error of string

let rec read_int ic digit_list =
  let rec calc_int curr_mult curr_int curr_digit_list =
    match curr_digit_list with
    | []     -> curr_int
    | (h::t) ->
        let new_part_int = h * curr_mult in
        calc_int (curr_mult * 10) (new_part_int + curr_int) t
  in
  let next = input_char ic in
  match next with
  | ' ' | '\n' | '(' | ')' ->
      seek_in ic (pos_in ic - 1); (* It's hacky and I know it *)
      calc_int 1 0 digit_list
  | '0'..'9' -> read_int ic ((int_of_char next)::digit_list)
  | _ -> raise (Lexing_error "Invalid end of integer")

let safe_read_char ic =
  try
    Some (input_char ic)
  with End_of_file ->
    None

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
        | '+'   -> Parser.EPlus :: tokenList
        | '0'..'9' ->
            (Parser.EInt (read_int_h (int_of_char c))) :: tokenList
        | ' ' | '\n'   -> tokenList
        |  _           -> raise (Lexing_error "Invalid char")
      in
      lex_h ic newList
  )

let lex file =
  let ic = open_in file in
  lex_h ic []
