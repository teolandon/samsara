(* Main file *)
open Printf

type token =
  | ELeftParen | ERightParen
  | EPlus
  | EInt of int

let string_of_token t =
  match t with
  | ELeftParen -> "("
  | ERightParen -> ")"
  | EPlus -> "+"
  | EInt a -> string_of_int a

let tokenList: token list ref = ref []

let addToken t =
  tokenList := t :: !tokenList

(* Returns the numeric value of the given char, given it's a digit *)
let int_of_char ch =
  (Pervasives.int_of_char ch) - 48

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
  | ' ' | '\n' | '(' | ')' -> calc_int 1 0 digit_list
  | '0'..'9' -> read_int ic ((int_of_char next)::digit_list)
  | _ -> printf "char '%c' too op\n" next; exit 0; 0

let rec print_tokens token_list =
  match token_list with
  | [] -> ()
  | (t::ts) -> print_endline (string_of_token t); print_tokens ts

let loop filename =
  let ic = open_in filename in
  let read_int_h i =
    read_int ic [i]
  in
  printf "Parsing file %s:\n" filename;
  try
    while true do
      let ch = input_char ic in
      match ch with
      | '('   -> addToken ELeftParen
      | ')'   -> addToken ERightParen
      | '+'   -> addToken EPlus
      | '0'..'9' ->
          addToken (EInt (read_int_h (int_of_char ch)));
          seek_in ic (pos_in ic - 1) (* It's hacky and I know it *)
      | ' '   -> ()
      |  _    -> ()
    done
  with End_of_file ->
    let lst = List.rev !tokenList in
    print_tokens lst;
    printf "Ended parsing file %s\n" filename

let main () =
  Arg.parse [] loop "this"

let () = main ()
