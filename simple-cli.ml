(* Open needed modules *)
open Arg
open Printf

type mode =
  | Print
  | Length
  | Help

let cliMode = ref Print

(* A reverse list of the arguments, along with *)
(*    an add operation and a print operation   *)
let argList = ref []

let addArg arg =
  argList := arg :: !argList

let printArgs () =
  List.iter print_endline (List.rev !argList)

let printLengths () =
  List.iter (printf "%d\n") (List.map String.length (List.rev !argList))

(* Usage message *)
let printHelp () =
  print_endline "Usage: simple-cli [flags] [args]";
  print_endline "Available flags:";
  print_endline "  -length     prints the lengths of each of the arguments";
  print_endline "  -help       prints this help message";;

(* Speclist for Arg Module *)
let speclist = [
  ("-length", Arg.Unit (fun () -> cliMode := Length),
      "prints the lengths of each argument");
  ("-help",   Arg.Unit (fun () -> cliMode := Help), "prints this help message")
]

let main () =
  Arg.parse speclist addArg "Usage?";
  match !cliMode with
  | Print  -> printArgs ()
  | Help   -> printHelp ()
  | Length -> printLengths ()

(* Main call to Arg.parse *)
let () = main ()
