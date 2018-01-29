(* Open needed modules *)
open Arg
open Printf

(* Program mode of operation *)
type mode =
  | Print
  | Length

let cliMode = ref Print

(* A reverse list of the arguments, along with *)
(*  an add operation and the print operations  *)
let argList = ref []

let addArg arg =
  argList := arg :: !argList

let printArgs () =
  List.iter print_endline (List.rev !argList)

let printLengths () =
  List.iter (printf "%d\n") (List.map String.length (List.rev !argList))

let usageMsg =
    "Usage: simplecli [flags] [args]\nAvailable flags:"

let printHelp speclist =
  usage speclist usageMsg

(* Speclist for Arg Module *)
let rec speclist = [
  ("-length", Arg.Unit (fun () -> cliMode := Length),
      " prints the lengths of each argument");
  ("-help",   Arg.Unit (fun () -> printHelp (align speclist); exit 0),
      " prints this help message");
  ("--help",   Arg.Unit (fun () -> ()), "") (* Supresses default flag *)
]

(* Use aligned version *)
let speclist = align speclist

let main () =
  Arg.parse speclist addArg usageMsg;
  match !cliMode with
  | Print  -> printArgs ()
  | Length -> printLengths ()

let () = main ()
