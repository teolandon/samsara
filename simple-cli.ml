(* Open needed modules *)
open Arg
open Printf

let printArg arg =
  print_endline arg

let printLength arg =
  printf "%d\n" (String.length arg)

(* Usage message *)
let printHelp () =
  print_endline "Usage: simple-cli [flags] [args]";
  print_endline "Available flags:";
  print_endline "  -length     prints the lengths of each of the arguments";
  print_endline "  -help       prints this help message";
  exit 0; ()

(* Speclist for Arg Module *)
let speclist = [("-length", Arg.Rest printLength, "prints the lengths of each of the arguments");
                ("-help", Arg.Unit printHelp, "prints this help message")]

(* Main call to Arg.parse *)
let () = Arg.parse speclist printArg "Usage?"
