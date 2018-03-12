(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*                Daniel de Rauglaudre, INRIA Rocquencourt             *)
(*                                                                     *)
(*  Copyright 1997-2008 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ledit.mli,v 1.13 2008/01/16 09:24:48 deraugla Exp $ *)

value input_char : in_channel -> string;

value set_prompt : string -> unit;
value get_prompt : unit -> string;
value open_histfile : bool -> string -> unit;
value close_histfile : unit -> unit;
value set_max_len : int -> unit;
value set_son_pid : int -> unit;

value unset_meta_as_escape : unit -> unit;
value set_utf8 : unit -> unit;
value set_ascii : unit -> unit;

value trace_sequences : ref bool;
