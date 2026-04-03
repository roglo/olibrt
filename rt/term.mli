(* $Id: term.mli,v 1.3 1999/01/08 16:35:24 roglo Exp $ *)

open Rtdecl;

(*** Sending text (simulation of vt100) *)

value term_send : widget -> string -> unit;
