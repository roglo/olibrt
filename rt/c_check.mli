(* $Id: c_check.mli,v 1.2 2006/08/01 10:31:12 deraugla Exp $ *)

open Rtdecl;

(*** Reported events *)

type check_event = [ CheckEvPress | CheckEvRelease ];

(*** Check widget builder *)

type check_args = unit;
type check_event_handler = widget -> check_event -> unit;

value check_desc :
  list attribute -> check_args -> check_event_handler -> widget_desc;

value check_set : widget -> bool -> unit;
value check_val : widget -> bool;
