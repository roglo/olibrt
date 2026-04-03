(* $Id: c_arrow.mli,v 1.3 2006/05/28 09:44:53 deraugla Exp $ *)

open Rtdecl;

(*** Reported events *)

type arrow_event = [ ArrowEvPress | ArrowEvRelease ];

(*** Arrow widget builder *)

type arrow_args = direction;
type arrow_event_handler = widget -> arrow_event -> unit;

value arrow_desc :
  list attribute -> arrow_args -> arrow_event_handler -> widget_desc;
