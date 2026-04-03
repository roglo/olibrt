(* $Id: c_line.mli,v 1.2 2015/06/22 09:35:36 deraugla Exp $ *)

open Rtdecl;

(*** Reported events *)

type line_event = [ NoLineEvent ];

(*** Line widget builder *)

type line_args = unit;
type line_event_handler = widget -> line_event -> unit;

value line_desc :
  list attribute -> line_args -> line_event_handler -> widget_desc;
