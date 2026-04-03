(* $Id: c_pack.mli,v 1.7 2008/03/20 10:23:17 deraugla Exp $ *)

open Keysym;
open Rtdecl;

type pack_event = [ PackEvKeyPress of modified keysym ];

(*** Pack widget builder *)

type pack_args = (orientation * list widget_desc);
type pack_event_handler = widget -> pack_event -> unit;

value pack_desc :
  list attribute -> pack_args -> pack_event_handler -> widget_desc;

value pack_extend : widget -> list widget_desc -> unit;
   (* Extend the pack with the list of widget_desc. Need a call to
      [rt_adjust_widget] on the top window it belongs to to terminate
      the job. *)
value pack_remove_nth : widget -> int -> unit;
   (* Remove the n-th element of the pack. Need a call to [rt_adjust_widget]
      on the top window it belongs to to terminate the job. *)

value pack_border : ref int;
value pack_band : ref int;
value pack_inter : ref int;
