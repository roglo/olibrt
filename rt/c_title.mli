(* $Id: c_title.mli,v 1.4 2008/03/20 18:35:12 deraugla Exp $ *)

open Rtdecl;

type title_event = [ TitleEvButtonPress of modified int ];

(*** Title widget builder *)

type title_args = string;
type title_event_handler = widget -> title_event -> unit;

value title_desc :
  list attribute -> title_args -> title_event_handler -> widget_desc;
value title_change : widget -> string -> unit;
   (* Change the title text. Need a call to [rt_adjust_widget]
      on the top window it belongs to to terminate the job. *)

value title_border : ref int;
value title_band : ref int;
value title_font : ref string;
