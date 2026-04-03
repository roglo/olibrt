(* $Id: c_raw.mli,v 1.7 2008/12/10 10:46:30 deraugla Exp $ *)

open Rtdecl;
open Keysym;

(*** Event selection *)

type event_selection =
  [ SelButtonMotion
  | SelButtonPress
  | SelButtonRelease
  | SelEnterWindow
  | SelExposure
  | SelFocusChange
  | SelKeyPress
  | SelLeaveWindow
  | SelPointerMotion
  | SelStructureNotify ]
;

(*** Reported event *)

type raw_event =
  [ RawEvButtonPress of int and int and int and int and modified int
  | RawEvButtonRelease of int and int and int and int and modified int
  | RawEvConfigureNotify of (int * int * int) and (int * int * int)
  | RawEvEnterNotify of int and int and int and int
  | RawEvExpose of int and int and int and int
  | RawEvFocusIn
  | RawEvFocusOut
  | RawEvGraphicsExpose of int and int and int and int and int
  | RawEvKeyPress of modified keysym
  | RawEvLeaveNotify
  | RawEvMotionNotify of int and int
  | RawEvNoExpose ]
;

(*** Raw widget builder *)

type raw_args = (int * int * int * list event_selection);
type raw_event_handler = widget -> raw_event -> unit;

value raw_desc :
  list attribute -> raw_args -> raw_event_handler -> widget_desc;

value raw_set_size : widget -> (int * int) -> unit;
    (* Need a call to [rt_adjust_widget] to terminate the job. *)
