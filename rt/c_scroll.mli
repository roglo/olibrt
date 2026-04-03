(* $Id: c_scroll.mli,v 1.3 2008/04/03 05:31:03 deraugla Exp $ *)

open Rtdecl;

(*** Reported events *)

type scroll_event =
  [ ScrollEvArrowPress of int
  | ScrollEvArrowRelease of int
  | ScrollEvButtonPress of modified int and int
  | ScrollEvButtonRelease
  | ScrollEvButtonMotion of int
  | ScrollEvSizeChange ]
;

(*** Scroll widget builder *)

type scroll_args = (orientation * int * int * int);
type scroll_event_handler = widget -> scroll_event -> unit;

value scroll_desc :
  list attribute -> scroll_args -> scroll_event_handler -> widget_desc;

(*** Scroll functions *)

value scroll_set : widget -> int -> unit;
        (* [scroll_set wid val] sets the cursor value in the scroll; if
           the scroll has [vmin], [vmax] and [csize] as parameters (see
           [scroll_args]), then:
-          [val = vmin] means the cursor completely hidden before the begin.
-          [val = vmax] means the cursor wholly visible at the end.
-          [val = vmax + csize] means the cursor completely hidden after the
           end.
-          [val] between [vmin] and [vmax + csize] means a value interpolated.
        *)
value scroll_val : widget -> int;

value scroll_set_bar_size : widget -> int -> unit;
