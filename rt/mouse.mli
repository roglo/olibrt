(* $Id: mouse.mli,v 1.2 1998/12/26 11:44:18 roglo Exp $ *)

open Rtdecl;

value rt_create_bitmap_mouse :
  xdata -> string -> string -> int -> int -> (int * int * int) ->
    (int * int * int) -> int -> int -> mouse;
value rt_create_font_mouse : xdata -> int -> mouse;
value rt_select_mouse : widget -> mouse -> unit;
value rt_unselect_mouse : widget -> unit;
