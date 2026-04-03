(* $Id: draw.mli,v 1.4 2010/10/08 08:09:04 deraugla Exp $ *)

open Rtdecl;

value rt_draw_point : drawable -> (int * int) -> unit;
value rt_draw_point_with_gc : drawable -> gc -> (int * int) -> unit;
value rt_draw_line : drawable -> (int * int) -> (int * int) -> unit;
value rt_draw_lines : drawable -> list (int * int) -> unit;
value rt_fill_polygon : drawable -> list (int * int) -> unit;
value rt_fill_rectangle : drawable -> (int * int * int * int) -> unit;
value rt_draw_rectangle : drawable -> (int * int * int * int) -> unit;
value rt_fill_arc : drawable -> (int * int * int * int * int * int) -> unit;
value rt_draw_arc : drawable -> (int * int * int * int * int * int) -> unit;
value rt_clear_widget : widget -> unit;
value rt_clear_area : widget -> (int * int * int * int) -> unit;
value rt_draw_string : drawable -> (int * int) -> string -> unit;
value rt_erase_draw_string : drawable -> (int * int) -> string -> unit;
value rt_copy_area :
  drawable -> drawable -> (int * int * int * int) -> (int * int) -> unit;

value rt_set_line_width : xdata -> int -> unit;
value rt_set_dashes : xdata -> int -> list int -> unit;
value rt_set_backing_store : widget -> unit;
