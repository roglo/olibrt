(* $Id: rt_color.mli,v 1.3 2010/10/08 08:09:04 deraugla Exp $ *)

open Rtdecl;

value color_pixel : color -> int;
value rt_change_color : color -> (int * int * int) -> unit;
value rt_white_color : xdata -> color;
value rt_black_color : xdata -> color;
value rt_create_color : xdata -> (int * int * int) -> color;
value rt_create_gc_with_color : color -> gc;
value rt_closest_color : xdata -> (int * int * int) -> color;
value rt_select_color : color -> unit;
value rt_select_background_color : color -> unit;
value rt_query_color : xdata -> int -> (int * int * int);
value rt_free_color : color -> unit;

value rgb_of_hsv : (int * int * int) -> (int * int * int);
