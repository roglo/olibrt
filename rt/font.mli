(* $Id: font.mli,v 1.2 1998/12/26 11:44:12 roglo Exp $ *)

open Rtdecl;

value rt_load_query_font : xdata -> string -> font;
value rt_select_font : font -> unit;
value rt_text_width : font -> string -> int;
value rt_font_size : font -> int;
