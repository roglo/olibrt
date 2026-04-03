(* $Id: pattern.mli,v 1.2 1998/12/26 11:44:21 roglo Exp $ *)

open Rtdecl;

type pattern = 'aa';
value rt_create_pattern : xdata -> string -> int -> int -> pattern;
value rt_select_pattern : pattern -> (int * int) -> unit;
value rt_select_pattern_mask : pattern -> (int * int) -> unit;
value rt_select_pattern_clip : pattern -> (int * int) -> unit;
value rt_unselect_pattern_clip : xdata -> unit;

