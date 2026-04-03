(* $Id: pixmap.mli,v 1.2 1998/12/26 11:44:21 roglo Exp $ *)

open Rtdecl;

value rt_create_pixmap : xdata -> int -> int -> pixmap;
value rt_select_pixmap : pixmap -> unit;
