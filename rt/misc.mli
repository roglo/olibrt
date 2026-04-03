(* $Id: misc.mli,v 1.4 2008/03/01 19:54:58 deraugla Exp $ *)

open Rtdecl;

value rt_create_subwidget : widget -> int -> int -> widget_desc -> widget;

value rt_move_widget : widget -> int -> int -> unit;
value rt_resize_widget : widget -> int -> int -> unit;
value rt_move_resize_widget : widget -> int -> int -> int -> int -> unit;
value rt_reparent_widget : widget -> widget -> int -> int -> unit;

value rt_create_transient_widget :
  widget -> string -> delete_callback -> widget_desc -> widget;
value rt_map_transient_widget : widget -> int -> int -> unit;
value rt_create_popup_widget : xdata -> widget_desc -> widget;
value rt_map_popup_widget : widget -> int -> int -> int -> unit;

value rt_sync : xdata -> unit;
value rt_flush : xdata -> unit;
value rt_query_pointer : widget -> (int * int * int * int * list int);
value rt_get_bell_params : xdata -> (int * int * int);
value rt_set_bell_params : xdata -> (int * int * int) -> unit;
value rt_bell : xdata -> int -> unit;

value rt_get_cut_buffer : xdata -> string;
value rt_set_cut_buffer : xdata -> string -> unit;

value rt_set_win_size : int -> int -> int -> int -> unit;

value rt_redirect_key_press_to : widget -> unit;
        (* [rt_redirect_key_press_to wid] redirects the key input to the
           widget [wid] if the application receives key input (depending
           on the current configuration and window manager). *)
value rt_dont_redirect_key_press : xdata -> unit;
value rt_dont_redirect_key_press_from : widget -> unit;
