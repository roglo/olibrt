(* $Id: c_button.mli,v 1.6 2008/02/10 07:02:59 deraugla Exp $ *)

open Rtdecl;

(*
    Buttons are widgets holding a non mutable text, sensitive to mouse
    button press and release. When a mouse button is pressed in the widget,
    the button appears in inverse video. Useful to trigger a specific action,
    or pop up a menu.
 *)

(*** Reported events *)

type button_event =
  [ ButtonEvEnter of int and int
  | ButtonEvPress of int and int and modified int
  | ButtonEvRelease of int and int and modified int
  | ButtonEvSizeChange
  | ButtonEvShortcut ]
;

(*** Button widget builder *)

type button_args = (string * option char);
type button_event_handler = widget -> button_event -> unit;

value button_desc :
  list attribute -> button_args -> button_event_handler -> widget_desc;

(*** Button default attributes values *)

value button_border : ref int;
value button_band : ref int;
value button_bold : ref int;
value button_bold_band : ref int;
value button_font : ref string;

(*** Button function *)

value menu_button : list attribute -> button_args -> widget -> widget_desc;
  (** [menu_button attl args wid] is actually a shortcut for a [button_desc]
      with a call to [rt_map_poppup_widget] on the given [wid] when the
      menu button is pressed or when it is entered when the button was
      pressed. *)
