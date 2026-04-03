(* $Id: action.mli,v 1.3 2006/06/02 00:22:00 deraugla Exp $ *)

open Jmage;
open RtN;

value button : (widget -> unit) -> widget -> button_event -> unit;
value popup : string -> widget -> button_event -> unit;
value no_button : widget -> button_event -> unit;
value no_line : widget -> line_event -> unit;
value no_pack : widget -> pack_event -> unit;
value no_term : widget -> term_event -> unit;
value butt_term : widget -> term_event -> unit;
value button_select :
  xdata -> list string -> list 'a -> (unit -> int) -> ('a -> unit) -> bool ->
    int -> unit;
value input :
  inputPic -> (keysym -> unit) -> (inputPic -> unit) -> int -> keysym ->
    unit;
value mettre_accent : char -> char -> char;

value filler : (size_policy * widget_desc);

value place_transient : widget -> unit;
