(* $Id: c_term.mli,v 1.5 2008/11/21 10:31:18 deraugla Exp $ *)

open Keysym;
open Rtdecl;

(*** Reported events *)

type term_event =
  [ TermEvButtonPress of int and int and int and modified int
  | TermEvButtonRelease of int and int and int and modified int
  | TermEvButtonMotion of int and int and int
  | TermEvKeyPress of modified keysym
  | TermEvAnswer of string
  | TermEvExtendHistory
  | TermEvSizeChange of int and int ]
;

(*** Term widget builder *)

type term_args = (int * int * int);
type term_event_handler = widget -> term_event -> unit;

value term_desc :
  list attribute -> term_args -> term_event_handler -> widget_desc;

value term_size : widget -> (int * int);
        (* In rows, columns *)
value term_set_size : widget -> (int * int) -> unit;
        (* In rows, columns. Need a call to [rt_adjust_widget] to terminate
           the task. *)
value term_set_max_history_size : widget -> int -> unit;
        (* Change the 3rd parameter of the term argument (number of saved
           lines) *)
value term_current_position : widget -> (int * int);
        (* In rows, columns, starting from 0 *)
value term_string_of_keysym : widget -> modified keysym -> string;
        (* Depending on the internal state of the widget (newline mode,
           ansi/application mode, etc) *)
value term_history_size : widget -> int;
        (* Returns the current history buffer size (from 0 to [nhist],
           defined by [term_desc]). *)
value term_shift : widget -> int -> unit;
        (* [term_shift wid nb] sets the display shift to [nb] lines down
           (0 <= [nb] <= [term_history_size wid]). *)
value term_shift_value : widget -> int;

(*** Emphasized zone *)

value term_emphasize_from : widget -> int -> int -> unit;
        (* [term_emphasize_from wid row col] starts emphasized zone,
           deleting the previous one, if any. *)
value term_emphasize_to : widget -> int -> int -> unit;
        (* [term_emphasize_to wid row col] updates emphasized zone. *)
value term_emphasized_location : widget -> ((int * int) * (int * int));
value term_get_emphasized : widget -> string;

value term_font : array string;
value term_border : ref int;
value term_inter : ref int;
value term_band : ref int;
value term_blink : ref (int * int);
