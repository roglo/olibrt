(* $Id: keysym.mli,v 1.5 2008/02/05 15:35:45 deraugla Exp $ *)

open Rtdecl;

type keysym =
  [ K_Ascii of char
  | K_Alt_L
  | K_Alt_R
  | K_BackSpace
  | K_Begin
  | K_Break
  | K_Cancel
  | K_Caps_Lock
  | K_Clear
  | K_Control_L
  | K_Control_R
  | K_Delete
  | K_Down
  | K_End
  | K_Escape
  | K_Execute
  | K_F of int
  | K_Find
  | K_Help
  | K_Home
  | K_Hyper_L
  | K_Hyper_R
  | K_Insert
  | K_KP_Add
  | K_KP_Decimal
  | K_KP_Divide
  | K_KP_Enter
  | K_KP_Equal
  | K_KP_F of int
  | K_KP_Multiply
  | K_KP_N of int
  | K_KP_Separator
  | K_KP_Space
  | K_KP_Subtract
  | K_KP_Tab
  | K_Left
  | K_Linefeed
  | K_Menu
  | K_Meta_L
  | K_Meta_R
  | K_Mode_switch
  | K_Multi_key
  | K_Next
  | K_Num_Lock
  | K_Pause
  | K_Print
  | K_Prior
  | K_Redo
  | K_Return
  | K_Right
  | K_Scroll_Lock
  | K_Select
  | K_Shift_L
  | K_Shift_Lock
  | K_Shift_R
  | K_Super_L
  | K_Super_R
  | K_Tab
  | K_Undo
  | K_Up
  | K_Other of int ]
;

(*** Handling keysyms *)

value string_of_keysym : modified keysym -> string;
        (* [string_of_keysym km] returns a ``reasonable'' string from
           the key [km]. *)
value is_modifier : keysym -> bool;

value keysym_of_keycode : widget -> int -> int -> modified keysym;
value int_of_keysym : keysym -> int;

value keysym_of_keysym_int : int -> int -> modified keysym;
