(* $Id: jmage.mli,v 1.2 2006/05/27 18:02:02 deraugla Exp $ *)

open RtN;

type inputPic =
  { iParr : array (bool * string * int * int * char * int * string * widget);
    iPcur : mutable int;
    iPlin : mutable int;
    iPcol : mutable int;
    iPtyp : mutable char;
    iPdec : mutable int;
    iPstr : mutable bytes;
    iPind : mutable int;
    iPwid : mutable widget }
;

type field =
  [ Fint of int
  | Fdecimal of (int * int * int)
  | Fstring of string
  | Fempty ]
;

value empty_pic : string -> string;
value input_of_pic : widget -> string -> inputPic;
value goto_field : inputPic -> int -> bool -> unit;

value lock_field : inputPic -> int -> unit;
value get_field : inputPic -> int -> field;
value format_field : string -> char -> int -> field -> bytes -> unit;
value set_field : inputPic -> int -> field -> unit;

value get_string : field -> string;
value get_int : field -> int;

value string_of_mois : int -> string;
value string_of_somme : int -> int -> string;
value string_of_date : int -> int -> int -> string;

value uppercase : string -> string;
value capitalize : string -> string;

value mois_annee_large : int -> int -> string;
value large : string -> string;

value term_goto : widget -> int -> int -> unit;
