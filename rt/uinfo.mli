(* $Id: uinfo.mli,v 1.2 1998/12/26 11:44:43 roglo Exp $ *)

open Rtdecl;

type uoption 'ta' = [ UNone | USome of 'ta' ];
type user_info_func 'ta' = ('ta' -> user_info * widget -> 'ta');
value user_info : string -> ref (uoption 'ta') -> user_info_func 'ta';
value rt_set_user_info : widget -> user_info -> unit;
