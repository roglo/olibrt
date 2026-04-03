(* $Id: time.mli,v 1.2 1998/12/26 11:44:42 roglo Exp $ *)

type timeb = { time : int; millitm : int };
value ftime : unit -> timeb;
value timeb_sub : timeb -> timeb -> int;
