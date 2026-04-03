(* $Id: computer.mli,v 1.2 1998/12/28 11:21:39 ddr Exp $ *)

open Common;

value possible_moves : player -> list move;
value alpha_beta : int -> player -> list move -> list move;

