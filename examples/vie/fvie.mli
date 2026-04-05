(* $Id: fvie.mli,v 1.3 1998/12/28 12:50:43 ddr Exp $ *)

type elem = int;
external c_one_step :
  array (array elem) -> int -> int -> bool = "ML_c_one_step";
external c_update : array (array elem) -> int -> int -> bool = "ML_c_update";
external c_fill :
  array (array elem) -> int -> int -> elem -> unit = "ML_c_fill";
