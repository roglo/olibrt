(* $Id: resource.ml,v 1.2 1998/12/26 11:44:22 roglo Exp $ *)

open Xlib;
open Rtdecl;

value rt_get_default xd program option =
  let val0 = xGetDefault (xd.dpy, program, option) in
  if is_null_C_String val0 then ""
  else string_of_C_String (val0, c_String_length val0)
;
