(* $Id: time.ml,v 1.3 2004/07/26 04:02:13 ddr Exp $ *)

open Xlib;

type timeb = { time : int; millitm : int };

value ftime () =
  let t = mL_ctime () in
  {time = t; millitm = mL_ctime_ms ()}
;

value timeb_sub t1 t2 = (t1.time - t2.time) * 1000 + t1.millitm - t2.millitm;
