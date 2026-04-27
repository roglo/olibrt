(* sudo apt install libxft-dev libfreetype-dev libxrandr-dev *)

open Rt;
open Rtf;

value main () = do {
  let xdf = rtf_initialize "" in
  let xd = rtf_xdata xdf in
  let xa = rt_args [xd] in
  rt_main_loop xa;
};

main ();
