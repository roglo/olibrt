(* $Id: go.ml,v 1.3 2007/07/05 17:54:38 deraugla Exp $ *)

open Xabalone;
open Abalone;

Sys.catch_break True;
try abalone xabalone Sys.argv with
[ Failure x -> do { Printf.eprintf "Failed: %s\n" x; flush stderr }
| exc -> Printexc.catch raise exc ];

