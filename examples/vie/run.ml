(* $Id: run.ml,v 1.3 2007/07/05 18:18:26 deraugla Exp $ *)

open Vie;
open Sys;
open Rt;

value command_line = argv;

value dname = ref "";

value i = ref 1;
while i.val < vect_length command_line do {
  match command_line.(i.val) with
  [ "-bw" -> cCOLOR.val := False
  | "-d" -> incr i
  | "-size" -> do {
      incr i;
      cWID.val := int_of_string command_line.(i.val);
      incr i;
      cHEI.val := int_of_string command_line.(i.val);
      ()
    }
  | s -> do { dname.val := s; () } ];
  incr i
};

try vie dname.val with
[ Failure s -> do {
    print_string "failure: ";
    print_string s;
    print_newline ()
  }
| Invalid_argument s -> do {
    print_string "Invalid_argument: ";
    print_string s;
    print_newline ()
  }
| x -> do {
    print_string "Unknown exception in vie";
    print_newline ();
    raise x
  } ];
