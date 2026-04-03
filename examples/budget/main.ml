(* $Id: main.ml,v 1.4 2006/11/10 20:47:35 deraugla Exp $ *)

open Stdpp;

value print_version () = do {
  Printf.eprintf "Budget version 1.1\n";
  flush Pervasives.stderr;
  exit 0
};

value main () = do {
  Arg.parse [("-v", Arg.Unit print_version, "print version")]
    (fun s -> File.source.val := s) "Usage: budget [-v] [source]";
  Budget.go ""
};

Printexc.catch
  (fun () ->
     try main () with
     [ Ploc.Exc _ exc -> do {
         Printf.printf "At some location...\n";
         flush Pervasives.stdout;
         raise exc
       }
     | exc -> raise exc ])
  ()
;
