(* $Id: uinfo.ml,v 1.5 2008/01/23 10:41:08 deraugla Exp $
 *
 * Rogloglo toolkit: user info
 *)

open Xlib;
open Std;
open Rtdecl;

type uoption 'a =
  [ UNone
  | USome of 'a ]
;
type user_info_func 'a = ('a -> user_info * widget -> 'a);

value dynamo_user_info b r =
  (fun x -> C_UI (fun () -> do { r.val := USome x; b }),
   fun
   [ C_UI f -> do {
       r.val := UNone;
       let v = f () in
       match r.val with
       [ UNone -> failwith ("bad type \"" ^ v ^ "\", should be \"" ^ b ^ "\"")
       | USome x -> x ]
     } ])
;

value user_info s =
  let u_info = dynamo_user_info s in
  fun r ->
    let (f, g) = u_info r in (f, fun wid -> g wid.user_info)
;

value rt_set_user_info wid ui = wid.user_info := ui;
