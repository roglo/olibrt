(* $Id: misc.ml,v 1.10 2009/01/27 21:36:09 deraugla Exp $
 *
 * Rogloglo toolkit: miscellaneous functions
 *)

open Std;
open Rtdecl;
open Util;
open Xlib;

value rt_create_subwidget pwid x y wdesc = do {
  let xd = pwid.wid_xd in
  let wsh = wdesc.wsize xd in
  let wid = wdesc.wcreate xd pwid.win False False wdesc x y wsh in
  pwid.children := [wid :: pwid.children];
  wid
}
and rt_move_widget =
  let mask = List.fold_left \+ 0 [cWX; cWY; cWStackMode]
  and xwc = mallocated_var alloc_XWindowChanges (ref None) in
  fun wid x y -> do {
    let xd = wid.wid_xd in
    let xwc = xwc () in set_XWindowChanges_x (x, xwc);
    set_XWindowChanges_y (y, xwc);
    set_XWindowChanges_stack_mode (above, xwc);
    xConfigureWindow (xd.dpy, wid.win, mask, xwc);
    wid.x := x;
    wid.y := y
  }
and rt_reparent_widget wid pwid x y =
  xReparentWindow (wid.wid_xd.dpy, wid.win, pwid.win, x, y)
and rt_resize_widget wid width height =
  let width = max 1 width
  and height = max 1 height in
  xResizeWindow (wid.wid_xd.dpy, wid.win, width, height)
;

value rt_move_resize_widget wid x y width height = do {
  let xd = wid.wid_xd in
  let width = max 1 width
  and height = max 1 height in
  xRaiseWindow (xd.dpy, wid.win);
  xMoveResizeWindow (xd.dpy, wid.win, x, y, width, height);
  wid.x := x;
  wid.y := y
};

value xswa = mallocated_var alloc_XSetWindowAttributes (ref None);

value rt_create_transient_widget pwid wname wdel wdesc = do {
  let xd = pwid.wid_xd in
  let wsh = wdesc.wsize xd in
  let wid = wdesc.wcreate xd xd.rootw True False wdesc 0 0 wsh in
  let xswa = xswa () in
  set_XSetWindowAttributes_save_under (1, xswa);
  xChangeWindowAttributes (xd.dpy, wid.win, cWSaveUnder, xswa);
  xSetTransientForHint (xd.dpy, wid.win, pwid.win);
  set_std_prop wid wname "<transient>" 0 0 wsh (uSPosition lor uSSize);
  set_wm_and_class_hints wid;
  set_delete_callback wid wdel;
  wid
};

value rt_map_transient_widget =
  let xsh = mallocated_var alloc_XSizeHints (ref None) in
  fun wid x y -> do {
    let xd = wid.wid_xd in
    let x = min (xd.root_width - wid.width) (max 0 x)
    and y = min (xd.root_height - wid.height) (max 0 y) in
    wid.is_mapped := True;
    xUnmapWindow (xd.dpy, wid.win);
    xMoveWindow (xd.dpy, wid.win, x, y);
    let xsh = xsh () in set_XSizeHints_x (x, xsh);
    set_XSizeHints_y (y, xsh);
    set_XSizeHints_width (wid.width, xsh);
    set_XSizeHints_height (wid.height, xsh);
    set_XSizeHints_flags (uSPosition lor uSSize, xsh);
    xSetNormalHints (xd.dpy, wid.win, xsh);
    xMapRaised (xd.dpy, wid.win)
  }
;

value rt_create_popup_widget xd wdesc = do {
  let wsh = wdesc.wsize xd in
  let wid = wdesc.wcreate xd xd.rootw True True wdesc 0 0 wsh in
  let xswa = xswa () in set_XSetWindowAttributes_save_under (1, xswa);
  set_XSetWindowAttributes_override_redirect (1, xswa);
  xChangeWindowAttributes
    (xd.dpy, wid.win, cWSaveUnder lor cWOverrideRedirect, xswa);
  wid
};

value rt_map_popup_widget = Util.map_popup_widget;

value rt_sync xd = xSync (xd.dpy, 1);

value rt_flush xd = xFlush xd.dpy;

value rt_query_pointer wid =
  let root = ref xNone
  and child = ref xNone
  and root_x = ref 0
  and root_y = ref 0
  and win_x = ref 0
  and win_y = ref 0
  and keys_buttons = ref 0 in
  let xd = wid.wid_xd in
  let r =
    xQueryPointer
      (xd.dpy, wid.win, root, child, root_x, root_y, win_x, win_y,
       keys_buttons)
  in
  let kb = keys_buttons.val in
  let buttl =
    List.fold_left (fun l (i, m) -> l @ (if kb land m <> 0 then [i] else []))
      []
      [(1, button1Mask); (2, button2Mask); (3, button3Mask); (4, button4Mask);
       (5, button5Mask)]
  in
  if r <> 0 then (win_x.val, win_y.val, root_x.val, root_y.val, buttl)
  else (-1, -1, root_x.val, root_y.val, buttl)
;

value rt_get_bell_params =
  let xks = mallocated_var alloc_XKeyboardState (ref None) in
  fun xd -> do {
    let xks = xks () in xGetKeyboardControl (xd.dpy, xks);
    (xKeyboardState_bell_percent xks, xKeyboardState_bell_pitch xks,
     xKeyboardState_bell_duration xks)
  }
;

value rt_set_bell_params =
  let xkc = mallocated_var alloc_XKeyboardControl (ref None) in
  fun xd (percent, pitch, duration) -> do {
    let xkc = xkc () in set_XKeyboardControl_bell_percent (percent, xkc);
    set_XKeyboardControl_bell_pitch (pitch, xkc);
    set_XKeyboardControl_bell_duration (duration, xkc);
    xChangeKeyboardControl
      (xd.dpy, kBBellPercent lor kBBellPitch lor kBBellDuration, xkc)
  }
;

value rt_bell xd percent = xBell (xd.dpy, percent);

value rt_set_cut_buffer xd str = do {
  xSetSelectionOwner (xd.dpy, xA_PRIMARY, xNone, currentTime);
  xStoreBytes (xd.dpy, str, String.length str)
}
and rt_get_cut_buffer xd = do {
  let i = ref 0 in
  let b = xFetchBytes (xd.dpy, i) in
  let r = string_of_C_String (b, i.val) in if i.val > 0 then free b else ();
  r
};

value rt_set_win_size row col xpix ypix = ioctl_winsz 0 row col xpix ypix;

value rt_redirect_key_press_to wid = do {
  let xd = wid.wid_xd in
  match xd.x_selected_wid with
  [ None -> ()
  | Some swid ->
      let bwid =
        match xd.redirected_wid with
        [ None -> swid
        | Some rwid -> rwid ]
      in
      if bwid != wid then do {
        bwid.wdesc.wselect bwid False;
        wid.wdesc.wselect wid True
      }
      else () ];
  xd.redirected_wid := Some wid;
  xd.no_redirecting_wid := [];
};

value rt_dont_redirect_key_press xd = do {
  match xd.x_selected_wid with
  [ None -> ()
  | Some swid ->
      match xd.redirected_wid with
      [ None -> ()
      | Some rwid ->
          if swid != rwid then do {
            rwid.wdesc.wselect rwid False;
            swid.wdesc.wselect swid True
          }
          else () ] ];
  xd.redirected_wid := None;
  xd.no_redirecting_wid := [];
};

value rt_dont_redirect_key_press_from wid =
  let xd = wid.wid_xd in
  xd.no_redirecting_wid := [wid :: xd.no_redirecting_wid]
;
