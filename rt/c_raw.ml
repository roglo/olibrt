(* $Id: c_raw.ml,v 1.16 2008/12/18 17:56:03 deraugla Exp $
 *
 * Rogloglo Toolkit: raw widget class
 *)

open Keysym;
open Std;
open Rtdecl;
open Util;
open Xlib;

type event_selection =
  [ SelButtonMotion
  | SelButtonPress
  | SelButtonRelease
  | SelEnterWindow
  | SelExposure
  | SelFocusChange
  | SelKeyPress
  | SelLeaveWindow
  | SelPointerMotion
  | SelStructureNotify ]
;

type raw_event =
  [ RawEvButtonPress of int and int and int and int and modified int
  | RawEvButtonRelease of int and int and int and int and modified int
  | RawEvConfigureNotify of (int * int * int) and (int * int * int)
  | RawEvEnterNotify of int and int and int and int
  | RawEvExpose of int and int and int and int
  | RawEvFocusIn
  | RawEvFocusOut
  | RawEvGraphicsExpose of int and int and int and int and int
  | RawEvKeyPress of modified keysym
  | RawEvLeaveNotify
  | RawEvMotionNotify of int and int
  | RawEvNoExpose ]
;

type raw_args = (int * int * int * list event_selection);
type raw_event_handler = widget -> raw_event -> unit;

value (raw_args, get_raw_args) =
  dynamo_args_info "raw_args_info" (ref None)
;

value raw_wdispatch callb wid xev =
  let xd = wid.wid_xd
  and t = xEvent_type xev in
  if t == expose then
    let xev = xEvent_xexpose xev in
    let ev =
      RawEvExpose (xExposeEvent_x xev) (xExposeEvent_y xev)
        (xExposeEvent_width xev) (xExposeEvent_height xev)
    in
    callb wid ev
  else if t == graphicsExpose then
    let xev = xEvent_xgraphicsexpose xev in
    let ev =
      RawEvGraphicsExpose (xGraphicsExposeEvent_x xev)
        (xGraphicsExposeEvent_y xev) (xGraphicsExposeEvent_width xev)
        (xGraphicsExposeEvent_height xev) (xGraphicsExposeEvent_count xev)
    in
    callb wid ev
  else if t == noExpose then callb wid RawEvNoExpose
  else if t == configureNotify then do {
    let xev = xEvent_xconfigure xev in
    let x = xConfigureEvent_x xev
    and y = xConfigureEvent_y xev
    and width = xConfigureEvent_width xev
    and height = xConfigureEvent_height xev
    and border = xConfigureEvent_border_width xev in
    let old_sz = (wid.width, wid.height, wid.border) in
    let new_sz = (width, height, border) in
    wid.x := x;
    wid.y := y;
    wid.width := width;
    wid.height := height;
    wid.border := border;
    callb wid (RawEvConfigureNotify old_sz new_sz);
  }
  else if wid.frozen then ()
  else if t == keyPress then
    let xkey_ev = xEvent_xkey xev in
    let state = xKeyEvent_state xkey_ev in
    let ksym =
      let k = keysym_int_of_xevent xev in
      keysym_of_keysym_int k state
    in
    let ev = RawEvKeyPress ksym in callb wid ev
  else if t == buttonPress then
    let xev = xEvent_xbutton xev in
    let ev =
      RawEvButtonPress (xButtonEvent_x xev) (xButtonEvent_y xev)
        (xButtonEvent_x_root xev) (xButtonEvent_y_root xev)
        (item_modified (xButtonEvent_state xev) (xButtonEvent_button xev))
    in
    callb wid ev
  else if t == buttonRelease then
    if xd.win_but == WB_None then
      let xev = xEvent_xbutton xev in
      let ev =
        RawEvButtonRelease (xButtonEvent_x xev) (xButtonEvent_y xev)
          (xButtonEvent_x_root xev) (xButtonEvent_y_root xev)
          (item_modified (xButtonEvent_state xev) (xButtonEvent_button xev))
      in
      callb wid ev
    else ()
  else if t == motionNotify then do {
    let args = (xd.dpy, pointerMotionMask, xev) in
    while xCheckMaskEvent args <> 0 do { () };
    let xev = xEvent_xmotion xev in
    let ev = RawEvMotionNotify (xMotionEvent_x xev) (xMotionEvent_y xev) in
    callb wid ev
  }
  else if t == enterNotify then
    let xev = xEvent_xcrossing xev in
    let ev =
      RawEvEnterNotify (xCrossingEvent_x xev) (xCrossingEvent_y xev)
        (xCrossingEvent_x_root xev) (xCrossingEvent_y_root xev)
    in
    callb wid ev
  else if t == leaveNotify then callb wid RawEvLeaveNotify
  else if t == focusIn then callb wid RawEvFocusIn
  else if t == focusOut then callb wid RawEvFocusOut
  else ()
;

value mask_of_selection =
  fun
  [ SelButtonMotion -> buttonMotionMask
  | SelButtonPress -> buttonPressMask lor ownerGrabButtonMask
  | SelButtonRelease -> buttonReleaseMask
  | SelEnterWindow -> enterWindowMask
  | SelExposure -> exposureMask
  | SelFocusChange -> focusChangeMask
  | SelKeyPress -> keyPressMask
  | SelLeaveWindow -> leaveWindowMask
  | SelPointerMotion -> pointerMotionMask
  | SelStructureNotify -> structureNotifyMask ]
;

value raw_desc attr args callb =
  let att_val = attribute_values attr in
  let (w, h, b, emask) = args in
  let wargs = raw_args (ref args) in
  {wsize xd =
     let (w, h, b, emask) = (get_raw_args wargs).val in
     let w = max 1 (opt_val w att_val.width_att) in
     let h = max 1 (opt_val h att_val.height_att) in
     let b = max 0 (opt_val b att_val.border_att) in
     {sh_width = w; sh_height = h; sh_border = b; base_width = w;
      base_height = h; width_inc = -1; height_inc = -1};
   wcreate xd pwin is_top in_popup wdesc x y wsh =
     let win =
       create_window xd pwin is_top x y wsh att_val
         (List.fold_left (fun m es -> m lor mask_of_selection es) 0 emask)
     in
     let wid = create_widget xd win is_top x y wsh wdesc no_info [] in
     add_widget att_val.name_att win wid;
   wresize wid w h = ();
   wdestroy wid = remove_widget att_val.name_att wid.win wid;
   wdispatch = raw_wdispatch callb; wselect _ _ = (); wfreeze _ = ();
   wargs = wargs; filler = List.mem FillerAtt attr}
;

value raw_set_size wid (w, h) = do {
  let wargs = get_raw_args wid.wdesc.wargs in
  let (_, _, b, emask) = wargs.val in
  wargs.val := (w, h, b, emask);
};
