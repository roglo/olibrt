(* $Id: c_check.ml,v 1.6 2008/04/16 09:24:46 deraugla Exp $ *)

open Rtdecl;
open Util;
open Xlib;

type check_event = [ CheckEvPress | CheckEvRelease ];

type check_args = unit;
type check_event_handler = widget -> check_event -> unit;

value check_band = 1;
value check_width = 9;

type check_local_info =
  { set : mutable bool; foreg : option paint; backg : option paint }
;

value (check_local_info, get_check_local_info) =
  dynamo_local_info "check_local_info" (ref (None : option check_local_info))
;

value check_wsize att_val args xd =
  let w = check_width + 2 * (check_band + xd.motif_border) in
  let h = check_width + 2 * (check_band + xd.motif_border) in
  let b = 0 in
  {sh_width = w; sh_height = h; sh_border = b; base_width = w;
   base_height = h; width_inc = -1; height_inc = -1}
;

value check_wcreate att_val args xd pwin is_top in_popup wdesc x y wsh = do {
  let mask = exposureMask lor buttonPressMask lor buttonReleaseMask in
  let info =
    check_local_info
      {set = False; backg = att_val.backg_att; foreg = att_val.foreg_att}
  in
  let win = create_window xd pwin is_top x y wsh att_val mask in
  match att_val.backg_att with
  [ None -> Motif.motif_backg xd win
  | _ -> () ];
  let wid = create_widget xd win is_top x y wsh wdesc info [] in
  add_widget att_val.name_att win wid
};

value check_expose xd wid li = do {
  let size = max 1 (min wid.width wid.height) in
  let x = (wid.width - size) / 2 in
  let y = (wid.height - size) / 2 in
  if li.set then
    match li.foreg with
    [ Some (ColorPn col) -> do  {
        xSetForeground (xd.dpy, xd.gc, col.pixel);
        xFillRectangle (xd.dpy, wid.win, xd.gc, x, y, size, size);
      }
    | Some _ | None -> () ]
  else
    match li.backg with
    [ Some (ColorPn col) -> do {
        xSetForeground (xd.dpy, xd.gc, col.pixel);
        xFillRectangle (xd.dpy, wid.win, xd.gc, x, y, size, size);
      }
    | Some _ | None -> () ];
  Motif.motif_border xd wid.win x y size size
    (if li.set then Motif.MBpush else Motif.MBpop)
};

value check_set wid val0 = do {
  let li = get_check_local_info wid.info in
  let xd = wid.wid_xd in
  li.set := val0;
  check_expose xd wid li
};

value check_val wid =
  let li = get_check_local_info wid.info in li.set
;

value check_wdispatch dir callb wid xev =
  let xd = wid.wid_xd in
  let li = get_check_local_info wid.info in
  let t = xEvent_type xev in
  if t = expose then check_expose xd wid li
  else if t = buttonPress then callb wid CheckEvPress
  else if t = buttonRelease then callb wid CheckEvRelease
  else ()
;

value check_desc attr args callb =
  let att_val = attribute_values attr in
  {wsize = check_wsize att_val args; wcreate = check_wcreate att_val args;
   wresize wid w h = ();
   wdestroy wid = remove_widget att_val.name_att wid.win wid;
   wdispatch = check_wdispatch args callb; wselect _ _ = (); wfreeze wid = ();
   wargs = no_args_info; filler = List.mem FillerAtt attr}
;
