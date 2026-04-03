(* $Id: c_arrow.ml,v 1.10 2008/04/16 09:24:46 deraugla Exp $ *)

open Rtdecl;
open Util;
open Xlib;

type arrow_event = [ ArrowEvPress | ArrowEvRelease ];

type arrow_args = direction;
type arrow_event_handler = widget -> arrow_event -> unit;

value arrow_band = 1;
value arrow_width = 9;

value arrow_wsize att_val args xd =
  let w = arrow_width + 2 * (arrow_band + xd.motif_border) in
  let h = arrow_width + 2 * (arrow_band + xd.motif_border) in
  let b = 0 in
  {sh_width = w; sh_height = h; sh_border = b; base_width = w;
   base_height = h; width_inc = -1; height_inc = -1}
;

value arrow_wcreate att_val args xd pwin is_top in_popup wdesc x y wsh = do {
  let mask = exposureMask lor buttonPressMask lor buttonReleaseMask in
  let win = create_window xd pwin is_top x y wsh att_val mask in
  Motif.motif_backg xd win;
  let wid = create_widget xd win is_top x y wsh wdesc no_info [] in
  add_widget att_val.name_att win wid
};

value arrow_expose xd wid dir inv =
  let band = arrow_band in
  let (adir, x, y, width, height) =
    match dir with
    [ D_up ->
        (Motif.MDup, band, band, wid.width - 2 * band,
         2 * xd.motif_border + arrow_width)
    | D_down ->
        (Motif.MDdown, band, 0, wid.width - 2 * band,
         2 * xd.motif_border + arrow_width)
    | D_left ->
        (Motif.MDleft, band, band, 2 * xd.motif_border + arrow_width,
         wid.height - 2 * band)
    | D_right ->
        (Motif.MDright, 0, band, 2 * xd.motif_border + arrow_width,
         wid.height - 2 * band) ]
  in
  let mb = if inv then Motif.MBpush else Motif.MBpop in
  Motif.motif_arrow xd wid.win x y width height adir mb
;

value arrow_wdispatch dir callb wid xev =
  let xd = wid.wid_xd in
  let t = xEvent_type xev in
  if t = expose then arrow_expose xd wid dir False
  else if t = buttonPress then do {
    arrow_expose xd wid dir True;
    callb wid ArrowEvPress
  }
  else if t = buttonRelease then do {
    arrow_expose xd wid dir False;
    callb wid ArrowEvRelease
  }
  else ()
;

value arrow_desc attr args callb =
  let att_val = attribute_values attr in
  {wsize = arrow_wsize att_val args; wcreate = arrow_wcreate att_val args;
   wresize wid w h  = ();
   wdestroy wid = remove_widget att_val.name_att wid.win wid;
   wdispatch = arrow_wdispatch args callb; wselect _ _ = (); wfreeze wid = ();
   wargs = no_args_info; filler = List.mem FillerAtt attr}
;
