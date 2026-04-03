(* $Id: c_line.ml,v 1.6 2015/06/22 09:35:36 deraugla Exp $ *)

open Rtdecl;
open Util;
open Xlib;

type line_event = [ NoLineEvent ];
type line_args = unit;
type line_event_handler = widget -> line_event -> unit;

value line_band = 1;

value line_wsize att_val args xd =
  let band = opt_val line_band att_val.band_att in
  let w = 2 * (band + xd.motif_border) in
  let h = 2 * (band + xd.motif_border) in
  let b = 0 in
  {sh_width = w; sh_height = h; sh_border = b; base_width = w;
   base_height = h; width_inc = -1; height_inc = -1}
;

value line_wcreate att_val args xd pwin is_top in_popup wdesc x y wsh = do {
  let win = create_window xd pwin is_top x y wsh att_val exposureMask in
  Motif.motif_backg xd win;
  let wid = create_widget xd win is_top x y wsh wdesc no_info [] in
  add_widget att_val.name_att win wid
};

value line_wdispatch args att_val callb wid xev =
  let xd = wid.wid_xd in
  let t = xEvent_type xev in
  if t = expose then
    let band = opt_val line_band att_val.band_att in
    let x = xd.motif_border + band in
    let y = wid.height / 2 - xd.motif_border in
    let width = wid.width - 2 * xd.motif_border - 2 * band in
    let height = 2 * xd.motif_border in
    Motif.motif_border xd wid.win x y width height Motif.MBpush
  else ()
;

value line_desc attr args callb =
  let att_val = attribute_values attr in
  {wsize = line_wsize att_val args; wcreate = line_wcreate att_val args;
   wresize wid w h = xResizeWindow (wid.wid_xd.dpy, wid.win, w, h);
   wdestroy wid = remove_widget att_val.name_att wid.win wid;
   wdispatch = line_wdispatch args att_val callb; wselect _ _ = ();
   wfreeze wid = (); wargs = no_args_info; filler = List.mem FillerAtt attr}
;
