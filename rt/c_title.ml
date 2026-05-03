(* $Id: c_title.ml,v 1.14 2010/02/12 16:20:30 deraugla Exp $
 *
 * Rogloglo Toolkit: title widget class
 *)

open Xlib;
open Xft;
open Std;
open Rtdecl;
open Font;
open Util;

type title_event = [ TitleEvButtonPress of modified int ];
type title_args = string;
type title_event_handler = widget -> title_event -> unit;

type title_global_info =
  { ftfont : xftfont;
    attrs : xWindowAttributes;
    tascent : int; tdescent : int;
    color : xftcolor; extents : glyphinfo;
    gc_title : gC }
;
type title_local_info = { draw : xftdraw; att_val : attribute_values };

value (title_global_info, get_title_global_info) =
  dynamo_global_info "title_global_info"
    (ref None : ref (option title_global_info))
and (title_local_info, get_title_local_info) =
  dynamo_local_info "title_local_info"
    (ref None : ref (option title_local_info))
;

value (title_args, get_title_args) =
  dynamo_args_info "title_args_info" (ref None)
;

value title_border = ref 1;
value title_band = ref 2;
value title_font = ref "mono:size=12";

value latin_1_txt xd txt =
  match xd.char_set with
  [ Latin_1 -> txt
  | Utf_8 -> latin_1_of_utf_8 txt ]
;

value make_global_info xd = do {
  let ftfont = xftFontOpenName (xd.dpy, xd.scr, title_font.val) in
  let attrs = alloc_XWindowAttributes () in
  let tascent = xftFont_ascent ftfont in
  let tdescent = xftFont_descent ftfont in
  let color = alloc_XftColor () in
  let _b = xftColorAllocName (xd.dpy, xd.vis, xd.cmap, "black", color) in
  let extents = alloc_glyphinfo () in
(*
  let mask = gCForeground lor gCBackground lor fs.gc_mask in
*)
  let gstr = gstr () in
(*
  set_XGCValues_font (fs.fid, gstr.xgcv);
  set_XGCValues_foreground (xd.black, gstr.xgcv);
  set_XGCValues_background (xd.white, gstr.xgcv);
*)
  let gc_title = xCreateGC (xd.dpy, xd.rootw, 0, gstr.xgcv) in
  add_ginfo xd "title" title_global_info
    {ftfont = ftfont; attrs = attrs; tascent = tascent; tdescent = tdescent;
     color = color; extents = extents; gc_title = gc_title}
};

value expose_title xd wid txt = do {
  let gi = get_title_global_info (ginfo xd "title") in
  let li = get_title_local_info wid.info in
  xClearWindow (xd.dpy, wid.win);
  let txt = latin_1_txt xd txt in
  let len = String.length txt in
  let w_foreg =
    match li.att_val.foreg_att with
    [ Some (ColorPn c) -> c.pixel
    | _ -> xd.black ]
  in
  xSetForeground (xd.dpy, gi.gc_title, w_foreg);
  let slen = String.length txt in
  xftTextExtents8 (xd.dpy, gi.ftfont, txt, slen, gi.extents);
  let x = (wid.width - glyphinfo_width gi.extents) / 2 in
  let y = (wid.height + gi.tascent - gi.tdescent) / 2 in
  xftDrawString8 (li.draw, gi.color, gi.ftfont, x, y, txt, len);
(*
  let x = (wid.width - xTextWidth (gi.tfs.fs, txt, len)) / 2 in
  let y = (wid.height + gi.tfs.ascent - gi.tfs.descent) / 2 in
  xDrawString
    (xd.dpy, wid.win, gi.gc_title,
     (wid.width - xTextWidth (gi.tfs.fs, txt, len)) / 2,
     (wid.height + gi.tfs.ascent - gi.tfs.descent) / 2, txt, len)
*)
};

value title_desc attr args callb =
  let att_val = attribute_values attr in
  let args_ref = ref args in
  {wsize xd = do {
     let gi =
       try get_title_global_info (ginfo xd "title") with _ ->
         make_global_info xd
     in
     let txt = args_ref.val in
     let txt = latin_1_txt xd txt in
     xftTextExtents8 (xd.dpy, gi.ftfont, txt, String.length txt, gi.extents);
     let w =
       max (opt_val 1 att_val.width_att)
         (2 * title_band.val + glyphinfo_width gi.extents)
     in
     let h =
       max (opt_val 1 att_val.height_att)
         (2 * title_band.val + glyphinfo_height gi.extents)
     in
     let b = opt_val title_border.val att_val.border_att in
(*
     let w =
       max (opt_val 1 att_val.width_att)
         (2 * title_band.val + xTextWidth (gi.tfs.fs, txt, String.length txt))
     and h =
       max (opt_val 1 att_val.height_att)
         (2 * title_band.val + gi.tfs.fheight)
     and b = opt_val title_border.val att_val.border_att in
*)
     {sh_width = w; sh_height = h; sh_border = b; base_width = w;
      base_height = h; width_inc = -1; height_inc = -1};
    };
   wcreate xd pwin is_top in_popup wdesc x y wsh =
     let win =
       create_window xd pwin is_top x y wsh att_val
         (exposureMask lor structureNotifyMask lor buttonPressMask)
     in
     let gi = get_title_global_info (ginfo xd "title") in
     let _s = xGetWindowAttributes(xd.dpy, win, gi.attrs) in
     let draw =
       xftDrawCreate
         (xd.dpy, win, xWindowAttributes_visual gi.attrs,
          xWindowAttributes_colormap gi.attrs)
     in
     let li = {draw = draw; att_val = att_val} in
     let info = title_local_info li in
     let wid = create_widget xd win is_top x y wsh wdesc info [] in
     add_widget att_val.name_att win wid;
   wresize wid w h = ();
   wdestroy wid = remove_widget att_val.name_att wid.win wid;
   wdispatch wid xev =
     let xd = wid.wid_xd in
     let t = xEvent_type xev in
     let txt = args_ref.val in
     if t = expose then expose_title xd wid txt
     else if t = configureNotify then do {
       let xev = xEvent_xconfigure xev in
       let width = xConfigureEvent_width xev
       and height = xConfigureEvent_height xev
       and border = xConfigureEvent_border_width xev in
       wid.width := width;
       wid.height := height;
       wid.border := border
     }
     else if t = buttonPress then
       let xev = xEvent_xbutton xev in
       let ev =
         TitleEvButtonPress
           (item_modified (xButtonEvent_state xev) (xButtonEvent_button xev))
       in
       callb wid ev
     else ();
   wselect _ _ = (); wfreeze wid = (); wargs = title_args (args_ref);
   filler = List.mem FillerAtt attr}
;

value title_change wid s = do {
  let wargs = get_title_args wid.wdesc.wargs in
  wargs.val := s;
  expose_title wid.wid_xd wid s;
};
