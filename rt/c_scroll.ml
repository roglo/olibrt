(* $Id: c_scroll.ml,v 1.15 2008/04/16 09:24:46 deraugla Exp $
 *
 * Rogloglo Toolkit: scroll bar widget class
 *)

open Motif;
open Mousefont;
open Rtdecl;
open Std;
open Util;
open Xlib;

type scroll_event =
  [ ScrollEvArrowPress of int
  | ScrollEvArrowRelease of int
  | ScrollEvButtonPress of modified int and int
  | ScrollEvButtonRelease
  | ScrollEvButtonMotion of int
  | ScrollEvSizeChange ]
;

type scroll_args = (orientation * int * int * int);
type scroll_event_handler = widget -> scroll_event -> unit;

type scroll_global_info = { scroll_width : int; scroll_band : int }
and scroll_local_info =
  { orient : orientation;
    vmin : int;
    vmax : int;
    bsize : mutable int;
    band : int;
    vcur : mutable int;
    bwidth : mutable int;
    bheight : mutable int;
    iwidth : mutable int;
    iheight : mutable int;
    iwin : window;
    bwin : window;
    awin : array window }
;

value (scroll_global_info, get_scroll_global_info) =
  dynamo_global_info "scroll_global_info"
    (ref (None : option scroll_global_info))
and (scroll_local_info, get_scroll_local_info) =
  dynamo_local_info "scroll_local_info"
    (ref (None : option scroll_local_info))
;

value (scroll_args, get_scroll_args) =
  dynamo_args_info "bar_args_info" (ref None)
;

value scroll_set wid val0 = do {
  let li = get_scroll_local_info wid.info
  and xd = wid.wid_xd in
  li.vcur := val0;
  let (x, y) =
    match li.orient with
    [ Vertical ->
        (li.band,
         nround (li.iheight * (val0 - li.vmin - li.bsize))
           (li.vmax - li.vmin))
    | Horizontal ->
        (nround (li.iwidth * (val0 - li.vmin - li.bsize)) (li.vmax - li.vmin),
         li.band)
    | InDepth -> failwith "bad scroll param InDepth" ]
  in
  xMoveWindow (xd.dpy, li.bwin, x, y)
}
and scroll_val wid = (get_scroll_local_info wid.info).vcur;

value get_or_make_scroll_global_info xd =
  try get_scroll_global_info (ginfo xd "scroll") with _ ->
    let v = get_default xd.dpy "scrollWidth" "" in
    let scroll_width = try int_of_string v with _ -> 9 in
    let v = get_default xd.dpy "scrollBand" "" in
    let scroll_band = try int_of_string v with _ -> 1 in
    add_ginfo xd "scroll" scroll_global_info
      {scroll_width = scroll_width; scroll_band = scroll_band}
;

value scroll_val_of_coord wid li ref_args xev f_x f_y =
  let (orient, vmin, vmax, _) = ref_args.val in
  let val0 =
    vmin +
    (match orient with
     [ Vertical -> nround (f_y xev * (vmax - vmin)) li.iheight
     | Horizontal -> nround (f_x xev * (vmax - vmin)) li.iwidth
     | InDepth -> failwith "bad scroll param InDepth" ])
  in
  max vmin (min val0 vmax)
;

value scroll_wsize att_val ref_args xd =
  let (orient, vmin, vmax, bsize) = ref_args.val in
  let gi = get_or_make_scroll_global_info xd in
  let band = opt_val gi.scroll_band att_val.band_att in
  let mb = xd.motif_border in
  let (iw, ih) =
    match orient with
    [ Vertical -> (gi.scroll_width, 2 * gi.scroll_width + 4 * mb + 1)
    | Horizontal -> (2 * gi.scroll_width + 4 * mb + 1, gi.scroll_width)
    | InDepth -> failwith "bad scroll param InDepth" ]
  in
  let w = iw + 2 * band + 4 * mb in
  let h = ih + 2 * band + 4 * mb in
  {sh_width = w; sh_height = h; sh_border = 0; base_width = w;
   base_height = h; width_inc = -1; height_inc = -1}
;

value protect_size (width, height) = (max 1 width, max 1 height);

value get_bar_rect xd band ref_args iwidth iheight =
  let (orient, vmin, vmax, bsize) = ref_args.val in
  let (x, y, width, height) =
    match orient with
    [ Vertical ->
        let width = iwidth - 2 * band in
        let height =
          if bsize == 0 then iheight
          else nround (iheight * bsize) (vmax - vmin)
        in
        (band, 0, width, height)
    | Horizontal ->
        let width =
          if bsize == 0 then iwidth else nround (iwidth * bsize) (vmax - vmin)
        in
        let height = iheight - 2 * band in (0, band, width, height)
    | InDepth -> failwith "bad scroll param InDepth" ]
  in
  let (width, height) = protect_size (width, height) in
  (x, y, width, height)
;

value get_iwin_rect xd band orient width height =
  let gi = get_scroll_global_info (ginfo xd "scroll") in
  let x = xd.motif_border in
  let y = xd.motif_border in
  let width = width - 2 * xd.motif_border in
  let height = height - 2 * xd.motif_border in
  let (x, y, width, height) =
    match orient with
    [ Vertical ->
        (x, y + band + gi.scroll_width + 2 * xd.motif_border, width,
         height - 2 * (band + gi.scroll_width + 2 * xd.motif_border))
    | Horizontal ->
        (x + band + gi.scroll_width + 2 * xd.motif_border, y,
         width - 2 * (band + gi.scroll_width + 2 * xd.motif_border), height)
    | InDepth -> failwith "bad scroll param InDepth" ]
  in
  let (width, height) = protect_size (width, height) in
  (x, y, width, height)
;

value get_awin_rect xd band orient i width height =
  let gi = get_scroll_global_info (ginfo xd "scroll") in
  let k = band + gi.scroll_width + 2 * xd.motif_border in
  let (x, y, width, height) =
    match orient with
    [ Vertical ->
        let y =
          if i == 0 then xd.motif_border else height - xd.motif_border - k
        in
        (xd.motif_border, y, width - 2 * xd.motif_border, k)
    | Horizontal ->
        let x =
          if i == 0 then xd.motif_border else width - xd.motif_border - k
        in
        (x, xd.motif_border, k, height - 2 * xd.motif_border)
    | InDepth -> failwith "bad scroll param InDepth" ]
  in
  let (width, height) = protect_size (width, height) in (x, y, width, height)
;

value scroll_wcreate att_val ref_args xd pwin is_top in_popup wdesc x y
    wsh = do {
  let (orient, vmin, vmax, bsize) = ref_args.val in
  let {sh_width = width; sh_height = height; sh_border = border} = wsh in
  let win =
    create_window xd pwin is_top x y wsh att_val
      (exposureMask lor structureNotifyMask)
  in
  let bsize = if bsize = 0 then vmax - vmin else bsize in
  let gi = get_scroll_global_info (ginfo xd "scroll") in
  let band = opt_val gi.scroll_band att_val.band_att in
  let make_awin i = do {
    let (lx, ly, lwidth, lheight) =
      get_awin_rect xd band orient i width height
    in
    let awin =
      xCreateSimpleWindow
        (xd.dpy, win, lx, ly, lwidth, lheight, 0, xd.black, xd.white)
    in
    motif_backg xd awin;
    xSelectInput
      (xd.dpy, awin, exposureMask lor buttonPressMask lor buttonReleaseMask);
    awin
  }
  in
  let awin = [| make_awin 0; make_awin 1 |] in
  let (lx, ly, iwidth, iheight) = get_iwin_rect xd band orient width height in
  let iwin =
    xCreateSimpleWindow
      (xd.dpy, win, lx, ly, iwidth, iheight, 0, xd.black, xd.white)
  in
  motif_backg xd iwin;
  xSelectInput
    (xd.dpy, iwin,
     buttonPressMask lor buttonReleaseMask lor buttonMotionMask);
  let (lx, ly, lwidth, lheight) =
    get_bar_rect xd band ref_args iwidth iheight
  in
  let bwin =
    xCreateSimpleWindow
      (xd.dpy, iwin, lx, ly, lwidth, lheight, 0, xd.white, xd.black)
  in
  motif_backg xd bwin;
  xSelectInput (xd.dpy, bwin, exposureMask);
  xMapSubwindows (xd.dpy, iwin);
  xMapSubwindows (xd.dpy, win);
  let info =
    scroll_local_info
      {orient = orient; vmin = vmin; vmax = vmax; bsize = bsize;
       vcur = vmin + bsize; band = band; bwidth = lwidth; bheight = lheight;
       iwidth = iwidth; iheight = iheight; iwin = iwin; bwin = bwin;
       awin = awin}
  in
  let wid = create_widget xd win is_top x y wsh wdesc info [] in
  let _ = add_widget None awin.(0) wid in
  let _ = add_widget None awin.(1) wid in
  let _ = add_widget None iwin wid in
  let _ = add_widget None bwin wid in add_widget att_val.name_att win wid
};

value scroll_expose_arrow xd wid li orient i inv =
  let gi = get_scroll_global_info (ginfo xd "scroll") in
  let k = 2 * xd.motif_border + gi.scroll_width in
  let (dir, x, y, width, height) =
    match orient with
    [ Vertical ->
        (if i == 0 then MDup else MDdown, li.band,
         if i == 0 then li.band else 0, li.iwidth - 2 * li.band, k)
    | Horizontal ->
        (if i == 0 then MDleft else MDright, if i == 0 then li.band else 0,
         li.band, k, li.iheight - 2 * li.band)
    | InDepth -> failwith "bad scroll param InDepth" ]
  in
  motif_arrow xd li.awin.(i) x y width height dir
    (if inv then MBpush else MBpop)
;

value scroll_expose_backg xd wid =
  motif_border xd wid.win 0 0 wid.width wid.height MBpush
;

value scroll_expose_bar xd wid =
  let li = get_scroll_local_info wid.info in
  motif_border xd li.bwin 0 0 li.bwidth li.bheight MBpop
;

value scroll_wdispatch ref_args callb wid xev =
  let (orient, _, _, _) = ref_args.val in
  let t = xEvent_type xev
  and xd = wid.wid_xd in
  if t == configureNotify then do {
    let xev = xEvent_xconfigure xev in
    let width = xConfigureEvent_width xev
    and height = xConfigureEvent_height xev
    and border = xConfigureEvent_border_width xev in
    wid.border := border;
    if width <> wid.width || height <> wid.height then do {
      let li = get_scroll_local_info wid.info in
      wid.width := width;
      wid.height := height;
      let (x, y, iwidth, iheight) =
        get_iwin_rect xd li.band orient width height
      in
      li.iwidth := iwidth;
      li.iheight := iheight;
      xMoveResizeWindow (xd.dpy, li.iwin, x, y, iwidth, iheight);
      for i = 0 to 1 do {
        let (x, y, width, height) =
          get_awin_rect xd li.band orient i width height
        in
        xMoveResizeWindow (xd.dpy, li.awin.(i), x, y, width, height);
      };
      let (x, y, bwidth, bheight) =
        get_bar_rect xd li.band ref_args iwidth iheight
      in
      li.bwidth := bwidth;
      li.bheight := bheight;
      xMoveResizeWindow (xd.dpy, li.bwin, x, y, bwidth, bheight);
      callb wid ScrollEvSizeChange
    }
    else ()
  }
  else if t == buttonPress then
    if not wid.frozen then
      let win = xAnyEvent_window (xEvent_xany xev) in
      let xev = xEvent_xbutton xev
      and li = get_scroll_local_info wid.info in
      let but = xButtonEvent_button xev in
      if win == li.awin.(0) then do {
        scroll_expose_arrow xd wid li orient 0 True;
        callb wid (ScrollEvArrowPress 1)
      }
      else if win == li.awin.(1) then do {
        scroll_expose_arrow xd wid li orient 1 True;
        callb wid (ScrollEvArrowPress 2)
      }
      else
        let val0 =
          scroll_val_of_coord wid li ref_args xev xButtonEvent_x
            xButtonEvent_y
        in
        let ev =
          ScrollEvButtonPress (item_modified (xButtonEvent_state xev) but)
            val0
        in
        callb wid ev
    else ()
  else if t == buttonRelease then
    let win = xAnyEvent_window (xEvent_xany xev) in
    let li = get_scroll_local_info wid.info in
    if win == li.awin.(0) then do {
      scroll_expose_arrow xd wid li orient 0 False;
      callb wid (ScrollEvArrowRelease 1)
    }
    else if win == li.awin.(1) then do {
      scroll_expose_arrow xd wid li orient 1 False;
      callb wid (ScrollEvArrowRelease 2)
    }
    else callb wid ScrollEvButtonRelease
  else if t == expose then
    let li = get_scroll_local_info wid.info in
    let win = xAnyEvent_window (xEvent_xany xev) in
    if win == wid.win then scroll_expose_backg xd wid
    else if win == li.awin.(0) then
      scroll_expose_arrow xd wid li orient 0 False
    else if win == li.awin.(1) then
      scroll_expose_arrow xd wid li orient 1 False
    else scroll_expose_bar xd wid
  else if t == motionNotify then do {
    let cargs = (xd.dpy, buttonMotionMask, xev) in
    while xCheckMaskEvent cargs != 0 do { () };
    if not wid.frozen then
      let xev = xEvent_xmotion xev in
      let li = get_scroll_local_info wid.info in
      let val0 =
        scroll_val_of_coord wid li ref_args xev xMotionEvent_x xMotionEvent_y
      in
      let ev = ScrollEvButtonMotion val0 in callb wid ev
    else ()
  }
  else ()
;

value scroll_desc attr args callb =
  let att_val = attribute_values attr in
  let ref_args = ref args in
  {wsize = scroll_wsize att_val ref_args;
   wcreate = scroll_wcreate att_val ref_args;
   wresize wid w h = ();
   wdestroy wid = remove_widget att_val.name_att wid.win wid;
   wdispatch = scroll_wdispatch ref_args callb; wselect _ _ = ();
   wfreeze wid = (); wargs = scroll_args ref_args;
   filler = List.mem FillerAtt attr}
;

value scroll_set_bar_size wid bsize = do {
  let wargs = get_scroll_args wid.wdesc.wargs in
  let (orient, vmin, vmax, _) = wargs.val in
  let bsize = if bsize <= 0 then vmax - vmin else bsize in
  wargs.val := (orient, vmin, vmax, bsize);
  let xd = wid.wid_xd in
  let li = get_scroll_local_info wid.info in
  li.bsize := bsize;
  let width = wid.width in
  let height = wid.height in
  let (x, y, iw, ih) = get_iwin_rect xd li.band orient width height in
  li.iwidth := iw;
  li.iheight := ih;
  xMoveResizeWindow (xd.dpy, li.iwin, x, y, iw, ih);
  let (x, y, bw, bh) = get_bar_rect xd li.band wargs iw ih in
  li.bwidth := bw;
  li.bheight := bh;
  xMoveResizeWindow (xd.dpy, li.bwin, x, y, bw, bh);
};
