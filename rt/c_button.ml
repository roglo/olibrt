(* $Id: c_button.ml,v 1.23 2008/10/23 08:31:14 deraugla Exp $
 *
 * Rogloglo Toolkit: button widget class
 *)

open Font;
open Motif;
open Rtdecl;
open Std;
open Util;
open Xlib;
open Xft;

type button_event =
  [ ButtonEvEnter of int and int
  | ButtonEvPress of int and int and modified int
  | ButtonEvRelease of int and int and modified int
  | ButtonEvSizeChange
  | ButtonEvShortcut ]
;

type button_args = (string * option char);
type button_event_handler = widget -> button_event -> unit;

type mutable_gc = { mgc : gC; c_foreg : mutable int; c_font : mutable xfont };

type button_global_info =
  { dfont : font;
    ftfont : xftfont;
    attrs : xWindowAttributes;
    color : xftcolor;
    extents : glyphinfo;
    bgc : mutable_gc };

type button_local_info =
  { button_gi : button_global_info; bfont : font; in_popup : bool }
;

value (button_global_info, get_button_global_info) =
  dynamo_global_info "button_global_info"
    (ref (None : option button_global_info))
;

value (button_local_info, get_button_local_info) =
  dynamo_local_info "button_local_info"
    (ref (None : option button_local_info))
;

value button_border = ref 0
and button_band = ref 1
and button_bold = ref 2
and button_bold_band = ref 0
and button_font = ref "*-helvetica-bold-r-*--14-*";

value make_button_global_info xd = do {
  let dfont = rt_load_query_font xd button_font.val in
  let ftfont = xftFontOpenName (xd.dpy, xd.scr, "mono:size=12") in
  let mask = gCLineWidth lor gCCapStyle lor dfont.gc_mask in
  let xgcv = (gstr ()).xgcv in set_XGCValues_cap_style (capProjecting, xgcv);
  set_XGCValues_font (dfont.fid, xgcv);
  let bgc =
    {mgc = xCreateGC (xd.dpy, xd.rootw, mask, xgcv); c_foreg = 0;
     c_font = dfont.fid}
  in
  let attrs = alloc_XWindowAttributes () in
  let color = alloc_XftColor () in
  let _b = xftColorAllocName (xd.dpy, xd.vis, xd.cmap, "black", color) in
  let extents = alloc_glyphinfo () in
  add_ginfo xd "button" button_global_info
    {dfont = dfont; ftfont = ftfont; attrs = attrs; color = color;
     extents = extents; bgc = bgc}
};

value set_gc_foreground xd mgc att =
  let pix =
    match att with
    [ Some (ColorPn col) -> col.pixel
    | None -> xd.black
    | _ -> mgc.c_foreg ]
  in
  if mgc.c_foreg != pix then do {
    mgc.c_foreg := pix;
    xSetForeground (xd.dpy, mgc.mgc, pix)
  }
  else ()
;

value set_gc_font xd mgc w_font =
  if mgc.c_font != w_font then do {
    mgc.c_font := w_font;
    xSetFont (xd.dpy, mgc.mgc, w_font)
  }
  else ()
;

value button_draw_border xd wid in_popup inv =
  motif_border xd wid.win 0 0 wid.width wid.height
    (if in_popup then if inv then MBpop else MBflat
     else if inv then MBpush
     else MBpop)
;

value latin_1_txt xd txt =
  match xd.char_set with
  [ Latin_1 -> txt
  | Utf_8 -> latin_1_of_utf_8 txt ]
;

value button_draw xd wid li (txt, shortcut) att_val = do {
  let gi = li.button_gi in
  let left_j = att_val.left_justif in
  let band = opt_val button_band.val att_val.band_att in
  let txt = latin_1_txt xd txt in
  let (txt, s_opt, left_j) =
    match try Some (String.index txt '\t') with [ Not_found -> None ] with
    [ Some i ->
        let s1 = String.sub txt 0 i in
        let s2 = String.sub txt (i + 1) (String.length txt - i - 1) in
        (s1, Some s2, True)
    | None ->
        (txt, None, left_j) ]
  in
  let len = String.length txt in
  set_gc_font xd gi.bgc li.bfont.fid;
  let col =
    if wid.frozen then Some (ColorPn {col_xd = xd; pixel = xd.bord_pix.(0)})
    else att_val.foreg_att
  in
  set_gc_foreground xd gi.bgc col;
  let _s = xGetWindowAttributes(xd.dpy, wid.win, gi.attrs) in
  let draw =
    xftDrawCreate
      (xd.dpy, wid.win, xWindowAttributes_visual gi.attrs,
       xWindowAttributes_colormap gi.attrs)
  in
  let x = xd.motif_border + band in
  let y = wid.height - 9 (* mouais, valeur au pif *) in
(*
Printf.printf "glyphinfo width %d height %d yOff %d\n"
  (glyphinfo_width gi.extents) (glyphinfo_height gi.extents)
  (glyphinfo_yOff gi.extents);
*)
  xftDrawString8 (draw, gi.color, gi.ftfont, x, y, txt, len);
  let s_opt =
    match shortcut with
    [ Some sc -> Some (Printf.sprintf "Alt %c" sc, "Alt m")
    | None ->
        match s_opt with
        [ Some s -> Some (s, s)
        | None -> None ] ]
  in
  match s_opt with
  [ Some (s, s2) -> do {
      let slen = String.length s2 in
      xftTextExtents8 (xd.dpy, gi.ftfont, s2, slen, gi.extents);
      let x =
        wid.width - xd.motif_border - band - glyphinfo_width gi.extents
      in
      xftDrawString8 (draw, gi.color, gi.ftfont, x, y, s, slen)
    }
  | None -> () ];
};

value button_wsize (txt, shortcut) att_val xd = do {
  let gi =
    try get_button_global_info (ginfo xd "button") with _ ->
      make_button_global_info xd
  in
  let band = opt_val button_band.val att_val.band_att in
  let bfs =
    if Array.length att_val.font_att >= 1 then att_val.font_att.(0)
    else gi.dfont
  in
  let txt = latin_1_txt xd txt in
  xftTextExtents8 (xd.dpy, gi.ftfont, txt, String.length txt, gi.extents);
  let b = max 0 (opt_val button_border.val att_val.border_att) in
  let wg =
    max (opt_val 1 att_val.width_att)
      (2 * (band + xd.motif_border) + glyphinfo_width gi.extents)
  and hg =
    max (opt_val 1 att_val.height_att)
      (2 * (band + xd.motif_border) + xftFont_height gi.ftfont)
  in
  let w =
    match shortcut with
    [ Some _ -> do {
        let s = "Alt m" in
        xftTextExtents8 (xd.dpy, gi.ftfont, s, String.length s, gi.extents);
	wg + glyphinfo_width gi.extents + 20
      }
    | None ->
        match try Some (String.index txt '\t') with [ Not_found -> None ] with
        [ Some _ -> wg + xTextWidth (bfs.fs, " ", 1)
        | None -> wg ] ]
  in
  {sh_width = w; sh_height = hg; sh_border = b; base_width = w;
   base_height = hg; width_inc = -1; height_inc = -1}
};

value select_mask =
  List.fold_left (fun x y -> x lor y) 0
    [exposureMask; enterWindowMask; leaveWindowMask; buttonPressMask;
     buttonReleaseMask; structureNotifyMask; ownerGrabButtonMask]
;

value button_wcreate att_val (_, shortcut) xd pwin is_top in_popup wdesc x y
    wsh = do {
  let win = create_window xd pwin is_top x y wsh att_val select_mask in
  match att_val.backg_att with
  [ None -> motif_backg xd win
  | _ -> () ];
  let gi = get_button_global_info (ginfo xd "button") in
  let bfont =
    if Array.length att_val.font_att >= 1 then att_val.font_att.(0)
    else gi.dfont
  in
  let li = {button_gi = gi; bfont = bfont; in_popup = in_popup} in
  let info = button_local_info li in
  let wid = create_widget xd win is_top x y wsh wdesc info [] in
  match shortcut with
  [ Some s -> xd.wid_by_shortcut := [(s, wid) :: xd.wid_by_shortcut]
  | None -> () ];
  add_widget att_val.name_att win wid
};

value button_wdispatch (txt, shortcut) att_val callb wid xev =
  let xd = wid.wid_xd
  and t = xEvent_type xev in
  let li = get_button_local_info wid.info in
  if t == expose then do {
    button_draw xd wid li (txt, shortcut) att_val;
    button_draw_border xd wid li.in_popup False
  }
  else if t == configureNotify then do {
    let xev = xEvent_xconfigure xev in
    let width = xConfigureEvent_width xev
    and height = xConfigureEvent_height xev
    and border = xConfigureEvent_border_width xev in
    if width != wid.width || height != wid.height then do {
      wid.width := width;
      wid.height := height;
      xClearWindow (xd.dpy, wid.win);
      button_draw xd wid li (txt, shortcut) att_val;
      button_draw_border xd wid li.in_popup False
    }
    else ();
    wid.border := border;
    callb wid ButtonEvSizeChange
  }
  else if t == leaveNotify then
    match xd.win_but with
    [ WB_None -> ()
    | WB_Win win -> xd.win_but := WB_None
    | WB_WinBut win -> do {
        xd.win_but := WB_WinButExit win;
        button_draw_border xd wid li.in_popup False
      }
    | WB_WinButExit win -> xd.win_but := WB_None
    | WB_But -> ()
    | WB_ButWin -> xd.win_but := WB_But ]
  else if t == enterNotify then
    match xd.win_but with
    [ WB_None -> xd.win_but := WB_Win wid.win
    | WB_Win win -> ()
    | WB_WinBut win -> ()
    | WB_WinButExit win -> do {
        let same = wid.win = win in
        xd.win_but := WB_WinBut wid.win;
        if not wid.frozen then do {
          button_draw_border xd wid li.in_popup True;
          if same then ()
          else
            let xev = xEvent_xcrossing xev in
            let x = xCrossingEvent_x xev
            and y = xCrossingEvent_y xev
            and x_root = xCrossingEvent_x_root xev
            and y_root = xCrossingEvent_y_root xev in
            let ev =
              ButtonEvEnter (x_root - x - wid.border)
                (y_root - y + wid.height)
            in
            callb wid ev
        }
        else ()
      }
    | WB_But -> xd.win_but := WB_ButWin
    | WB_ButWin -> () ]
  else if t == buttonPress then
    match xd.win_but with
    [ WB_None -> ()
    | WB_Win win -> do {
        xd.win_but := WB_WinBut win;
        if not wid.frozen then do {
          button_draw_border xd wid li.in_popup True;
          let xev = xEvent_xbutton xev in
          let x = xButtonEvent_x xev
          and y = xButtonEvent_y xev
          and x_root = xButtonEvent_x_root xev
          and y_root = xButtonEvent_y_root xev in
          let ev =
            ButtonEvPress (x_root - x - wid.border) (y_root - y + wid.height)
              (item_modified (xButtonEvent_state xev)
                 (xButtonEvent_button xev))
          in
          callb wid ev
        }
        else ()
      }
    | WB_WinBut win -> ()
    | WB_WinButExit win -> ()
    | WB_But -> ()
    | WB_ButWin -> () ]
  else if t == buttonRelease then
    match xd.win_but with
    [ WB_None -> ()
    | WB_Win win -> ()
    | WB_WinBut win -> do {
        xd.win_but := WB_Win win;
        button_draw_border xd wid li.in_popup False;
        if not wid.frozen then
          let xev = xEvent_xbutton xev in
          let x = xButtonEvent_x xev
          and y = xButtonEvent_y xev
          and x_root = xButtonEvent_x_root xev
          and y_root = xButtonEvent_y_root xev in
          let state = xButtonEvent_state xev in
          let ev =
            ButtonEvRelease (x_root - x - wid.border)
              (y_root - y + wid.height)
              (item_modified state (xButtonEvent_button xev))
          in
          callb wid ev
        else ()
      }
    | WB_WinButExit win -> xd.win_but := WB_None
    | WB_But -> xd.win_but := WB_None
    | WB_ButWin -> xd.win_but := WB_Win wid.win ]
  else if t == keyPress then
    if not wid.frozen then callb wid ButtonEvShortcut else ()
  else ()
;

value button_desc attr args callb =
  let att_val = attribute_values attr in
  {wsize = button_wsize args att_val; wcreate = button_wcreate att_val args;
   wresize wid w h = ();
   wdestroy wid = do {
     let xd = wid.wid_xd in
     match xd.win_but with
     [ WB_Win win -> if win == wid.win then xd.win_but := WB_None else ()
     | _ -> () ];
     remove_widget att_val.name_att wid.win wid
   };
   wdispatch = button_wdispatch args att_val callb; wselect _ _ = ();
   wfreeze wid =
     let xd = wid.wid_xd in
     let li = get_button_local_info wid.info in
     button_draw xd wid li args att_val;
   wargs = no_args_info; filler = List.mem FillerAtt attr}
;

value menu_button attl args wid =
  let act xll yll = map_popup_widget wid xll yll 0 in
  button_desc attl args
    (fun _ ->
       fun
       [ ButtonEvPress xll yll _ -> act xll yll
       | ButtonEvEnter xll yll -> act xll yll
       | _ -> () ])
;
