(* $Id: c_term.ml,v 1.19 2017/05/14 01:11:49 deraugla Exp $
 *
 * Rogloglo Toolkit: term widget class
 *)

open Font;
open Keysym;
open Mousefont;
open Rtdecl;
open Termdef;
open Util;
open Xlib;
open Xft;

type term_event =
  Termdef.term_event ==
    [ TermEvButtonPress of int and int and int and modified int
    | TermEvButtonRelease of int and int and int and modified int
    | TermEvButtonMotion of int and int and int
    | TermEvKeyPress of modified keysym
    | TermEvAnswer of string
    | TermEvExtendHistory
    | TermEvSizeChange of int and int ]
;

type term_event_handler = Termdef.term_event_handler;

type term_args = (int * int * int);

value term_font = Termdef.term_font
and term_inter = Termdef.term_inter
and term_band = Termdef.term_band
and term_blink = Termdef.term_blink
and term_border = ref 1;

value create_term_font xd font_att i =
  freeze
    (fun () ->
       if i < Array.length font_att then font_att.(i)
       else rt_load_query_font xd term_font.(i))
;

value create_gc xd fs = do {
  let gstr = gstr () in
  let _mask = fs.gc_mask in set_XGCValues_font (fs.fid, gstr.xgcv);
  xCreateGC (xd.dpy, xd.rootw, fs.gc_mask, gstr.xgcv)
};

value make_term_global_info xd =
let _ = Printf.printf "*** make_term_global_info\n" in
let _ = flush stdout in
  let ftfont = xftFontOpenName (xd.dpy, xd.scr, "mono:size=12") in
  let attrs = alloc_XWindowAttributes () in
  let color = alloc_XftColor () in
  let _b = xftColorAllocName (xd.dpy, xd.vis, xd.cmap, "black", color) in
  let fs = rt_load_query_font xd term_font.(0) in
  let _gstr = gstr () in
  let tgc = create_gc xd fs in
  add_ginfo xd "term" term_global_info
    {dfont = fs; tgc = tgc;
     ftfont = ftfont; attrs = attrs; color = color;
     c_backg = 1; c_foreg = 0; c_font = fs.fid}
;

value (term_args, get_term_args) =
  dynamo_args_info "term_args_info" (ref None)
;

value term_wsize wargs att_val xd =
  let (rows, cols, _) = (get_term_args wargs).val in
  let _gi =
    try get_term_global_info (ginfo xd "term") with _ ->
      make_term_global_info xd
  in
(**)
  let ftfont = xftFontOpenName (xd.dpy, xd.scr, "mono:size=12") in
(*
  let font =
    if Array.length att_val.font_att >= 1 then att_val.font_att.(0)
    else gi.dfont
  in
*)
  let tinter = opt_val term_inter.val att_val.inter_att in
  let tband = opt_val term_band.val att_val.band_att in
(**)
  let twidth = xftFont_width ftfont in
  let theight = xftFont_height ftfont + tinter in
(*
  let twidth = font.fwidth in
  let theight = font.fheight + tinter in
*)
  {sh_width = cols * twidth + 2 * tband;
   sh_height = rows * theight - tinter + 2 * tband;
   sh_border = opt_val term_border.val att_val.border_att;
   base_width = 2 * tband; base_height = 2 * tband - tinter;
   width_inc = twidth; height_inc = theight}
;

value select_mask =
  exposureMask lor structureNotifyMask lor keyPressMask lor
  buttonPressMask lor buttonReleaseMask lor buttonMotionMask lor
  enterWindowMask lor leaveWindowMask lor focusChangeMask
;

value term_wcreate att_val wargs callb xd pwin is_top in_popup wdesc x y
    wsh = do {
  let (_, _, nhist) = (get_term_args wargs).val in
  let {sh_width = width; sh_height = height; sh_border = border} = wsh in
  let win = create_window xd pwin is_top x y wsh att_val select_mask in
  xDefineCursor (xd.dpy, win, xCreateFontCursor (xd.dpy, xC_xterm));
  let gi = get_term_global_info (ginfo xd "term") in
(**)
  let ftfont = xftFontOpenName (xd.dpy, xd.scr, "mono:size=5") in
(*
  let font =
    if Array.length att_val.font_att >= 1 then att_val.font_att.(0)
    else gi.dfont
  in
*)
  let tband = opt_val term_band.val att_val.band_att in
  let tinter = opt_val term_inter.val att_val.inter_att in
(**)
  let twidth = xftFont_width ftfont in
  let theight = xftFont_height ftfont + tinter in
(*
  let twidth = font.fwidth in
  let theight = font.fheight + tinter in
*)
  let ncol = max 1 ((width - 2 * tband) / twidth) in
  let nrow = max 1 ((height - 2 * tband + tinter) / theight) in
  let lines =
    make_array nrow
      (fun _ ->
         {str = Gstring.create ncol; vid = Bytes.create ncol;
          foreg = Array.make ncol 0; backg = Array.make ncol 0})
  in
  let tfs =
    make_array (f_bld lor f_ita + 1) (create_term_font xd att_val.font_att)
  in
  let _s = xGetWindowAttributes(xd.dpy, win, gi.attrs) in
  let draw =
    xftDrawCreate
      (xd.dpy, win, xWindowAttributes_visual gi.attrs,
       xWindowAttributes_colormap gi.attrs)
  in
  let li =
    {term_gi = gi; draw= draw; att_val = att_val; callb = callb; tfs = tfs;
     twidth = twidth; theight = theight; tascent = 0; (* font.ascent; *)
     tdescent = (*font.descent*)0; max_history_size = nhist; lines = lines;
     nhrow = 0; nrow = nrow; ncol = ncol; shift = 0; crow = 0; ccol = 0;
     sreg1 = 0; sreg2 = ncol; vmask = Char.chr 0; foregm = 0; backgm = 0;
     vcrow = 0; vccol = 0; vvmask = Char.chr 0; vforegm = 0; vbackgm = 0;
     erow1 = 0; ecol1 = 0; erow2 = 0; ecol2 = 0; scroll_cnt = 0;
     scroll_rect = []; state = IS_normal; tabs = Bytes.create ncol;
     flags = 0}
  in
  term_soft_reset li;
  let info = term_local_info li in
  let wid = create_widget xd win is_top x y wsh wdesc info [] in
  add_widget att_val.name_att win wid
};

value term_wdestroy att_val wid = remove_widget att_val.name_att wid.win wid;

value term_graphics_expose wid li = do {
  if li.scroll_cnt == 1 then do {
    List.iter
      (fun (x, y, width, height) -> do {
         term_expose wid x 0 width wid.height;
         term_expose wid 0 y wid.width height
       })
      li.scroll_rect;
    li.scroll_rect := []
  }
  else ();
  li.scroll_cnt := max 0 (li.scroll_cnt - 1)
};

value term_reinit wid li =
  let tband = opt_val term_band.val li.att_val.band_att in
  let tinter = opt_val term_inter.val li.att_val.inter_att in
  let nrow = max 1 ((wid.height - 2 * tband + tinter) / li.theight) in
  let ncol = max 1 ((wid.width - 2 * tband) / li.twidth) in
  term_resize wid li nrow ncol
;

value term_wselect wid select =
  let li = get_term_local_info wid.info in
  if select then show_cursor wid li else hide_cursor wid li
;

value term_wdispatch callb wid xev =
  let xd = wid.wid_xd
  and t = xEvent_type xev in
  if t == expose then
    let xev = xEvent_xexpose xev in
    let x = xExposeEvent_x xev
    and y = xExposeEvent_y xev
    and width = xExposeEvent_width xev
    and height = xExposeEvent_height xev in
    term_expose wid x y width height
  else if t == graphicsExpose then do {
    let xev = xEvent_xgraphicsexpose xev in
    let li = get_term_local_info wid.info in
    let x = xGraphicsExposeEvent_x xev
    and y = xGraphicsExposeEvent_y xev
    and width = xGraphicsExposeEvent_width xev
    and height = xGraphicsExposeEvent_height xev in
    let rect = (x, y, width, height) in
    li.scroll_rect :=
      if List.mem rect li.scroll_rect then li.scroll_rect
      else [rect :: li.scroll_rect];
    if xGraphicsExposeEvent_count xev == 0 then term_graphics_expose wid li
    else ()
  }
  else if t == noExpose then
    let li = get_term_local_info wid.info in term_graphics_expose wid li
  else if t == configureNotify then do {
    let xev = xEvent_xconfigure xev in
    let width = xConfigureEvent_width xev in
    let height = xConfigureEvent_height xev in
    let border = xConfigureEvent_border_width xev in wid.border := border;
    if width <> wid.width || height <> wid.height then do {
      let li = get_term_local_info wid.info in
      wid.width := width;
      wid.height := height;
      term_reinit wid li;
      xClearWindow (xd.dpy, wid.win);
      for row = 0 to li.nrow - 1 do { term_expose_row wid li row 0 li.ncol };
      callb wid (TermEvSizeChange li.nrow li.ncol)
    }
    else ()
  }
  else if t == keyPress then
    if not wid.frozen then
      let xkey_ev = xEvent_xkey xev in
      let state = xKeyEvent_state xkey_ev in
      let ksym =
        let k = keysym_int_of_xevent xev in
        keysym_of_keysym_int k state
      in
      let ev = TermEvKeyPress ksym in
      callb wid ev
    else ()
  else if t == buttonPress then
    if not wid.frozen then
      let xev = xEvent_xbutton xev in
      let x = xButtonEvent_x xev
      and y = xButtonEvent_y xev
      and button = xButtonEvent_button xev
      and li = get_term_local_info wid.info in
      let state = xButtonEvent_state xev in
      let (row, char_col) = row_col_of_xy li x y in
      let (_, curs_col) = row_col_of_xy li (x + li.twidth / 2) y in
      let ev =
        TermEvButtonPress row char_col curs_col (item_modified state button)
      in
      callb wid ev
    else ()
  else if t == buttonRelease then
    if not wid.frozen then
      if xd.win_but == WB_None then
        let xev = xEvent_xbutton xev in
        let x = xButtonEvent_x xev
        and y = xButtonEvent_y xev in
        let button = xButtonEvent_button xev in
        let state = xButtonEvent_state xev in
        let li = get_term_local_info wid.info in
        let (row, char_col) = row_col_of_xy li x y in
        let (_, curs_col) = row_col_of_xy li (x + li.twidth / 2) y in
        let ev =
          TermEvButtonRelease row char_col curs_col
            (item_modified state button)
        in
        callb wid ev
      else ()
    else ()
  else if t == motionNotify then
    if not wid.frozen then do {
      let args = (xd.dpy, pointerMotionMask, xev) in
      while xCheckMaskEvent args <> 0 do { () };
      let xev = xEvent_xmotion xev in
      let x = xMotionEvent_x xev
      and y = xMotionEvent_y xev
      and li = get_term_local_info wid.info in
      let _gi = li.term_gi in
      let (row, char_col) = row_col_of_xy li x y in
      let (_, curs_col) = row_col_of_xy li (x + li.twidth / 2) y in
      let ev = TermEvButtonMotion row char_col curs_col in callb wid ev
    }
    else ()
  else ()
;

value term_desc attr args callb =
  let att_val = attribute_values attr in
  let wargs = term_args (ref args) in
  {wsize = term_wsize wargs att_val;
   wcreate = term_wcreate att_val wargs callb;
   wresize wid w h = (); wdestroy = term_wdestroy att_val;
   wdispatch = term_wdispatch callb; wselect = term_wselect;
   wfreeze _ = (); wargs = wargs; filler = List.mem FillerAtt attr}
;

value term_size wid =
  let li = get_term_local_info wid.info in (li.nrow, li.ncol)
;

value term_set_size wid (nrow, ncol) = do {
  let wargs = get_term_args wid.wdesc.wargs in
  let (_, _, nhist) = wargs.val in
  let li = get_term_local_info wid.info in
  wargs.val := (nrow, ncol, nhist);
  term_resize wid li nrow ncol
};

value term_set_max_history_size wid new_nhist = do {
  let wargs = get_term_args wid.wdesc.wargs in
  let (nrow, ncol, _) = wargs.val in
  let li = get_term_local_info wid.info in
  wargs.val := (nrow, ncol, new_nhist);
  li.max_history_size := new_nhist;
};

value term_current_position wid =
  let li = get_term_local_info wid.info in (li.crow, li.ccol)
;

value term_string_of_keysym wid ksym =
  let li = get_term_local_info wid.info in
  match ksym with
  [ {item = K_Ascii x; control = False} -> String.make 1 x
  | {item = K_Ascii x; control = True} ->
      String.make 1 (Char.chr (Char.code x land 31))
  | {item = K_BackSpace} -> "\008"
  | {item = K_Tab} -> "\t"
  | {item = K_Return} ->
      if flg_set li flg_newline_mode then "\013\n" else "\013"
  | {item = K_Escape} -> "\027"
  | {item = K_Delete} -> "\127"
  | {item = K_Up} ->
      if flg_set li flg_appl_curs_key then "\027OA" else "\027[A"
  | {item = K_Down} ->
      if flg_set li flg_appl_curs_key then "\027OB" else "\027[B"
  | {item = K_Right} ->
      if flg_set li flg_appl_curs_key then "\027OC" else "\027[C"
  | {item = K_Left} ->
      if flg_set li flg_appl_curs_key then "\027OD" else "\027[D"
  | {item = K_KP_Separator} ->
      if flg_set li flg_appl_num_kp then "\027Ol" else ","
  | {item = K_KP_Subtract} ->
      if flg_set li flg_appl_num_kp then "\027Om" else "-"
  | {item = K_KP_Decimal} ->
      if flg_set li flg_appl_num_kp then "\027On" else "."
  | {item = K_KP_Enter} ->
      if flg_set li flg_appl_num_kp then "\027OM" else "\013"
  | {item = K_KP_N n} ->
      if flg_set li flg_appl_num_kp then
        "\027O" ^ String.make 1 (Char.chr (Char.code 'p' + n))
      else string_of_int n
  | {item = k} -> if is_modifier k then "" else "<?>" ]
;

value term_history_size wid =
  let li = get_term_local_info wid.info in
  li.nhrow
;

value term_shift_value wid =
  let li = get_term_local_info wid.info in
  li.shift
;

value term_shift wid nb =
  let li = get_term_local_info wid.info in
  let _gi = li.term_gi in
  let nb = max 0 (min li.nhrow nb) in
  let d = nb - li.shift in
  if li.shift != nb then do {
    li.shift := nb;
    let tband = opt_val term_band.val li.att_val.band_att in
    if d < 0 then do {
      term_scroll_up wid li (-d) 0 li.nrow;
      let nb = min (-d) li.nrow in
      term_expose wid tband (tband + (li.nrow - nb) * li.theight)
        (li.ncol * li.twidth) (nb * li.theight)
    }
    else if d > 0 then do {
      term_scroll_down wid li d 0 li.nrow;
      let nb = min d li.nrow in
      term_expose wid tband tband (li.ncol * li.twidth) (nb * li.theight)
    }
    else ()
  }
  else ()
;

value term_emphasize_from = Termdef.term_emphasize_from;
value term_emphasize_to = Termdef.term_emphasize_to;
value term_emphasized_location = Termdef.term_emphasized_location;
value term_get_emphasized = Termdef.term_get_emphasized;
