(* $Id: mouse.ml,v 1.5 2008/01/23 10:41:08 deraugla Exp $ *)

open Std;
open Xlib;
open Rtdecl;
open Util;

value cM = 65535;

value make_xcolor (red_val, green_val, blue_val) xcol = do {
  let red_val = max 0 (min cM (256 * red_val))
  and green_val = max 0 (min cM (256 * green_val))
  and blue_val = max 0 (min cM (256 * blue_val)) in
  set_XColor_red (red_val, xcol);
  set_XColor_green (green_val, xcol);
  set_XColor_blue (blue_val, xcol);
  set_XColor_flags (doRed lor doGreen lor doBlue, xcol)
};

value rt_create_bitmap_mouse =
  let foreg_col = mallocated_var alloc_XColor (ref None)
  and backg_col = mallocated_var alloc_XColor (ref None) in
  fun xd src mask width height foreg backg x y -> do {
    let src_pixm =
      xCreateBitmapFromData (xd.dpy, xd.rootw, src, width, height)
    and mask_pixm =
      xCreateBitmapFromData (xd.dpy, xd.rootw, mask, width, height)
    and foreg_col = foreg_col ()
    and backg_col = backg_col () in
    make_xcolor foreg foreg_col;
    make_xcolor backg backg_col;
    let cursor =
      xCreatePixmapCursor
        (xd.dpy, src_pixm, mask_pixm, foreg_col, backg_col, x, y)
    in
    xFreePixmap (xd.dpy, src_pixm);
    xFreePixmap (xd.dpy, mask_pixm);
    {curs_xd = xd; cursor = cursor}
  }
;

value rt_create_font_mouse xd c =
  {curs_xd = xd; cursor = xCreateFontCursor (xd.dpy, c)}
;

value rt_select_mouse wid curs =
  let xd = wid.wid_xd in xDefineCursor (xd.dpy, wid.win, curs.cursor)
;

value rt_unselect_mouse wid =
  let xd = wid.wid_xd in xUndefineCursor (xd.dpy, wid.win)
;
