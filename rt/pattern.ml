(* $Id: pattern.ml,v 1.5 2008/01/23 10:41:08 deraugla Exp $)
 *
 * Rogloglo Toolkit: patterns
 *)

open Xlib;
open Std;
open Rtdecl;

type pattern = { patt_xd : xdata; pattern : xpixmap };

value rt_create_pattern xd data width height = do {
  if visual_class xd.vis = trueColor then do {
    Printf.eprintf "\
<W> rt_create_pattern seems to be bugged in Xlib for TrueColor displays
";
    flush stderr
  }
  else ();
  let pixm = xCreateBitmapFromData (xd.dpy, xd.rootw, data, width, height) in
  {patt_xd = xd; pattern = pixm}
};

value rt_select_pattern patt (ts_x_origin, ts_y_origin) = do {
  let xd = patt.patt_xd in xSetStipple (xd.dpy, xd.gc, patt.pattern);
  xSetFillStyle (xd.dpy, xd.gc, fillOpaqueStippled);
  xSetTSOrigin (xd.dpy, xd.gc, ts_x_origin, ts_y_origin)
};

value rt_select_pattern_mask patt (ts_x_origin, ts_y_origin) = do {
  let xd = patt.patt_xd in xSetStipple (xd.dpy, xd.gc, patt.pattern);
  xSetFillStyle (xd.dpy, xd.gc, fillStippled);
  xSetTSOrigin (xd.dpy, xd.gc, ts_x_origin, ts_y_origin)
};

value rt_select_pattern_clip patt (clip_x_origin, clip_y_origin) = do {
  let xd = patt.patt_xd in xSetClipMask (xd.dpy, xd.gc, patt.pattern);
  xSetClipOrigin (xd.dpy, xd.gc, clip_x_origin, clip_y_origin)
};

value rt_unselect_pattern_clip xd = xSetClipMask (xd.dpy, xd.gc, xNone);

