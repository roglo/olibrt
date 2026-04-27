(* $Id: pixmap.ml,v 1.5 2008/01/23 10:41:08 deraugla Exp $
 *
 * Rogloglo Toolkit: pixmaps
 *)

open Xlib;
open Std;
open Rtdecl;

value rt_create_pixmap xd width height =
  let pixmap = xCreatePixmap (xd.dpy, xd.rootw, width, height, xd.depth) in
  {pixm_xd = xd; pixmap = pixmap;
   pixm_width = width; pixm_height = height}
;

value rt_select_pixmap pix = do {
  let xd = pix.pixm_xd in xSetTile (xd.dpy, xd.gc, pix.pixmap);
  xSetFillStyle (xd.dpy, xd.gc, fillTiled)
};
