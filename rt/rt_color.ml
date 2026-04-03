(* $Id: rt_color.ml,v 1.3 2010/10/08 08:09:04 deraugla Exp $
 *
 * Rogloglo Toolkit: colors
 *)

open Xlib;
open Std;
open Rtdecl;
open Util;

value cM = 65535;

value rt_closest_color =
  let xcol = mallocated_var alloc_XColor (ref None) in
  fun xd (red_val, green_val, blue_val) -> do {
    let xcol = xcol () in
    let red_val = max 0 (min cM (256 * red_val))
    and green_val = max 0 (min cM (256 * green_val))
    and blue_val = max 0 (min cM (256 * blue_val)) in
    set_XColor_red (red_val, xcol);
    set_XColor_green (green_val, xcol);
    set_XColor_blue (blue_val, xcol);
    set_XColor_flags (doRed lor doGreen lor doBlue, xcol);
    let _ = xAllocColor (xd.dpy, xd.cmap, xcol) in
    {col_xd = xd; pixel = xColor_pixel xcol}
  }
;

value rt_change_color =
  let xcol = mallocated_var alloc_XColor (ref None) in
  fun col (red_val, green_val, blue_val) -> do {
    if visual_class col.col_xd.vis = trueColor then
      failwith "rt_change_color not implemented for TrueColor visual"
    else ();
    let xd = col.col_xd
    and xcol = xcol () in
    set_XColor_pixel (col.pixel, xcol);
    let red_val = max 0 (min cM (256 * red_val))
    and green_val = max 0 (min cM (256 * green_val))
    and blue_val = max 0 (min cM (256 * blue_val)) in
    set_XColor_red (red_val, xcol);
    set_XColor_green (green_val, xcol);
    set_XColor_blue (blue_val, xcol);
    set_XColor_flags (doRed lor doGreen lor doBlue, xcol);
    xStoreColor (xd.dpy, xd.cmap, xcol)
  }
;

value rt_create_color =
  let pp =
    mallocated_var (fun _ -> (alloc_LongRef (), alloc_LongRef ())) (ref None)
  in
  fun xd (red_val, green_val, blue_val) ->
    let (pixels, plane_masks) = pp () in
    if visual_class xd.vis = pseudoColor then do {
      let pix =
        if xAllocColorCells (xd.dpy, xd.cmap, 0, plane_masks, 0, pixels, 1) ==
             0
        then
          failwith "rt_create_color"
        else longRef_value pixels
      in
      let col = {col_xd = xd; pixel = pix} in
      rt_change_color col (red_val, green_val, blue_val);
      col
    }
    else if visual_class xd.vis = trueColor then
      rt_closest_color xd (red_val, green_val, blue_val)
    else failwith "rt_create_color 1"
and rt_black_color xd = {col_xd = xd; pixel = xd.black}
and rt_white_color xd = {col_xd = xd; pixel = xd.white}
and color_pixel col = col.pixel;

value rt_free_color =
  let pp = mallocated_var alloc_LongRef (ref None) in
  fun col -> do {
    let pixels = pp () in
    let xd = col.col_xd in
    set_longRef_value (col.pixel, pixels);
    let _ : int = xFreeColors (xd.dpy, xd.cmap, pixels, 1, 0) in
    ()
  }
;

value rt_select_color col = do {
  let xd = col.col_xd in
  xSetForeground (xd.dpy, xd.gc, col.pixel);
  xSetFillStyle (xd.dpy, xd.gc, fillSolid)
};

value rt_create_gc_with_color col = do {
  let xd = col.col_xd in
  let xgcv = (gstr ()).xgcv in
  set_XGCValues_foreground (col.pixel, xgcv);
  let gc = xCreateGC (xd.dpy, xd.rootw, gCForeground, xgcv) in
  {gc_xd = xd; xgc = gc}
};

value rt_select_background_color col = do {
  let xd = col.col_xd in xSetBackground (xd.dpy, xd.gc, col.pixel);
  xSetFillStyle (xd.dpy, xd.gc, fillSolid)
};

value rt_query_color =
  let xcol = mallocated_var alloc_XColor (ref None) in
  fun xd pix -> do {
    let xcol = xcol () in set_XColor_pixel (pix, xcol);
    set_XColor_flags (doRed lor doGreen lor doBlue, xcol);
    xQueryColor (xd.dpy, xd.cmap, xcol);
    (xColor_red xcol / 256, xColor_green xcol / 256, xColor_blue xcol / 256)
  }
;

value rgb_of_hsv = Util.rgb_of_hsv;
