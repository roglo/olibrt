(* $Id: draw.ml,v 1.12 2017/12/28 10:38:58 deraugla Exp $
 *
 * Rogloglo Toolkit: drawing routines
 *)

open Xlib;
open Std;
open Rtdecl;
open Util;

value rt_draw_point_with_gc draw gc (x, y) =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xDrawPoint (xd.dpy, draw, gc.xgc, x, y)
;

value rt_draw_point draw (x, y) =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xDrawPoint (xd.dpy, draw, xd.gc, x, y)
and rt_draw_line draw (x1, y1) (x2, y2) =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xDrawLine (xd.dpy, draw, xd.gc, x1, y1, x2, y2)
;

value pts =
  let plen = ref 20 in
  let ptsr = ref (alloc_XPoint plen.val) in
  fun len -> do {
    if len > plen.val then do {
      free ptsr.val;
      ptsr.val := alloc_XPoint len;
      plen.val := len
    }
    else ();
    ptsr.val
  }
;

value rt_draw_lines draw points = do {
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  let pts = pts (List.length points) in
  do_list_i
    (fun i (x, y) -> do {
       let ii = i in set_XPoint_x (x, pts, ii);
       set_XPoint_y (y, pts, ii)
     })
    0 points;
  xDrawLines (xd.dpy, draw, xd.gc, pts, List.length points, coordModeOrigin)
}
and rt_fill_polygon draw points = do {
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  let pts = pts (List.length points) in
  do_list_i
    (fun i (x, y) -> do {
       let ii = i in set_XPoint_x (x, pts, ii);
       set_XPoint_y (y, pts, ii)
     })
    0 points;
  xFillPolygon
    (xd.dpy, draw, xd.gc, pts, List.length points, convex, coordModeOrigin)
};

value rt_fill_rectangle draw (x, y, width, height) =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xFillRectangle (xd.dpy, draw, xd.gc, x, y, width, height)
and rt_draw_rectangle draw (x, y, width, height) =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xDrawRectangle (xd.dpy, draw, xd.gc, x, y, width, height)
and rt_fill_arc draw (x, y, width, height, a1, a2) =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xFillArc (xd.dpy, draw, xd.gc, x, y, width, height, a1, a2)
and rt_draw_arc draw (x, y, width, height, a1, a2) =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xDrawArc (xd.dpy, draw, xd.gc, x, y, width, height, a1, a2)
and rt_clear_area wid (x, y, width, height) =
  let xd = wid.wid_xd in xClearArea (xd.dpy, wid.win, x, y, width, height, 0)
and rt_clear_widget wid =
  let xd = wid.wid_xd in xClearWindow (xd.dpy, wid.win)
and rt_erase_draw_string draw (x, y) str =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  let str =
    match xd.char_set with
    [ Latin_1 -> str
    | Utf_8 -> latin_1_of_utf_8 str ]
  in
  xDrawImageString (xd.dpy, draw, xd.gc, x, y, str, String.length str)
and rt_draw_string draw (x, y) str =
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  let str =
    match xd.char_set with
    [ Latin_1 -> str
    | Utf_8 -> latin_1_of_utf_8 str ]
  in
  xDrawString (xd.dpy, draw, xd.gc, x, y, str, String.length str)
;

value rt_copy_area draw1 draw2 (src_x, src_y, width, height)
    (dst_x, dst_y) = do {
  let (xd1, draw1) =
    match draw1 with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  and (xd2, draw2) =
    match draw2 with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  if xd1 != xd2 then
    failwith "rt_copy_area: can't copy between different displays"
  else ();
  let xd = xd1 in
  xCopyArea
    (xd.dpy, draw1, draw2, xd.gc, src_x, src_y, width, height, dst_x, dst_y)
};

value rt_set_line_width xd lw = do {
  xd.line_width := lw;
  xSetLineAttributes (xd.dpy, xd.gc, lw, lineSolid, capRound, joinMiter)
};

value rt_set_dashes xd dash_offset dash_list = do {
  let n = List.length dash_list in
  let s = Bytes.create n in
  loop 0 dash_list where rec loop i =
    fun
    [ [] -> ()
    | [d :: dl] -> do {
        if d < 0 || d > 255 then invalid_arg "rt_set_dashes" else ();
        Bytes.set s i (Char.chr d);
        loop (i + 1) dl
      } ];
  xSetLineAttributes
    (xd.dpy, xd.gc, xd.line_width, lineOnOffDash, capRound, joinMiter);
  xSetDashes (xd.dpy, xd.gc, dash_offset, Bytes.to_string s, n)
};

value rt_set_backing_store =
  let xswa = mallocated_var alloc_XSetWindowAttributes (ref None) in
  fun wid -> do {
    let xswa = xswa () in
    set_XSetWindowAttributes_backing_store (always, xswa);
    xChangeWindowAttributes (wid.wid_xd.dpy, wid.win, cWBackingStore, xswa)
  }
;
