(* $Id: rt_image.ml,v 1.3 2009/01/05 23:03:34 deraugla Exp $
 *
 * Rogloglo Toolkit: images
 *)

open Xlib;
open Std;
open Rtdecl;

(*
type image = { ximage : xImage; mlimage : mLimage; im_data : string };
*)
type image = { ximage : xImage; im_data : string };
(**)

value rt_image_data_len xd width height depth =
  if depth == 1 then (width + 7) / 8 * height
  else if visual_class xd.vis = trueColor then width * 4 * height
  else width * height
;

value rt_create_image xd data width height depth = do {
  let image =
    xCreateImageNullData
      (xd.dpy, xd.vis, depth, if depth = 1 then xYBitmap else zPixmap, 0,
       width, height, 8, 0)
  in
  let len = rt_image_data_len xd width height depth in
  if String.length data < len then
    failwith
      ("rt_create_image: data length should be at least " ^ string_of_int len)
  else ();
  set_XImage_bitmap_bit_order (lSBFirst, image);
  set_XImage_byte_order (lSBFirst, image);
(*
  (* I think the mLimage_of_XImage is bugged and trigger memory faults
     do I delete this call; drawback : images are not garbage collected *)
  {ximage = image; mlimage = mLimage_of_XImage image; im_data = data}
*)
  {ximage = image; im_data = data}
(**)
};

value rt_put_image draw image (s_x, s_y, width, height) (d_x, d_y) =
  let ximage = image.ximage
  and im_data = image.im_data in
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  xPutImageWithData
    (xd.dpy, draw, xd.gc, ximage, im_data, s_x, s_y, d_x, d_y, width, height)
;

value rt_get_image draw image (s_x, s_y, width, height) (d_x, d_y) =
  let ximage = image.ximage
  and im_data = image.im_data in
  let (xd, draw) =
    match draw with
    [ WidgetDr wid -> (wid.wid_xd, wid.win)
    | PixmapDr pixm -> (pixm.pixm_xd, pixm.pixmap) ]
  in
  let _ =
    xGetSubImageWithData
      (xd.dpy, draw, s_x, s_y, width, height, 0xFFFFFF, zPixmap, ximage,
       im_data, d_x, d_y)
  in
  ()
;

value rt_get_pixel im (x, y) =
  xGetPixelWithData (im.ximage, im.im_data, x, y)
;

value rt_put_pixel im (x, y, pix) =
  xPutPixelWithData (im.ximage, im.im_data, x, y, pix)
;
