(* $Id: font.ml,v 1.5 2008/01/23 10:41:08 deraugla Exp $ *)

open Std;
open Xlib;
open Rtdecl;

value make_font xd fs mask =
  let ascent = xFontStruct_ascent fs
  and descent = xFontStruct_descent fs in
  {font_xd = xd; fs = fs; fid = xFontStruct_fid fs; gc_mask = mask;
   ascent = ascent; descent = descent; fwidth = xTextWidth (fs, "m", 1);
   fheight = ascent + descent}
;

value rt_load_query_font xd fname =
  try hash_assoc fname xd.font_by_name with _ ->
    do {
      let font =
        let fs = xLoadQueryFont (xd.dpy, fname) in
        if is_null_XFontStruct fs then do {
          let xid = xGContextFromGC (xDefaultGC (xd.dpy, xd.scr)) in
          let fs = xQueryFont (xd.dpy, xid) in
          xd.end_func := [fun () -> xFreeFontInfoSpec fs :: xd.end_func];
          make_font xd fs 0
        }
        else do {
          xd.end_func := [fun () -> xFreeFont (xd.dpy, fs) :: xd.end_func];
          make_font xd fs gCFont
        }
      in
      hash_add_assoc (fname, font) xd.font_by_name;
      font
    }
;

value rt_select_font font =
  let xd = font.font_xd in xSetFont (xd.dpy, xd.gc, font.fid)
;

value rt_text_width font str = xTextWidth (font.fs, str, String.length str);

value rt_font_size font = font.fheight;
