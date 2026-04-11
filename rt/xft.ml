(* Xft *)

open Xlib.

type glyphinfo = 'a;
type xftcolor = 'a;
type xftdraw = 'a;
type xftfont = 'a;

external alloc_XftColor : unit -> xftcolor = "ML_alloc_XftColor";
external alloc_glyphinfo : unit -> glyphinfo = "ML_alloc_XGlyphInfo";

external xftColorAllocName :
  (display * visual * colormap * string * xftcolor) -> bool =
    "ML_XftColorAllocName";
external xftDrawCreate : (display * drawable * visual * colormap) -> xftdraw =
    "ML_XftDrawCreate";
external xftDrawString8 :
  (xftdraw * xftcolor * xftfont * int * int * string * int) -> unit =
    "ML_XftDrawString8";
external xftFontOpenName : (display * int * string) -> xftfont =
  "ML_XftFontOpenName";
external xftTextExtents8 :
  (display * xftfont * string * int * glyphinfo) -> unit =
    "ML_XftTextExtents8";

external get_screen_size_mm : display -> (int * int) =
  "ML_get_screen_size_mm";
