(* $Id: xlib.ml,v 1.17 2017/12/28 10:38:58 deraugla Exp $ *)

type drawable = int;

type display = 'a
and colormap = 'a
and gC = 'a
and visual = 'a
and xAtom = 'a
and xEvent = 'a
and xFontStruct = 'a
and xFontSet = 'a
and xfont = 'a
and xSetWindowAttributes = 'a
and xGCValues = 'a
and xWindowChanges = 'a
and xColor = 'a
and xWMHints = 'a
and xAnyEvent = 'a
and xImage = 'a
and xPoint = 'a
and xCrossingEvent = 'a
and xButtonEvent = 'a
and xClientMessageEvent = 'a
and xConfigureEvent = 'a
and xGraphicsExposeEvent = 'a
and xExposeEvent = 'a
and xKeyEvent = 'a
and xMappingEvent = 'a
and xMotionEvent = 'a
and xFocusChangeEvent = 'a
and xSizeHints = 'a
and longRef = 'a
and cursor = 'a
and xKeyboardControl = 'a
and xKeyboardState = 'a
and xID = 'a;
type window = drawable
and xpixmap = drawable;
type gContext = xID;
type atom = int
and time = int
and keySym = int
and status = int;
type c_String = 'a;
type mLimage = 'a;
type fd_set = 'a;

exception X_failure of string;
exception X_io_error of string and string;

external xAllocColor :
  (display * colormap * xColor) -> status = "ML_XAllocColor";
external xAllocColorCells :
  (display * colormap * int * longRef * int * longRef * int) ->
    status = "ML_XAllocColorCells";
external xAllocNamedColor :
  (display * colormap * string * xColor * xColor) ->
    status = "ML_XAllocNamedColor";
external xBell : (display * int) -> unit = "ML_XBell";
external xBlackPixel : (display * int) -> int = "ML_XBlackPixel";
external xChangeKeyboardControl :
  (display * int * xKeyboardControl) -> unit = "ML_XChangeKeyboardControl";
external xChangeWindowAttributes :
  (display * window * int * xSetWindowAttributes) ->
    unit = "ML_XChangeWindowAttributes";
external xCheckMaskEvent :
  (display * int * xEvent) -> int = "ML_XCheckMaskEvent";
external xClearArea :
  (display * window * int * int * int * int * int) -> unit = "ML_XClearArea";
external xClearWindow : (display * window) -> unit = "ML_XClearWindow";
external xCloseDisplay : display -> unit = "ML_XCloseDisplay";
external xConfigureWindow :
  (display * window * int * xWindowChanges) -> unit = "ML_XConfigureWindow";
external xConnectionNumber : display -> int = "ML_XConnectionNumber";
external xCopyArea :
  (display * drawable * drawable * gC * int * int * int * int * int * int) ->
    unit = "ML_XCopyArea";
external xCreateBitmapFromData :
  (display * drawable * string * int * int) ->
    xpixmap = "ML_XCreateBitmapFromData";
external xCreateColormap :
  (display * window * visual * int) -> colormap =
    "ML_XCreateColormap";
external xCreateFontCursor :
  (display * int) -> cursor = "ML_XCreateFontCursor";
external xCreateFontSet : (display * string) -> xFontSet =
  "ML_XCreateFontSet";
external xCreateGC :
  (display * window * int * xGCValues) -> gC = "ML_XCreateGC";
external xCreateImageNullData :
  (display * visual * int * int * int * int * int * int * int) ->
    xImage = "ML_XCreateImageNullData";
external xCreatePixmap :
  (display * drawable * int * int * int) -> xpixmap = "ML_XCreatePixmap";
external xCreatePixmapCursor :
  (display * xpixmap * xpixmap * xColor * xColor * int * int) ->
    cursor = "ML_XCreatePixmapCursor";
external xCreatePixmapFromBitmapData :
  (display * drawable * string * int * int * int * int * int) ->
    xpixmap = "ML_XCreatePixmapFromBitmapData";
external xCreateSimpleWindow :
  (display * window * int * int * int * int * int * int * int) ->
    window = "ML_XCreateSimpleWindow";
external xDefaultColormap :
  (display * int) -> colormap = "ML_XDefaultColormap";
external xDefaultDepth : (display * int) -> int = "ML_XDefaultDepth";
external xDefaultGC : (display * int) -> gC = "ML_XDefaultGC";
external xDefaultRootWindow : display -> window = "ML_XDefaultRootWindow";
external xDefaultScreen : display -> int = "ML_XDefaultScreen";
external xDefaultVisual : (display * int) -> visual = "ML_XDefaultVisual";
external xDefineCursor :
  (display * window * cursor) -> unit = "ML_XDefineCursor";
external xDestroyImage : xImage -> unit = "ML_XDestroyImage";
external xDestroyWindow : (display * window) -> unit = "ML_XDestroyWindow";
external xDisplayHeight : (display * int) -> int = "ML_XDisplayHeight";
external xDisplayName : string -> c_String = "ML_XDisplayName";
external xDisplayWidth : (display * int) -> int = "ML_XDisplayWidth";
external xDisplayWidthMM : (display * int) -> int = "ML_XDisplayWidthMM";
external xDrawArc :
  (display * drawable * gC * int * int * int * int * int * int) ->
    unit = "ML_XDrawArc";
external xDrawImageString :
  (display * drawable * gC * int * int * string * int) ->
    unit = "ML_XDrawImageString";
external xDrawLine :
  (display * drawable * gC * int * int * int * int) -> unit = "ML_XDrawLine";
external xDrawLines :
  (display * drawable * gC * xPoint * int * int) -> unit = "ML_XDrawLines";
external xDrawPoint :
  (display * drawable * gC * int * int) -> unit = "ML_XDrawPoint";
external xDrawRectangle :
  (display * drawable * gC * int * int * int * int) ->
    unit = "ML_XDrawRectangle";
external xDrawString :
  (display * drawable * gC * int * int * string * int) ->
    unit = "ML_XDrawString";
external xFetchBytes : (display * ref int) -> c_String = "ML_XFetchBytes";
external xFillArc :
  (display * drawable * gC * int * int * int * int * int * int) ->
    unit = "ML_XFillArc";
external xFillPolygon :
  (display * drawable * gC * xPoint * int * int * int) ->
    unit = "ML_XFillPolygon";
external xFillRectangle :
  (display * drawable * gC * int * int * int * int) ->
    unit = "ML_XFillRectangle";
external xFlush : display -> unit = "ML_XFlush";
external xFreeColors : (display * colormap * longRef * int * int) -> int =
  "ML_XFreeColors";
external xFreeCursor : (display * cursor) -> unit = "ML_XFreeCursor";
external xFreeFont : (display * xFontStruct) -> unit = "ML_XFreeFont";
external xFreeFontSet : (display * xFontSet) -> unit = "ML_XFreeFontSet";
external xFreeFontInfoSpec : xFontStruct -> unit = "ML_XFreeFontInfoSpec";
external xFreeGC : (display * gC) -> unit = "ML_XFreeGC";
external xFreePixmap : (display * xpixmap) -> unit = "ML_XFreePixmap";
external xGContextFromGC : gC -> gContext = "ML_XGContextFromGC";
external xGetDefault :
  (display * string * string) -> c_String = "ML_XGetDefault";
external xGetImage :
  (display * drawable * int * int * int * int * int * int) ->
    xImage = "ML_XGetImage";
external xGetKeyboardControl :
  (display * xKeyboardState) -> unit = "ML_XGetKeyboardControl";
external xGetPixel : (xImage * int * int) -> int = "ML_XGetPixel";
external xGetPixelWithData :
  (xImage * string * int * int) -> int = "ML_XGetPixelWithData";
external xGetSubImage :
  (display * drawable * int * int * int * int * int * int * xImage * int *
   int) ->
    xImage = "ML_XGetSubImage";
external xGetSubImageWithData :
  (display * drawable * int * int * int * int * int * int * xImage * string *
   int * int) ->
    xImage = "ML_XGetSubImageWithData";
external xInternAtom : (display * string * bool) -> status = "ML_XInternAtom";
external xKeycodeToKeysym :
  (display * int * int) -> keySym = "ML_XKeycodeToKeysym";
external xKeysymToString : keySym -> c_String = "ML_XKeysymToString";
external xLoadQueryFont :
  (display * string) -> xFontStruct = "ML_XLoadQueryFont";
external xLookupString : (xEvent * string * int) -> (int * keySym) =
  "ML_XLookupString";
external xMapRaised : (display * window) -> unit = "ML_XMapRaised";
external xMapSubwindows : (display * window) -> unit = "ML_XMapSubwindows";
external xMapWindow : (display * window) -> unit = "ML_XMapWindow";
external xMoveResizeWindow :
  (display * window * int * int * int * int) -> unit = "ML_XMoveResizeWindow";
external xMoveWindow :
  (display * window * int * int) -> unit = "ML_XMoveWindow";
external xNextEvent : (display * xEvent) -> unit = "ML_XNextEvent";
external xOpenDisplay : string -> display = "ML_XOpenDisplay";
external xPending : display -> int = "ML_XPending";
external xPutImage :
  (display * drawable * gC * xImage * int * int * int * int * int * int) ->
    unit = "ML_XPutImage";
external xPutImageWithData :
  (display * drawable * gC * xImage * string * int * int * int * int * int *
   int) ->
    unit = "ML_XPutImageWithData";
external xPutPixel : (xImage * int * int * int) -> unit = "ML_XPutPixel";
external xPutPixelWithData :
  (xImage * string * int * int * int) -> unit = "ML_XPutPixelWithData";
external xQueryColor :
  (display * colormap * xColor) -> unit = "ML_XQueryColor";
external xQueryFont : (display * xID) -> xFontStruct = "ML_XQueryFont";
external xQueryPointer :
  (display * window * ref window * ref window * ref int * ref int * ref int *
   ref int * ref int) ->
    int = "ML_XQueryPointer";
external xRaiseWindow : (display * window) -> unit = "ML_XRaiseWindow";
external xRefreshKeyboardMapping :
  xMappingEvent -> unit = "ML_XRefreshKeyboardMapping";
external xReparentWindow :
  (display * window * window * int * int) -> unit = "ML_XReparentWindow";
external xResizeWindow :
  (display * window * int * int) -> unit = "ML_XResizeWindow";
external xSelectInput : (display * window * int) -> unit = "ML_XSelectInput";
external xSetBackground : (display * gC * int) -> unit = "ML_XSetBackground";
external xSetClassHint :
  (display * window * string * string) -> unit = "ML_XSetClassHint";
external xSetClipMask : (display * gC * xpixmap) -> unit = "ML_XSetClipMask";
external xSetClipOrigin :
  (display * gC * int * int) -> unit = "ML_XSetClipOrigin";
external xSetCloseDownMode : (display * int) -> int = "ML_XSetCloseDownMode";
external xSetDashes : (display * gC * int * string * int) -> unit = "ML_XSetDashes";
external xSetFillStyle : (display * gC * int) -> unit = "ML_XSetFillStyle";
external xSetFont : (display * gC * xfont) -> unit = "ML_XSetFont";
external xSetForeground : (display * gC * int) -> unit = "ML_XSetForeground";
external xSetIconName :
  (display * window * string) -> unit = "ML_XSetIconName";
external xSetInputFocus :
  (display * window * int * time) -> unit = "ML_XSetInputFocus";
external xSetLineAttributes :
  (display * gC * int * int * int * int) -> unit = "ML_XSetLineAttributes";
external xSetNormalHints :
  (display * window * xSizeHints) -> unit = "ML_XSetNormalHints";
external xSetPlaneMask : (display * gC * int) -> unit = "ML_XSetPlaneMask";
external xSetSelectionOwner :
  (display * atom * window * time) -> unit = "ML_XSetSelectionOwner";
external xSetStandardProperties :
  (display * window * string * string * xpixmap * string * xSizeHints) ->
    unit = "ML_XSetStandardProperties";
external xSetStipple : (display * gC * xpixmap) -> unit = "ML_XSetStipple";
external xSetTile : (display * gC * xpixmap) -> unit = "ML_XSetTile";
external xSetTransientForHint :
  (display * window * window) -> unit = "ML_XSetTransientForHint";
external xSetTSOrigin :
  (display * gC * int * int) -> unit = "ML_XSetTSOrigin";
external xSetWindowBackground :
  (display * window * int) -> unit = "ML_XSetWindowBackground";
external xSetWindowBackgroundPixmap :
  (display * window * xpixmap) -> unit = "ML_XSetWindowBackgroundPixmap";
external xSetWindowBorder :
  (display * window * int) -> unit = "ML_XSetWindowBorder";
external xSetWindowBorderPixmap :
  (display * window * xpixmap) -> unit = "ML_XSetWindowBorderPixmap";
external xSetWMHints :
  (display * window * xWMHints) -> unit = "ML_XSetWMHints";
external xSetWMProtocols :
  (display * window * xAtom * int) -> status = "ML_XSetWMProtocols";
external xStoreBytes : (display * string * int) -> unit = "ML_XStoreBytes";
external xStoreColor :
  (display * colormap * xColor) -> unit = "ML_XStoreColor";
external xStoreName : (display * window * string) -> unit = "ML_XStoreName";
external xSync : (display * int) -> unit = "ML_XSync";
external xTextWidth : (xFontStruct * string * int) -> int = "ML_XTextWidth";
external xUndefineCursor : (display * window) -> unit = "ML_XUndefineCursor";
external xUnmapWindow : (display * window) -> unit = "ML_XUnmapWindow";
external xWhitePixel : (display * int) -> int = "ML_XWhitePixel";
external alloc_XAtom : int -> xAtom = "ML_alloc_XAtom";
external alloc_XColor : unit -> xColor = "ML_alloc_XColor";
external alloc_XEvent : unit -> xEvent = "ML_alloc_XEvent";
external alloc_XGCValues : unit -> xGCValues = "ML_alloc_XGCValues";
external alloc_XKeyboardControl :
  unit -> xKeyboardControl = "ML_alloc_XKeyboardControl";
external alloc_XKeyboardState :
  unit -> xKeyboardState = "ML_alloc_XKeyboardState";
external alloc_XPoint : int -> xPoint = "ML_alloc_XPoint";
external alloc_XSetWindowAttributes :
  unit -> xSetWindowAttributes = "ML_alloc_XSetWindowAttributes";
external alloc_XSizeHints : unit -> xSizeHints = "ML_alloc_XSizeHints";
external alloc_XWindowChanges :
  unit -> xWindowChanges = "ML_alloc_XWindowChanges";
external alloc_XWMHints : unit -> xWMHints = "ML_alloc_XWMHints";
external visual_class : visual -> int = "ML_Visual_class";
external xAnyEvent_window : xAnyEvent -> window = "ML_XAnyEvent_window";
external xButtonEvent_button : xButtonEvent -> int = "ML_XButtonEvent_button";
external xButtonEvent_state : xButtonEvent -> int = "ML_XButtonEvent_state";
external xButtonEvent_x : xButtonEvent -> int = "ML_XButtonEvent_x";
external xButtonEvent_x_root : xButtonEvent -> int = "ML_XButtonEvent_x_root";
external xButtonEvent_y : xButtonEvent -> int = "ML_XButtonEvent_y";
external xButtonEvent_y_root : xButtonEvent -> int = "ML_XButtonEvent_y_root";
external xClientMessageEvent_data_l_0 :
  xClientMessageEvent -> int = "ML_XClientMessageEvent_data_l_0";
external xClientMessageEvent_message_type :
  xClientMessageEvent -> atom = "ML_XClientMessageEvent_message_type";
external xColor_blue : xColor -> int = "ML_XColor_blue";
external xColor_green : xColor -> int = "ML_XColor_green";
external xColor_pixel : xColor -> int = "ML_XColor_pixel";
external xColor_red : xColor -> int = "ML_XColor_red";
external xConfigureEvent_border_width :
  xConfigureEvent -> int = "ML_XConfigureEvent_border_width";
external xConfigureEvent_height :
  xConfigureEvent -> int = "ML_XConfigureEvent_height";
external xConfigureEvent_width :
  xConfigureEvent -> int = "ML_XConfigureEvent_width";
external xConfigureEvent_x : xConfigureEvent -> int = "ML_XConfigureEvent_x";
external xConfigureEvent_y : xConfigureEvent -> int = "ML_XConfigureEvent_y";
external xCrossingEvent_detail :
  xCrossingEvent -> int = "ML_XCrossingEvent_detail";
external xCrossingEvent_focus :
  xCrossingEvent -> int = "ML_XCrossingEvent_focus";
external xCrossingEvent_x : xCrossingEvent -> int = "ML_XCrossingEvent_x";
external xCrossingEvent_x_root :
  xCrossingEvent -> int = "ML_XCrossingEvent_x_root";
external xCrossingEvent_y : xCrossingEvent -> int = "ML_XCrossingEvent_y";
external xCrossingEvent_y_root :
  xCrossingEvent -> int = "ML_XCrossingEvent_y_root";
external xEvent_type : xEvent -> int = "ML_XEvent_type";
external xEvent_xany : xEvent -> xAnyEvent = "ML_XEvent_xany";
external xEvent_xbutton : xEvent -> xButtonEvent = "ML_XEvent_xbutton";
external xEvent_xclient : xEvent -> xClientMessageEvent = "ML_XEvent_xclient";
external xEvent_xconfigure :
  xEvent -> xConfigureEvent = "ML_XEvent_xconfigure";
external xEvent_xcrossing : xEvent -> xCrossingEvent = "ML_XEvent_xcrossing";
external xEvent_xexpose : xEvent -> xExposeEvent = "ML_XEvent_xexpose";
external xEvent_xfocus : xEvent -> xFocusChangeEvent = "ML_XEvent_xfocus";
external xEvent_xgraphicsexpose :
  xEvent -> xGraphicsExposeEvent = "ML_XEvent_xgraphicsexpose";
external xEvent_xkey : xEvent -> xKeyEvent = "ML_XEvent_xkey";
external xEvent_xmapping : xEvent -> xMappingEvent = "ML_XEvent_xmapping";
external xEvent_xmotion : xEvent -> xMotionEvent = "ML_XEvent_xmotion";
external xExposeEvent_height : xExposeEvent -> int = "ML_XExposeEvent_height";
external xExposeEvent_width : xExposeEvent -> int = "ML_XExposeEvent_width";
external xExposeEvent_x : xExposeEvent -> int = "ML_XExposeEvent_x";
external xExposeEvent_y : xExposeEvent -> int = "ML_XExposeEvent_y";
external xExposeEvent_count : xExposeEvent -> int = "ML_XExposeEvent_count";
external xFocusChangeEvent_detail :
  xFocusChangeEvent -> int = "ML_XFocusChangeEvent_detail";
external xFocusChangeEvent_mode :
  xFocusChangeEvent -> int = "ML_XFocusChangeEvent_mode";
external xFontStruct_ascent : xFontStruct -> int = "ML_XFontStruct_ascent";
external xFontStruct_descent : xFontStruct -> int = "ML_XFontStruct_descent";
external xFontStruct_fid : xFontStruct -> xfont = "ML_XFontStruct_fid";
external xFontStruct_max_bounds_ascent :
  xFontStruct -> int = "ML_XFontStruct_max_bounds_ascent";
external xFontStruct_max_bounds_descent :
  xFontStruct -> int = "ML_XFontStruct_max_bounds_descent";
external xGraphicsExposeEvent_count :
  xGraphicsExposeEvent -> int = "ML_XGraphicsExposeEvent_count";
external xGraphicsExposeEvent_height :
  xGraphicsExposeEvent -> int = "ML_XGraphicsExposeEvent_height";
external xGraphicsExposeEvent_width :
  xGraphicsExposeEvent -> int = "ML_XGraphicsExposeEvent_width";
external xGraphicsExposeEvent_x :
  xGraphicsExposeEvent -> int = "ML_XGraphicsExposeEvent_x";
external xGraphicsExposeEvent_y :
  xGraphicsExposeEvent -> int = "ML_XGraphicsExposeEvent_y";
external xImage_data : xImage -> c_String = "ML_XImage_data";
external xKeyboardState_bell_duration :
  xKeyboardState -> int = "ML_XKeyboardState_bell_duration";
external xKeyboardState_bell_percent :
  xKeyboardState -> int = "ML_XKeyboardState_bell_percent";
external xKeyboardState_bell_pitch :
  xKeyboardState -> int = "ML_XKeyboardState_bell_pitch";
external xKeyEvent_keycode : xKeyEvent -> int = "ML_XKeyEvent_keycode";
external xKeyEvent_state : xKeyEvent -> int = "ML_XKeyEvent_state";
external xMotionEvent_x : xMotionEvent -> int = "ML_XMotionEvent_x";
external xMotionEvent_y : xMotionEvent -> int = "ML_XMotionEvent_y";
external xMotionEvent_state : xMotionEvent -> int = "ML_XMotionEvent_state";
external set_XAtom : (atom * xAtom * int) -> unit = "ML_set_XAtom";
external set_XColor_blue : (int * xColor) -> unit = "ML_set_XColor_blue";
external set_XColor_flags : (int * xColor) -> unit = "ML_set_XColor_flags";
external set_XColor_green : (int * xColor) -> unit = "ML_set_XColor_green";
external set_XColor_pixel : (int * xColor) -> unit = "ML_set_XColor_pixel";
external set_XColor_red : (int * xColor) -> unit = "ML_set_XColor_red";
external set_XGCValues_background :
  (int * xGCValues) -> unit = "ML_set_XGCValues_background";
external set_XGCValues_cap_style :
  (int * xGCValues) -> unit = "ML_set_XGCValues_cap_style";
external set_XGCValues_fill_style :
  (int * xGCValues) -> unit = "ML_set_XGCValues_fill_style";
external set_XGCValues_font :
  (xfont * xGCValues) -> unit = "ML_set_XGCValues_font";
external set_XGCValues_foreground :
  (int * xGCValues) -> unit = "ML_set_XGCValues_foreground";
external set_XGCValues_line_width :
  (int * xGCValues) -> unit = "ML_set_XGCValues_line_width";
external set_XGCValues_stipple :
  (xpixmap * xGCValues) -> unit = "ML_set_XGCValues_stipple";
external set_XGCValues_tile :
  (xpixmap * xGCValues) -> unit = "ML_set_XGCValues_tile";
external set_XImage_bitmap_bit_order :
  (int * xImage) -> unit = "ML_set_XImage_bitmap_bit_order";
external set_XImage_byte_order :
  (int * xImage) -> unit = "ML_set_XImage_byte_order";
external set_XKeyboardControl_bell_duration :
  (int * xKeyboardControl) -> unit = "ML_set_XKeyboardControl_bell_duration";
external set_XKeyboardControl_bell_percent :
  (int * xKeyboardControl) -> unit = "ML_set_XKeyboardControl_bell_percent";
external set_XKeyboardControl_bell_pitch :
  (int * xKeyboardControl) -> unit = "ML_set_XKeyboardControl_bell_pitch";
external set_XPoint_x : (int * xPoint * int) -> unit = "ML_set_XPoint_x";
external set_XPoint_y : (int * xPoint * int) -> unit = "ML_set_XPoint_y";
external set_XSetWindowAttributes_backing_store :
  (int * xSetWindowAttributes) ->
    unit = "ML_set_XSetWindowAttributes_backing_store";
external set_XSetWindowAttributes_bit_gravity :
  (int * xSetWindowAttributes) -> unit =
    "ML_set_XSetWindowAttributes_bit_gravity";
external set_XSetWindowAttributes_override_redirect :
  (int * xSetWindowAttributes) ->
    unit = "ML_set_XSetWindowAttributes_override_redirect";
external set_XSetWindowAttributes_save_under :
  (int * xSetWindowAttributes) ->
    unit = "ML_set_XSetWindowAttributes_save_under";
external set_XSizeHints_base_width :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_base_width";
external set_XSizeHints_base_height :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_base_height";
external set_XSizeHints_flags :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_flags";
external set_XSizeHints_min_width :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_min_width";
external set_XSizeHints_min_height :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_min_height";
external set_XSizeHints_x :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_x";
external set_XSizeHints_y :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_y";
external set_XSizeHints_width :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_width";
external set_XSizeHints_width_inc :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_width_inc";
external set_XSizeHints_height :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_height";
external set_XSizeHints_height_inc :
  (int * xSizeHints) -> unit = "ML_set_XSizeHints_height_inc";
external set_XWMHints_flags :
  (int * xWMHints) -> unit = "ML_set_XWMHints_flags";
external set_XWMHints_input :
  (int * xWMHints) -> unit = "ML_set_XWMHints_input";
external set_XWindowChanges_stack_mode :
  (int * xWindowChanges) -> unit = "ML_set_XWindowChanges_stack_mode";
external set_XWindowChanges_x :
  (int * xWindowChanges) -> unit = "ML_set_XWindowChanges_x";
external set_XWindowChanges_y :
  (int * xWindowChanges) -> unit = "ML_set_XWindowChanges_y";
external c_String_length : c_String -> int = "ML_C_String_length";
external fD_ISSET : (int * fd_set) -> int = "ML_FD_ISSET";
external fD_SET : (int * fd_set) -> unit = "ML_FD_SET";
external fD_ZERO : fd_set -> unit = "ML_FD_ZERO";
external mLimage_of_XImage : xImage -> mLimage = "ML_MLimage_of_XImage";
external alloc_LongRef : unit -> longRef = "ML_alloc_LongRef";
external longRef_value : longRef -> int = "ML_LongRef_value";
external set_longRef_value : (int * longRef) -> unit = "ML_set_LongRef_value";
external null_XGCValues : unit -> xGCValues = "ML_null_pt";
external alloc_fd_set : unit -> fd_set = "ML_alloc_fd_set";
external free : 'a -> unit = "ML_free"; (* C free *)
external fselect : (int * fd_set * int) -> int = "ML_fselect";
external is_null_Display : display -> bool = "ML_is_null_pt";
external is_null_XFontStruct : xFontStruct -> bool = "ML_is_null_pt";
external is_null_C_String : c_String -> bool = "ML_is_null_pt";
external set_xerror_exn : (exn * exn) -> unit = "ML_set_xerror_exn";
external string_of_C_String :
  (c_String * int) -> string = "ML_string_of_C_String";
external read : int -> bytes -> int -> int = "ML_read";
external ioctl_winsz :
  int -> int -> int -> int -> int -> unit = "ML_ioctl_winsz";
external mL_ctime : unit -> int = "ML_ctime";
external mL_ctime_ms : unit -> int = "ML_ctime_ms";

set_xerror_exn (X_failure "", X_io_error "" "");

value bit_In_Long n = do {
  let x = ref 1
  and i = ref n in
  while (do { decr i; i.val }) >= 0 do { x.val := x.val * 2 };
  x.val
};

value bit_In_Int n = do {
  let x = ref 1
  and i = ref n in
  while (do { decr i; i.val }) >= 0 do { x.val := x.val * 2 };
  x.val
};

value xNone = 0;
value pointerRoot = 1;

value gCFunction = bit_In_Long 0
and gCPlaneMask = bit_In_Long 1
and gCForeground = bit_In_Long 2
and gCBackground = bit_In_Long 3
and gCLineWidth = bit_In_Long 4
and gCLineStyle = bit_In_Long 5
and gCCapStyle = bit_In_Long 6
and gCJoinStyle = bit_In_Long 7
and gCFillStyle = bit_In_Long 8
and gCFillRule = bit_In_Long 9
and gCTile = bit_In_Long 10
and gCStipple = bit_In_Long 11
and gCTileStipXOrigin = bit_In_Long 12
and gCTileStipYOrigin = bit_In_Long 13
and gCFont = bit_In_Long 14
and gCSubwindowMode = bit_In_Long 15
and gCGraphicsExposures = bit_In_Long 16
and gCClipXOrigin = bit_In_Long 17
and gCClipYOrigin = bit_In_Long 18
and gCClipMask = bit_In_Long 19
and gCDashOffset = bit_In_Long 20
and gCDashList = bit_In_Long 21
and gCArcMode = bit_In_Long 22;

value xYBitmap = 0 and xYPixmap = 1 and zPixmap = 2;

value lSBFirst = 0 and mSBFirst = 1;

value noEventMask = 0
and keyPressMask = bit_In_Long 0
and keyReleaseMask = bit_In_Long 1
and buttonPressMask = bit_In_Long 2
and buttonReleaseMask = bit_In_Long 3
and enterWindowMask = bit_In_Long 4
and leaveWindowMask = bit_In_Long 5
and pointerMotionMask = bit_In_Long 6
and pointerMotionHintMask = bit_In_Long 7
and button1MotionMask = bit_In_Long 8
and button2MotionMask = bit_In_Long 9
and button3MotionMask = bit_In_Long 10
and button4MotionMask = bit_In_Long 11
and button5MotionMask = bit_In_Long 12
and buttonMotionMask = bit_In_Long 13
and keymapStateMask = bit_In_Long 14
and exposureMask = bit_In_Long 15
and visibilityChangeMask = bit_In_Long 16
and structureNotifyMask = bit_In_Long 17
and resizeRedirectMask = bit_In_Long 18
and substructureNotifyMask = bit_In_Long 19
and substructureRedirectMask = bit_In_Long 20
and focusChangeMask = bit_In_Long 21
and propertyChangeMask = bit_In_Long 22
and colormapChangeMask = bit_In_Long 23
and ownerGrabButtonMask = bit_In_Long 24;

value keyPress = 2
and keyRelease = 3
and buttonPress = 4
and buttonRelease = 5
and motionNotify = 6
and enterNotify = 7
and leaveNotify = 8
and focusIn = 9
and focusOut = 10
and keymapNotify = 11
and expose = 12
and graphicsExpose = 13
and noExpose = 14
and visibilityNotify = 15
and createNotify = 16
and destroyNotify = 17
and unmapNotify = 18
and mapNotify = 19
and mapRequest = 20
and reparentNotify = 21
and configureNotify = 22
and configureRequest = 23
and gravityNotify = 24
and resizeRequest = 25
and circulateNotify = 26
and circulateRequest = 27
and propertyNotify = 28
and selectionClear = 29
and selectionRequest = 30
and selectionNotify = 31
and colormapNotify = 32
and clientMessage = 33
and mappingNotify = 34
and lASTEvent = 35;

value button1Mask = bit_In_Int 8
and button2Mask = bit_In_Int 9
and button3Mask = bit_In_Int 10
and button4Mask = bit_In_Int 11
and button5Mask = bit_In_Int 12
and anyModifier = bit_In_Int 15;

value lineSolid = 0 and lineOnOffDash = 1 and lineDoubleDash = 2;

value capNotLast = 0 and capButt = 1 and capRound = 2 and capProjecting = 3;

value joinMiter = 0 and joinRound = 1 and joinBevel = 2;

value cWBackPixmap = bit_In_Long 0
and cWBackPixel = bit_In_Long 1
and cWBorderPixmap = bit_In_Long 2
and cWBorderPixel = bit_In_Long 3
and cWBitGravity = bit_In_Long 4
and cWWinGravity = bit_In_Long 5
and cWBackingStore = bit_In_Long 6
and cWBackingPlanes = bit_In_Long 7
and cWBackingPixel = bit_In_Long 8
and cWOverrideRedirect = bit_In_Long 9
and cWSaveUnder = bit_In_Long 10
and cWEventMask = bit_In_Long 11
and cWDontPropagate = bit_In_Long 12
and cWColormap = bit_In_Long 13
and cWCursor = bit_In_Long 14;

value cWX = bit_In_Int 0
and cWY = bit_In_Int 1
and cWWidth = bit_In_Int 2
and cWHeight = bit_In_Int 3
and cWBorderWidth = bit_In_Int 4
and cWSibling = bit_In_Int 5
and cWStackMode = bit_In_Int 6;

value northWestGravity = 1;

value above = 0
and below = 1
and topIf = 2
and bottomIf = 3
and opposite = 4;

value fillSolid = 0
and fillTiled = 1
and fillStippled = 2
and fillOpaqueStippled = 3;

(* flags argument in size hints *)
value uSPosition = 1
and uSSize = 2
and pPosition = 4
and pSize = 8
and pMinSize = 16
and pResizeInc = 64
and pBaseSize = 256;

value notUseful = 0 and whenMapped = 1 and always = 2;

value allocNone = 0
and allocAll = 1;

value doRed = 1
and doGreen = 2
and doBlue = 4;

value currentTime = 0;

value xA_PRIMARY = 1;

value shiftMask = bit_In_Int 0
and lockMask = bit_In_Int 1
and controlMask = bit_In_Int 2
and mod1Mask = bit_In_Int 3
and mod2Mask = bit_In_Int 4
and mod3Mask = bit_In_Int 5
and mod4Mask = bit_In_Int 6
and mod5Mask = bit_In_Int 7;

value complex = 0 and nonconvex = 1 and convex = 2;

value coordModeOrigin = 0 and coordModePrevious = 1;

value staticGray = 0
and grayScale = 1
and staticColor = 2
and pseudoColor = 3
and trueColor = 4
and directColor = 5;

value inputHint = 1
and stateHint = 2
and iconPixmapHint = 4
and iconWindowHint = 8
and iconPositionHint = 16
and iconMaskHint = 32
and windowGroupHint = 64
and allHints = 127;

(* Notify detail *)
value notifyAncestor = 0
and notifyVirtual = 1
and notifyInferior = 2
and notifyNonlinear = 3
and notifyNonlinearVirtual = 4
and notifyPointer = 5
and notifyPointerRoot = 6
and notifyDetailNone = 7;

(* Notify modes *)
value notifyNormal = 0
and notifyGrab = 1
and notifyUngrab = 2
and notifyWhileGrabbed = 3;

(* masks for ChangeKeyboardControl *)
value kBKeyClickPercent = bit_In_Long 0
and kBBellPercent = bit_In_Long 1
and kBBellPitch = bit_In_Long 2
and kBBellDuration = bit_In_Long 3
and kBLed = bit_In_Long 4
and kBLedMode = bit_In_Long 5
and kBKey = bit_In_Long 6
and kBAutoRepeatMode = bit_In_Long 7;

value xK_Shift_L = 0xFFE1;
value xK_Hyper_R = 0xFFEE;

value isModifierKey keysym = keysym >= xK_Shift_L && keysym <= xK_Hyper_R;

(* Used in SetInputFocus, GetInputFocus *)
value revertToNone = xNone
and revertToPointerRoot = pointerRoot
and revertToParent = 2;

(* Xft *)

type xftcolor = 'a;
type xftdraw = 'a;
type xftfont = 'a;

external alloc_XftColor : unit -> xftcolor = "ML_alloc_XftColor";
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

external get_screen_size_mm : display -> (int * int) =
  "ML_get_screen_size_mm";
