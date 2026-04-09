(* sudo apt install libxft-dev libfreetype-dev libxrandr-dev *)
(* ocamlc -custom -pp camlp5r librt.cma test.ml ../Xlib/libx.a -cclib -lX11 -lXft *)

open Xlib;

value main () = do {
  let dpy = xOpenDisplay "" in
  let screen = xDefaultScreen dpy in
  Printf.printf "default screen = %d\n" screen;
  flush stdout;
  Printf.printf "screen width = %d\n" (xDisplayWidth (dpy, screen));
  Printf.printf "screen width mm = %d (not sure)\n"
    (xDisplayWidthMM (dpy, screen));
  flush stdout;
  let (width_mm, height_mm) = get_screen_size_mm dpy in
  Printf.printf "screen (width, height) in mm = (%d, %d)\n" width_mm height_mm;
  flush stdout;
  let dpmm =
    float (xDisplayWidth (dpy, screen)) /. float (xDisplayWidthMM (dpy, screen))
  in
  Printf.printf "dpmm = %g (not sure)\n" dpmm;
  flush stdout;
  let dpmm = float (xDisplayWidth (dpy, screen)) /. float width_mm in
  Printf.printf "dpmm = %g\n" dpmm;
  let font = xftFontOpenName (dpy, screen, "mono:size=12") in
  let  window =
    xCreateSimpleWindow
      (dpy, xDefaultRootWindow dpy, 0, 0, truncate (150. *. dpmm),
       truncate (100. *. dpmm), 0, 0, 0)
  in
  ()
(*
  printf("dpmm = %g\n", dpmm);
  font = XftFontOpenName(display, screen, "mono:size=12");
  if (font) print_font_info(display, font);
  window =
    XCreateSimpleWindow(display, DefaultRootWindow(display),
			0, 0, (int)(200 * dpmm), (int)(150 * dpmm), 0, 0, 0);
  XSelectInput(display, window, ExposureMask);
  XMapWindow(display, window);
  XGetWindowAttributes(display, window, &attrs);
  draw = XftDrawCreate(display, window, attrs.visual, attrs.colormap);
  XftColorAllocName(display, attrs.visual, attrs.colormap, "white", &color);
  while (1) {
    XNextEvent(display, &xev);
    if (xev.type == Expose) {
      if (xev.xexpose.count == 0) {
	XftDrawString8(draw, &color, font, (int)(10 * dpmm), (int)(20 * dpmm),
		       (XftChar8 -)"Bonjour", strlen("Bonjour"));
	XFlush(display);
      }
    }
  }
  XftFontClose(display, font);
*)
};

main ();
