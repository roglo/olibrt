(* sudo apt install libxft-dev libfreetype-dev libxrandr-dev *)
(* ocamlc -custom -pp camlp5r librt.cma test.ml ../Xlib/libx.a -cclib -lX11 *)

open Xlib;

value main () =
  let dpy = xOpenDisplay "" in
  ()
(*
  screen = DefaultScreen(display);
  printf("screen width = %d\n", DisplayWidth(display, screen));
  printf("screen width mm = %d (not sure)\n", DisplayWidthMM(display, screen));
  get_screen_size_mm(display, &width_mm, &height_mm);
  printf("screen (width, height) in mm = (%d, %d)\n", width_mm, height_mm);
  dpmm =
    (double)DisplayWidth(display, screen) /
    (double)DisplayWidthMM(display, screen);
  printf("dpmm = %g (not sure)\n", dpmm);
  dpmm = (double)DisplayWidth(display, screen) / (double)width_mm;
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
;
