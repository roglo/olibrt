(* sudo apt install libxft-dev libfreetype-dev libxrandr-dev *)

open Rt;
open Rtf;

value main () = do {
  let xdf = rtf_initialize "" in
  let xd = rtf_xdata xdf in
(*
  Printf.printf "screen width = %d\n" (rt_screen_width xd);
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
  flush stdout;
  let font = xftFontOpenName (dpy, screen, "mono:size=12") in
(*
  if (font) print_font_info(display, font);
*)
  let window =
    xCreateSimpleWindow
      (dpy, xDefaultRootWindow dpy, 0, 0, truncate (150. *. dpmm),
       truncate (100. *. dpmm), 0, 0, 0)
  in
  xSelectInput (dpy, window, exposureMask);
  xMapWindow (dpy, window);
  let attrs = alloc_XWindowAttributes () in
  let s = xGetWindowAttributes(dpy, window, attrs) in
  Printf.printf "xGetWindowsAttributes returns %d\n" s;
  flush stdout;
  let draw =
    xftDrawCreate
      (dpy, window, xWindowAttributes_visual attrs,
       xWindowAttributes_colormap attrs)
  in
  let color = alloc_XftColor () in
  let b =
    xftColorAllocName
      (dpy, xWindowAttributes_visual attrs,
       xWindowAttributes_colormap attrs, "white", color)
  in
  Printf.printf "xftColorAllocName returns %b\n" b;
  flush stdout;
  let xev = alloc_XEvent () in
*)
  let xa = rt_args [xd] in
  rt_main_loop xa;
(*
  while True do {
    xNextEvent(dpy, xev);
    if xEvent_type xev = expose then
      let xeev = xEvent_xexpose xev in
      if xExposeEvent_count xeev = 0 then do {
        Printf.printf "expose\n";
        flush stdout;
	xftDrawString8 (draw, color, font, truncate (10. *. dpmm),
	                truncate (20. *. dpmm),
		        "Bonjour", String.length("Bonjour"));
	xFlush dpy;
      }
      else ()
    else ();
  };
*)
(*
  xftFontClose(dpy, font);
*)
};

main ();
