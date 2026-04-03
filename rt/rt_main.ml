(* $Id: rt_main.ml,v 1.45 2017/12/28 10:38:58 deraugla Exp $
 *
 * Rogloglo Toolkit
 *)

open Rtdecl;
open Std;
open Time;
open Util;
open Xlib;

value rt_version = "1.09-exp";

value rt_initialize =
  let cols =
    mallocated_var (fun () -> (alloc_XColor (), alloc_XColor ())) (ref None)
  in
  fun name -> do {
    let dpy = xOpenDisplay name in
    if is_null_Display dpy then failwith "Can't open display" else ();
    let scr = xDefaultScreen dpy
    and rootw = xDefaultRootWindow dpy in
    let depth = xDefaultDepth (dpy, scr) in
    let vis = xDefaultVisual (dpy, scr) in
    let cmap = xDefaultColormap (dpy, scr) in
    let black = xBlackPixel (dpy, scr)
    and white = xWhitePixel (dpy, scr) in
    let (backg_pix, light_pix, dark_pix) =
      if List.mem (visual_class vis) [pseudoColor; trueColor] then do {
        let (scol, ecol) = cols () in
        (*
                  let col = get_default dpy "background" "CadetBlue" in
        *)
        let col = "CadetBlue" in
        (**)
        let _ = xAllocNamedColor (dpy, cmap, col, scol, ecol) in
        let backg_pix = xColor_pixel scol in
        let (h, s, v) =
          hsv_of_rgb
            (xColor_red scol / 256, xColor_green scol / 256,
             xColor_blue scol / 256)
        in
        let (r, g, b) = rgb_of_hsv (h, s / 3, v * 4 / 3) in
        set_XColor_red (r * 256, scol);
        set_XColor_green (g * 256, scol);
        set_XColor_blue (b * 256, scol);
        let _ = xAllocColor (dpy, cmap, scol) in
        let light_pix = xColor_pixel scol in
        let (r, g, b) = rgb_of_hsv (h, s, v * 8 / 15) in
        set_XColor_red (r * 256, scol);
        set_XColor_green (g * 256, scol);
        set_XColor_blue (b * 256, scol);
        let _ = xAllocColor (dpy, cmap, scol) in
        let dark_pix = xColor_pixel scol in (backg_pix, light_pix, dark_pix)
      }
      else (white, white, black)
    in
    (*
          let v = get_default dpy "border" "3" in
          let motif_border = try int_of_string v with _ -> 3 in
    *)
    let motif_border = 2 in
    (**)
    let xgcv = (gstr ()).xgcv in set_XGCValues_background (white, xgcv);
    set_XGCValues_foreground (black, xgcv);
    let gc = xCreateGC (dpy, rootw, gCBackground lor gCForeground, xgcv) in
    let wdw = xInternAtom (dpy, "WM_DELETE_WINDOW", False) in
    let wmp = xInternAtom (dpy, "WM_PROTOCOLS", False) in
    {dpy = dpy; scr = scr; vis = vis; black = black; white = white;
     backg_pix = backg_pix; bord_pix = [| light_pix; dark_pix |];
     rootw = rootw; root_width = xDisplayWidth (dpy, scr);
     root_height = xDisplayHeight (dpy, scr); depth = depth; cmap = cmap;
     connection_number = xConnectionNumber dpy; motif_border = motif_border;
     gray_pixm =
       xCreatePixmapFromBitmapData
         (dpy, rootw, "Uª", 8, 2, black, white, depth);
     gc = gc; wm_delete_window = wdw; wm_protocols = wmp; char_set = Latin_1;
     line_width = 1; win_but = WB_None; popped_up = []; end_func = [];
     sys_timeout = []; cancel_timeout = []; focus = False;
     x_selected_wid = None; redirected_wid = None; no_redirecting_wid = [];
     ginfo = Array.make 17 []; wid_by_win = Array.make 53 [];
     wid_by_name = Array.make 53 []; font_by_name = Array.make 17 [];
     wid_by_shortcut = []}
  }
;

value hash_clear lv = for i = 0 to Array.length lv - 1 do { lv.(i) := [] };

value rt_end xd = do {
  List.iter (fun f -> f ()) xd.end_func;
  hash_clear xd.ginfo;
  hash_clear xd.wid_by_win;
  hash_clear xd.wid_by_name;
  hash_clear xd.font_by_name;
  xd.sys_timeout := [];
  xd.end_func := [];
  xd.popped_up := [];
  xCloseDisplay xd.dpy
};

value rt_select_char_set xd cs = xd.char_set := cs;

value rt_args xdl =
  {xdl = xdl; fd_list = []; initial_time = ftime (); current_time = 0;
   timeout = []; running = False; file_buff = Bytes.create 1024}
;

value rt_map_widget wid = do {
  wid.is_mapped := True;
  xMapWindow (wid.wid_xd.dpy, wid.win)
}
and rt_unmap_widget wid = do {
  wid.wid_xd.win_but := WB_None;
  wid.is_mapped := False;
  xUnmapWindow (wid.wid_xd.dpy, wid.win)
}
and rt_raise_widget wid = xRaiseWindow (wid.wid_xd.dpy, wid.win);

value set_timeout xa tag tm tmf =
  xa.timeout :=
    let rec insert =
      fun
      [ [((_, tm', _) as timeout') :: l] as l' ->
          if tm' < tm then [timeout' :: insert l] else [(tag, tm, tmf) :: l']
      | [] -> [(tag, tm, tmf)] ]
    in
    insert xa.timeout
;

value update_time_and_timeouts xa = do {
  xa.current_time := timeb_sub (ftime ()) xa.initial_time;
  List.iter
    (fun xd -> do {
       List.iter
         (fun (tag, tm, tmf) ->
            set_timeout xa (SysTimeout tag) (xa.current_time + tm) tmf)
         xd.sys_timeout;
       xd.sys_timeout := []
     })
    xa.xdl;
  List.iter
    (fun xd -> do {
       List.iter
         (fun wid ->
            xa.timeout :=
              List.fold_right
                (fun ((tag, _, _) as timeout) l ->
                   match tag with
                   [ SysTimeout wid' ->
                       if wid == wid' then l else [timeout :: l]
                   | UserTimeout _ -> [timeout :: l] ])
                xa.timeout [])
         xd.cancel_timeout;
       xd.cancel_timeout := []
     })
    xa.xdl;
};

value init_fds xa fds = do {
  fD_ZERO fds;
  let max_fd =
    List.fold_left
      (fun max_fd (fd, _) -> do { fD_SET (fd, fds); max max_fd fd }) 0
      xa.fd_list
  in
  List.fold_left
    (fun max_fd xd -> do {
       let fd = xd.connection_number in fD_SET (fd, fds);
       max max_fd fd
     })
    max_fd xa.xdl
};

value rt_treat_one_event xa = do {
  if xa.xdl = [] && xa.fd_list = [] && xa.timeout = [] then
    failwith "rt_treat_one_event: no xdata, no fd, no timeout"
  else ();
  update_time_and_timeouts xa;
  let gstr = gstr () in
  match xa.timeout with
  [ [] ->
      if not (try_pending xa.xdl gstr.xev) then
        let max_fd = init_fds xa gstr.fds in
        if fselect (max_fd + 1, gstr.fds, -1) >= 0 then do {
          xa.current_time := timeb_sub (ftime ()) xa.initial_time;
          dispatch xa gstr.xev gstr.fds
        }
        else failwith "rt_treat_one_event: fselect error"
      else ()
  | [(_, tmout, tmout_fun) :: l] ->
      if not (try_pending xa.xdl gstr.xev) then
        if xa.current_time < tmout then
          let max_fd = init_fds xa gstr.fds in
          let tm = tmout - xa.current_time in
          let ns = fselect (max_fd + 1, gstr.fds, tm) in
          if ns = 0 then do {
            xa.current_time := tmout;
            xa.timeout := l;
            tmout_fun ()
          }
          else if ns > 0 then do {
            xa.current_time := timeb_sub (ftime ()) xa.initial_time;
            dispatch xa gstr.xev gstr.fds
          }
          else failwith "rt_treat_one_event: fselect error"
        else do { xa.current_time := tmout; xa.timeout := l; tmout_fun () }
      else () ]
};

value rec rt_treat_pending_events xa =
  if try_pending xa.xdl (gstr ()).xev then rt_treat_pending_events xa else ()
;

value rt_pending_events xa =
  if List.exists (fun xd -> xPending xd.dpy <> 0) xa.xdl then True
  else do {
    update_time_and_timeouts xa;
    match xa.timeout with
    [ [] ->
        if xa.fd_list = [] then False
        else
          let gstr = gstr () in
          let max_fd = init_fds xa gstr.fds in
          if fselect (max_fd + 1, gstr.fds, 0) > 0 then True else False
    | [(_, tmout, _) :: l] ->
        if xa.current_time >= tmout then True else False ]
  }
;

value rt_immediate_time xa = timeb_sub (ftime ()) xa.initial_time;

value rt_main_loop xa = do {
  xa.running := True;
  while xa.running do { rt_treat_one_event xa }
}
and rt_stop_main_loop xa = xa.running := False
and rt_set_timeout xa name = set_timeout xa (UserTimeout name)
and rt_cancel_timeout xa name =
  xa.timeout :=
    List.fold_right
      (fun ((tag, _, _) as timeout) l ->
         match tag with
         [ SysTimeout _ -> [timeout :: l]
         | UserTimeout name' -> if name = name' then l else [timeout :: l] ])
      xa.timeout []
and rt_current_time xa = xa.current_time
and rt_select_xdata xa xd = xa.xdl := add_setq xd xa.xdl
and rt_unselect_xdata xa xd = xa.xdl := filter_neg (\== xd) xa.xdl
and rt_select_file_descr xa fd fd_fun =
  xa.fd_list :=
    [(fd, fd_fun) :: filter_neg (fun (fd1, _) -> fd = fd1) xa.fd_list]
and rt_select_file xa fd f =
  let fd_fun fd =
    let len = read fd xa.file_buff (Bytes.length xa.file_buff) in
    f (Bytes.to_string xa.file_buff) len
  in
  xa.fd_list :=
    [(fd, fd_fun) :: filter_neg (fun (fd1, _) -> fd = fd1) xa.fd_list]
and rt_unselect_file xa fd =
  xa.fd_list := filter_neg (fun (fd1, _) -> fd = fd1) xa.fd_list
;

value rt_create_widget xd wname iname position wdel wdesc = do {
  let wsh = wdesc.wsize xd in
  let (x, y, flag) =
    match position with
    [ UserPosition x y -> (x, y, uSPosition)
    | AutoPosition -> (0, 0, pPosition) ]
  in
  let wid = wdesc.wcreate xd xd.rootw True False wdesc x y wsh in
  set_std_prop wid wname iname x y wsh (flag lor uSSize);
  set_wm_and_class_hints wid;
  set_delete_callback wid wdel;
  wid
}
and rt_destroy_widget wid = destroy_widget wid;

value rt_adjust_widget wid = do {
  let xd = wid.wid_xd in
  let wsh = wid.wdesc.wsize xd in
  let wsh =
    {(wsh) with
     sh_width = max 1 wsh.sh_width;
     sh_height = max 1 wsh.sh_height}
  in
  xResizeWindow (xd.dpy, wid.win, wsh.sh_width, wsh.sh_height);
  wid.wdesc.wresize wid wsh.sh_width wsh.sh_height;
  (wsh.sh_width, wsh.sh_height)
};

value rt_change_background wid backg =
  let xd = wid.wid_xd in
  match backg with
  [ NonePn -> xSetWindowBackgroundPixmap (xd.dpy, wid.win, xNone)
  | PixmapPn p -> xSetWindowBackgroundPixmap (xd.dpy, wid.win, p.pixmap)
  | ColorPn c -> xSetWindowBackground (xd.dpy, wid.win, c.pixel) ]
;

value rt_change_widget_name =
  let xsh = mallocated_var alloc_XSizeHints (ref None) in
  fun wid wname iname -> do {
    let xd = wid.wid_xd in
    let xsh = xsh () in set_XSizeHints_flags (0, xsh);
    xSetStandardProperties
      (xd.dpy, wid.win, wname, iname, xNone, Sys.argv.(0), xsh)
  }
;

value widget_named xd wname =
  try hash_assoc wname xd.wid_by_name with _ ->
    failwith ("widget_named " ^ wname)
;

value screen_width xd = xd.root_width
and screen_height xd = xd.root_height
and screen_depth xd = xd.depth
and rt_display_name dname =
  let s = xDisplayName dname in string_of_C_String (s, c_String_length s)
and is_colored xd = List.mem (visual_class xd.vis) [pseudoColor; trueColor];

value xdata_of_pixmap pixm = pixm.pixm_xd
and xdata_of_widget wid = wid.wid_xd;

value widget_x wid = wid.x
and widget_y wid = wid.y
and widget_width wid = wid.width
and widget_height wid = wid.height
and widget_border wid = wid.border
and widget_children wid = wid.children
and is_mapped wid = wid.is_mapped
and widget_size xd wdesc =
  let wsh = wdesc.wsize xd in (wsh.sh_width, wsh.sh_height)
;

value freeze_unfreeze frozen wid =
  let _xd = wid.wid_xd in
  freeze_rec wid where rec freeze_rec wid = do {
    wid.frozen := frozen;
    if wid.is_mapped then wid.wdesc.wfreeze wid else ();
    List.iter freeze_rec wid.children
  }
;

value rt_freeze_widget = freeze_unfreeze True
and rt_unfreeze_widget = freeze_unfreeze False;

value is_frozen wid = wid.frozen;
