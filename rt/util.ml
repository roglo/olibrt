(* $Id: util.ml,v 1.26 2017/12/28 10:38:58 deraugla Exp $ *)

open Font;
open Rtdecl;
open Std;
open Xlib;

type glob_struct = { xgcv : xGCValues; xev : xEvent; fds : fd_set };

type attribute_values =
  { backg_att : option paint;
    band_att : option int;
    bd_backg_att : option paint;
    bold_att : option int;
    border_att : option int;
    font_att : array font;
    foreg_att : option paint;
    height_att : option int;
    inter_att : option int;
    left_justif : bool;
    name_att : option string;
    width_att : option int }
;

value dynamo_tag (a : 'a) b r =
  (fun x (y : 'a) -> do { r.val := Some x; b },
   fun f -> do {
     r.val := None;
     let v = f a in
     match r.val with
     [ None -> failwith ("bad type \"" ^ v ^ "\", should be \"" ^ b ^ "\"")
     | Some x -> x ]
   })
;

value dynamo_local_info x = dynamo_tag C_LI x
and dynamo_global_info x = dynamo_tag C_GI x
and dynamo_args_info x = dynamo_tag C_AI x;

value no_info _ = "";
value no_user_info = C_UI (fun _ -> failwith "no_user_info");
value no_args_info _ = failwith "no_args_info";

value mallocated_var alloc_fun r _ =
  match r.val with
  [ None -> do {
      let v = alloc_fun () in r.val := Some v;
      v
    }
  | Some v -> v ]
;

value gstr =
  let gstr = ref None in
  fun () ->
    match gstr.val with
    [ Some gstr -> gstr
    | None -> do {
        let str =
          {xgcv = alloc_XGCValues (); xev = alloc_XEvent ();
           fds = alloc_fd_set ()}
        in
        gstr.val := Some str;
        str
      } ]
;

value create_widget xd win istop x y wsh wdesc info children =
  {wid_xd = xd; win = win; is_toplevel = istop; x = x; y = y;
   width = wsh.sh_width; height = wsh.sh_height; border = wsh.sh_border;
   wdesc = wdesc; is_mapped = False; frozen = False; deleted = False;
   delete_callback = None; info = info; user_info = no_user_info;
   children = children}
;

value destroy_widget wid = do {
  let xd = wid.wid_xd
  and win = wid.win in
  destroy wid where rec destroy wid = do {
    List.iter (fun wid -> destroy wid) wid.children;
    wid.wdesc.wdestroy wid
  };
  xDestroyWindow (xd.dpy, win);
  xd.win_but := WB_None;
  xd.popped_up := List.filter (fun wid -> wid.win <> win) xd.popped_up;
};

value add_widget name_att win wid = do {
  let xd = wid.wid_xd in
  hash_add_assoc (win, wid) xd.wid_by_win;
  match name_att with
  [ Some wname -> hash_add_assoc (wname, wid) xd.wid_by_name
  | _ -> () ];
  wid
}
and remove_widget name_att win wid = do {
  let xd = wid.wid_xd in
  match name_att with
  [ Some wname -> hash_remove_assoc wname xd.wid_by_name
  | _ -> () ];
  wid.deleted := True;
  hash_remove_assoc win xd.wid_by_win
}
and add_ginfo xd wname global_info ginfo = do {
  hash_add_assoc (wname, global_info ginfo) xd.ginfo;
  ginfo
}
and ginfo xd wname = hash_assoc wname xd.ginfo
and remove_ginfo xd wname = hash_remove_assoc wname xd.ginfo;

value opt_val def =
  fun
  [ Some x -> x
  | None -> def ]
;


value set_north_west_bit_gravity =
  let xswa = mallocated_var alloc_XSetWindowAttributes (ref None) in
  fun xd win -> do {
    let xswa = xswa () in
    set_XSetWindowAttributes_bit_gravity (northWestGravity, xswa);
    xChangeWindowAttributes (xd.dpy, win, cWBitGravity, xswa)
  }
;

value create_window xd pwin is_top x y wsh att_val emask = do {
  let {sh_width = width; sh_height = height; sh_border = border} = wsh in
  let bg_att = att_val.backg_att in
  let bd_att = att_val.bd_backg_att in
  let bg =
    match bg_att with
    [ Some (ColorPn c) -> c.pixel
    | _ -> xd.white ]
  in
  let bd =
    match bd_att with
    [ Some (ColorPn c) -> c.pixel
    | _ -> xd.black ]
  in
  let win =
    xCreateSimpleWindow
      (xd.dpy, pwin, x, y, max width 1, max height 1, max border 0, bd, bg)
  in
  match bg_att with
  [ Some NonePn -> xSetWindowBackgroundPixmap (xd.dpy, win, xNone)
  | Some (PixmapPn p) -> xSetWindowBackgroundPixmap (xd.dpy, win, p.pixmap)
  | _ -> () ];
  match bd_att with
  [ Some NonePn -> xSetWindowBorderPixmap (xd.dpy, win, xNone)
  | Some (PixmapPn p) -> xSetWindowBorderPixmap (xd.dpy, win, p.pixmap)
  | _ -> () ];
  set_north_west_bit_gravity xd win;
  let emask =
    if is_top then
      emask lor focusChangeMask lor keyPressMask lor enterWindowMask lor
      leaveWindowMask
    else emask
  in
  xSelectInput (xd.dpy, win, emask);
  win
};

value set_wm_and_class_hints =
  let xwmh = mallocated_var alloc_XWMHints (ref None) in
  fun wid -> do {
    let xd = wid.wid_xd
    and xwmh = xwmh () in
    set_XWMHints_input (1, xwmh);
    set_XWMHints_flags (inputHint, xwmh);
    xSetWMHints (xd.dpy, wid.win, xwmh);
    xSetClassHint (xd.dpy, wid.win, "camlrt", "Camlrt")
  }
;

value atoms = alloc_XAtom 1;

value set_delete_callback wid wdel =
  let xd = wid.wid_xd in
  match wdel with
  [ Some callb -> do {
      set_XAtom (xd.wm_delete_window, atoms, 0);
      let _ : status = xSetWMProtocols (xd.dpy, wid.win, atoms, 1) in
      wid.delete_callback := Some callb
    }
  | None -> () ]
;

value rev_implode l =
  let s = Bytes.create (List.length l) in
  loop (Bytes.length s - 1) l where rec loop i =
    fun
    [ [c :: l] -> do { Bytes.unsafe_set s i c; loop (i - 1) l }
    | [] -> Bytes.to_string s ]
;

value latin_1_of_utf_8 s =
  loop 0 [] where rec loop i rev_cl =
    if i = String.length s then rev_implode rev_cl
    else
      let c = s.[i] in
      match Char.code c with
      [ 0xC2 when i + 1 < String.length s ->
          loop (i + 2) [s.[i+1] :: rev_cl]
      | 0xC3 when i + 1 < String.length s ->
          loop (i + 2) [Char.chr (Char.code s.[i+1] + 0x40) :: rev_cl]
      | 0xE2 when i + 2 < String.length s ->
          loop (i + 3) ['@' :: rev_cl]
      | _ ->
          let c = if Char.code c > 127 then '#' else c in
          loop (i + 1) [c :: rev_cl] ]
;

value set_std_prop =
  let xsh = mallocated_var alloc_XSizeHints (ref None) in
  fun wid wname iname x y wsh flags -> do {
    let xd = wid.wid_xd in
    let wname =
       match xd.char_set with
       [ Latin_1 -> wname
       | Utf_8 -> latin_1_of_utf_8 wname ]
    in
    let iname =
       match xd.char_set with
       [ Latin_1 -> iname
       | Utf_8 -> latin_1_of_utf_8 iname ]
    in
    let xsh = xsh () in
    set_XSizeHints_x (x, xsh);
    set_XSizeHints_y (y, xsh);
    set_XSizeHints_width (wsh.sh_width, xsh);
    set_XSizeHints_height (wsh.sh_height, xsh);
    let flags =
      if wsh.width_inc == -1 && wsh.height_inc == -1 then flags
      else do {
        (* xsh.width must be xsh.base_width + k * xsh.width_inc for some k.
           In other words, xsh.width and xsh.base_width must have the same
           rest of the division by xsh.width_inc.
           (same thing for height)
           Otherwise the Window Manager (or the X server, I don't know),
           may generate a ConfigureNotify to change the size! *)
        let width_inc = max 1 wsh.width_inc in
        let height_inc = max 1 wsh.height_inc in
        let min_width =
          (* = wsh.base_width + r, so that this sum has the same rest by
             division by width_inc as wsh.sh_width *)
          wsh.sh_width -
          (wsh.sh_width - wsh.base_width) / width_inc * width_inc
        in
        let min_height =
          (* same comment as for "min_width" above *)
          wsh.sh_height -
          (wsh.sh_height - wsh.base_height) / height_inc * height_inc
        in
        set_XSizeHints_min_width (min_width, xsh);
        set_XSizeHints_min_height (min_height, xsh);
        set_XSizeHints_width_inc (width_inc, xsh);
        set_XSizeHints_height_inc (height_inc, xsh);
        flags lor pMinSize lor pResizeInc
      }
    in
    set_XSizeHints_flags (flags, xsh);
    xSetStandardProperties
      (xd.dpy, wid.win, wname, iname, xNone, Sys.argv.(0), xsh)
  }
;

value lnot i = -i - 1;

value make_array len f = do {
  let v = Array.make len (Obj.magic 0) in
  for i = 0 to len - 1 do { v.(i) := f i };
  v
};

value item_modified state item =
  {shiftMod = state land shiftMask != 0; shiftLock = state land lockMask != 0;
   control = state land controlMask != 0; mod1 = state land mod1Mask != 0;
   item = item}
;

value attribute_values =
  let backg_att = ref None in
  let band_att = ref None in
  let bd_backg_att = ref None in
  let bold_att = ref None in
  let border_att = ref None in
  let font_att = ref [| |] in
  let foreg_att = ref None in
  let height_att = ref None in
  let inter_att = ref None in
  let left_justif = ref False in
  let name_att = ref None in
  let width_att = ref None in
  fun attr -> do {
    List.iter
      (fun
       [ BackgroundAtt v -> backg_att.val := Some v
       | BandAtt v -> band_att.val := Some v
       | BoldAtt v -> bold_att.val := Some v
       | BorderBackgAtt v -> bd_backg_att.val := Some v
       | BorderAtt v -> border_att.val := Some v
       | FontAtt v -> font_att.val := v
       | ForegroundAtt v -> foreg_att.val := Some v
       | HeightAtt v -> height_att.val := Some v
       | InterAtt v -> inter_att.val := Some v
       | LeftJustifAtt -> left_justif.val := True
       | NameAtt v -> name_att.val := Some v
       | WidthAtt v -> width_att.val := Some v
       | _ -> () ])
      attr;
    let r =
      {backg_att = backg_att.val; band_att = band_att.val;
       bd_backg_att = bd_backg_att.val; border_att = border_att.val;
       bold_att = bold_att.val; font_att = font_att.val;
       foreg_att = foreg_att.val; height_att = height_att.val;
       inter_att = inter_att.val; left_justif = left_justif.val;
       name_att = name_att.val; width_att = width_att.val}
    in
    backg_att.val := None;
    band_att.val := None;
    bd_backg_att.val := None;
    border_att.val := None;
    bold_att.val := None;
    font_att.val := [| |];
    foreg_att.val := None;
    height_att.val := None;
    inter_att.val := None;
    left_justif.val := False;
    name_att.val := None;
    width_att.val := None;
    r
  }
;

value nround x y =
  if x > 0 && y > 0 || x < 0 && y < 0 then (2 * x + y) / (2 * y)
  else (2 * x - y) / (2 * y)
;

value mX = 255;

value hsv_of_rgb (r, g, b) =
  let r = max 0 (min mX r) in
  let g = max 0 (min mX g) in
  let b = max 0 (min mX b) in
  let v = max r (max g b)
  and x = min r (min g b) in
  (if v = 0 || v = x then 0
   else
     let r0 = (v - r) * mX / (v - x)
     and g0 = (v - g) * mX / (v - x)
     and b0 = (v - b) * mX / (v - x) in
     let h =
       if r = v then if g = x then 5 * mX + b0 else mX - g0
       else if g = v then if b = x then mX + r0 else 3 * mX - b0
       else if r = x then 3 * mX + g0
       else 5 * mX - r0
     in
     h / 6,
   if v = 0 then 0 else nround (mX * (v - x)) v, v)
;

value rgb_of_hsv (h, s, v) =
  let h = max 0 (min mX h) in
  let s = max 0 (min mX s) in
  let v = max 0 (min mX v) in
  let h = h * 6 in
  let i = h / mX * mX in
  let f = h - i in
  let m = v * (mX - s) / mX
  and n = v * (mX - s * f / mX) / mX
  and k = v * (mX - s * (mX - f) / mX) / mX in
  (nround
     (mX *
      (match i / mX with
       [ 0 | 6 -> v
       | 1 -> n
       | 2 -> m
       | 3 -> m
       | 4 -> k
       | 5 -> v
       | _ -> failwith "red_of_hsv" ]))
     mX,
   nround
     (mX *
      (match i / mX with
       [ 0 | 6 -> k
       | 1 -> v
       | 2 -> v
       | 3 -> n
       | 4 -> m
       | 5 -> m
       | _ -> failwith "green_of_hsv" ]))
     mX,
   nround
     (mX *
      (match i / mX with
       [ 0 | 6 -> m
       | 1 -> m
       | 2 -> k
       | 3 -> v
       | 4 -> v
       | 5 -> n
       | _ -> failwith "blue_of_hsv" ]))
     mX)
;

value get_default dpy str dval =
  let v = xGetDefault (dpy, "rt", str) in
  if is_null_C_String v then dval
  else string_of_C_String (v, c_String_length v)
;

value keysym_int_of_xevent xev =
  let buff = String.make 50 ' ' in
  let (k, len) = xLookupString (xev, buff, String.length buff) in
  k
;

value map_popup_widget wid x y lev = do {
  let xd = wid.wid_xd in
  let x = min (xd.root_width - wid.width) (max 0 x)
  and y = min (xd.root_height - wid.height) (max 0 y) in
  unmap (List.length xd.popped_up - lev) xd.popped_up where rec unmap n widl =
    if n <= 0 then xd.popped_up := [wid :: widl]
    else
      match widl with
      [ [w :: wl] ->
          if wl <> [] || w != wid then do {
            w.is_mapped := False;
            xUnmapWindow (xd.dpy, w.win);
            unmap (n - 1) wl
          }
          else ()
      | [] -> unmap (n - 1) [] ];
  wid.is_mapped := True;
  xMoveWindow (xd.dpy, wid.win, x, y);
  xMapRaised (xd.dpy, wid.win)
};

exception Bad_wid;

value allModMask =
  mod1Mask (* lor mod2Mask *) lor mod3Mask lor mod4Mask lor mod5Mask
;

value x_select_widget wid = do {
  let xd = wid.wid_xd in
  match xd.x_selected_wid with
  [ None ->
      let bwid =
        match xd.redirected_wid with
        [ None -> wid
        | Some rwid -> rwid ]
      in
      bwid.wdesc.wselect bwid True
  | Some swid ->
      match xd.redirected_wid with
      [ None ->
          if swid != wid then do {
            swid.wdesc.wselect swid False;
            wid.wdesc.wselect wid True
          }
          else ()
      | Some _ -> () ] ];
  xd.x_selected_wid := Some wid
};

value x_unselect_widget xd = do {
  match xd.x_selected_wid with
  [ None -> ()
  | Some swid ->
      let bwid =
        match xd.redirected_wid with
        [ None -> swid
        | Some rwid -> rwid ]
      in
      bwid.wdesc.wselect bwid False ];
  xd.x_selected_wid := None
};

value string_of_xevent_keysym =
  ref (fun _ -> failwith "call to string_of_xevent_keysym before evaluation")
;

value treat_next_event xd xev = do {
  xNextEvent (xd.dpy, xev);
  let win = xAnyEvent_window (xEvent_xany xev) in
  try
    let wid = try hash_assoc win xd.wid_by_win with _ -> raise Bad_wid in
    let t = xEvent_type xev in
    let wid_opt =
      if t == clientMessage then
        let xev = xEvent_xclient xev in
        if xClientMessageEvent_message_type xev = xd.wm_protocols &&
           xClientMessageEvent_data_l_0 xev = xd.wm_delete_window
        then
          match wid.delete_callback with
          [ Some callb -> do { callb wid; None }
          | None -> Some wid ]
        else Some wid
      else if t == buttonRelease then do {
        List.iter
          (fun wid -> do {
             wid.is_mapped := False;
             if wid.deleted then () else xUnmapWindow (xd.dpy, wid.win)
           })
          xd.popped_up;
        xd.popped_up := [];
        Some wid
      }
      else if t == mappingNotify then do {
        xRefreshKeyboardMapping (xEvent_xmapping xev);
        Some wid
      }
      else if t == focusIn then do {
        let xev = xEvent_xfocus xev in
        let detail = xFocusChangeEvent_detail xev in
        if detail != notifyPointer then xd.focus := True else ();
        x_select_widget wid;
        Some wid
      }
      else if t == focusOut then do {
        let xev = xEvent_xfocus xev in
        let detail = xFocusChangeEvent_detail xev in
        if detail != notifyPointer then xd.focus := False else ();
        x_unselect_widget xd;
        Some wid
      }
      else if t == enterNotify then do {
        let xev = xEvent_xcrossing xev in
        let detail = xCrossingEvent_detail xev in
        if detail != notifyVirtual && detail != notifyNonlinearVirtual &&
           xCrossingEvent_focus xev != 0
        then
          x_select_widget wid
        else x_unselect_widget xd;
        Some wid
      }
      else if t == leaveNotify then do {
        let xev = xEvent_xcrossing xev in
        let detail = xCrossingEvent_detail xev in
        if not xd.focus || xCrossingEvent_focus xev == 0 ||
           not wid.is_toplevel || detail == notifyInferior
        then
          x_unselect_widget xd
        else x_select_widget wid;
        Some wid
      }
      else if t == keyPress then
        let xkey_ev = xEvent_xkey xev in
        let state = xKeyEvent_state xkey_ev in
        let key_shortcut =
          if state land allModMask <> 0 then
            let s = string_of_xevent_keysym.val xev in
            if String.length s = 1 then
              try Some (List.assoc s.[0] xd.wid_by_shortcut) with
              [ Not_found -> None ]
            else None
          else None
        in
        match key_shortcut with
        [ Some wid -> Some wid
        | None ->
            if List.memq wid xd.no_redirecting_wid then Some wid
            else
              match xd.redirected_wid with
              [ Some wid -> Some wid
              | None -> Some wid ] ]
      else Some wid
    in
    match wid_opt with
    [ Some wid -> wid.wdesc.wdispatch wid xev
    | None -> () ]
  with
  [ Bad_wid -> () ]
};

value try_pending xdl xev =
  List.fold_left
    (fun ok xd ->
       if xPending xd.dpy <> 0 then do { treat_next_event xd xev; True }
       else ok)
    False xdl
and dispatch xa xev fds = do {
  List.iter
    (fun (fd, fd_fun) -> if fD_ISSET (fd, fds) <> 0 then fd_fun fd else ())
    xa.fd_list;
  List.iter
    (fun xd ->
       if fD_ISSET (xd.connection_number, fds) <> 0 then
         treat_next_event xd xev
       else ())
    xa.xdl
};

value pix_of_mm xd v =
  truncate (v *. float xd.root_width /. float (fst (xd.root_size_mm)) +. 0.5)
;
