(* $Id: vie.ml,v 1.8 2008/07/21 09:50:13 deraugla Exp $ *)

open Rt;
open Fvie;

value lshift_left a b = a lsl b;
value lshift_right a b = a lsr b;
type vect 'a = array 'a;
value string_length = String.length;
value sub_string = String.sub;
value fvect__vect_item = Array.get;
value set_nth_char = String.set;
value int_of_char = Char.code;
value create_string = String.create;
value nth_char = String.get;
value make_string = String.make;
value map = List.map;
value make_vect = Array.make;
value vect_length = Array.length;

value cWID = ref 150 and cHEI = ref 100 and cSQ = 10 and cCOLOR = ref True;

value genc = lshift_left 1 8;

value tag_mask = lshift_left (lshift_left 1 8 - 1) 16
and genc_mask = lshift_left (lshift_left 1 8 - 1) 8
and sum_mask = lshift_left (lshift_left 1 8 - 1) 0;

value elem_tag e = e land tag_mask
and elem_genc e = e land genc_mask
and elem_sum e = e land sum_mask;

value get_genc e = lshift_right (elem_genc e) 8;

value make_tag t = lshift_left t 16
and make_genc r = lshift_left r 8
and make_sum s = s;

value noElem = make_tag 0
and bornElem = make_tag 1
and normElem = make_tag 2
and deadElem = make_tag 3;

type game =
  { board : mutable vect (vect elem);
    x_shift : mutable int;
    y_shift : mutable int;
    backg : color;
    foreg : vect color;
    birth : color;
    death : color;
    board_pix : pixmap;
    grille_pix : pixmap;
    period : mutable int;
    running : mutable bool;
    step : mutable bool;
    inter : mutable bool;
    time : mutable int;
    invisible : mutable bool;
    reading : mutable bool;
    genc : mutable int }
;

value max x y = if x > y then x else y and min x y = if x < y then x else y;

value map_popup xd popup_wid button_wid =
  fun
  [ ButtonEvPress xll yll but -> rt_map_popup_widget popup_wid xll yll 0
  | ButtonEvEnter xll yll -> rt_map_popup_widget popup_wid xll yll 0
  | _ -> () ]
;

value my_scroll_set vmin vmax shift act wid =
  fun
  [ ScrollEvButtonMotion valu ->
      let but = 2 in
      let oval = scroll_val wid - shift in
      let valu =
        if but = 1 then oval - 1 else if but = 3 then oval + 1 else valu
      in
      let valu = max vmin (min vmax valu) in
      if valu <> oval then do { scroll_set wid (valu + shift); act valu }
      else ()
  | _ -> () ]
;

value my_text_button xd wid (lin, char_col, curs_col, but) =
  let txt = rt_get_cut_buffer xd in
  for i = 0 to string_length txt - 1 do {
    match txt.[i] with
    [ ' '..'~' -> term_send wid (sub_string txt i 1)
    | _ -> () ];
  }
;

value c_update_board xd drw board x_shift y_shift width height gm = do {
  let glop = not gm.step || gm.inter in
  let curcol = ref gm.foreg.(0) in
  rt_select_color curcol.val;
  for i = 0 to width - 1 do {
    let x = i * cSQ in
    let i = (i + x_shift) mod cWID.val in
    let v = fvect__vect_item board i in
    for j = 0 to height - 1 do {
      let y = j * cSQ in
      let j = (j + y_shift) mod cHEI.val in
      let e = fvect__vect_item v j in
      let t = elem_tag e in
      if t == bornElem then do {
        let newcol = if glop then gm.foreg.(get_genc e) else gm.birth in
        if newcol != curcol.val then
          rt_select_color (do { curcol.val := newcol; curcol.val })
        else ();
        rt_fill_rectangle drw (x + 2, y + 2, cSQ - 3, cSQ - 3)
      }
      else if t == deadElem then do {
        let newcol = if glop then gm.backg else gm.death in
        if newcol != curcol.val then
          rt_select_color (do { curcol.val := newcol; curcol.val })
        else ();
        rt_fill_rectangle drw (x + 2, y + 2, cSQ - 3, cSQ - 3)
      }
      else ();
    };
  }
};

value update_board xd gm =
  if not gm.invisible then
    let wid = widget_named xd "low_res" in
    let drw = WidgetDr wid in
    let width = widget_width wid / cSQ
    and height = widget_height wid / cSQ in
    let _ =
      c_update_board xd drw gm.board gm.x_shift gm.y_shift width height gm
    in
    ()
  else ()
;

value draw_elem gm drw x y i j =
  let e = gm.board.(i).(j) in
  let v = elem_tag e in
  if v == bornElem then do {
    rt_select_color gm.birth;
    rt_fill_rectangle drw
      (x / cSQ * cSQ + 2, y / cSQ * cSQ + 2, cSQ - 3, cSQ - 3)
  }
  else if v == normElem then do {
    rt_select_color gm.foreg.(get_genc e);
    rt_fill_rectangle drw
      (x / cSQ * cSQ + 2, y / cSQ * cSQ + 2, cSQ - 3, cSQ - 3)
  }
  else if v == deadElem then do {
    rt_select_color gm.death;
    rt_fill_rectangle drw
      (x / cSQ * cSQ + 2, y / cSQ * cSQ + 2, cSQ - 3, cSQ - 3)
  }
  else ()
;

value draw_board_line wid gm j =
  let drw = WidgetDr wid in
  let width = widget_width wid / cSQ in
  let y = j * cSQ in
  let j = (j + gm.y_shift) mod cHEI.val in
  for i = 0 to width - 1 do {
    let x = i * cSQ in
    let i = (i + gm.x_shift) mod cWID.val in
    draw_elem gm drw x y i j;
  }
;

value draw_board_column wid gm i =
  let drw = WidgetDr wid in
  let height = widget_height wid / cSQ in
  let x = i * cSQ in
  let i = (i + gm.x_shift) mod cWID.val in
  for j = 0 to height - 1 do {
    let y = j * cSQ in
    let j = (j + gm.y_shift) mod cHEI.val in
    draw_elem gm drw x y i j;
  }
;

value draw_board wid gm =
  let drw = WidgetDr wid in
  let width = widget_width wid / cSQ
  and height = widget_height wid / cSQ in
  for i = 0 to width - 1 do {
    let x = i * cSQ in
    let i = (i + gm.x_shift) mod cWID.val in
    for j = 0 to height - 1 do {
      let y = j * cSQ in
      let j = (j + gm.y_shift) mod cHEI.val in
      draw_elem gm drw x y i j;
    };
  }
;

value set_val xd valu = do {
  term_send (widget_named xd "period_txt") "\027[2J\027[H";
  term_send (widget_named xd "period_txt") (string_of_int valu)
};

value set_time xd gm = do {
  let time = "      " ^ string_of_int gm.time in
  let time = sub_string time (string_length time - 6) 6 in
  let wid = widget_named xd "time" in
  term_send wid "\027[0;5f";
  term_send wid time;
  term_send wid (if gm.inter then ":" else " ")
};

value stop_fun xa xd gm _ = do {
  gm.running := False;
  rt_cancel_timeout xa "aa";
  rt_raise_widget (widget_named xd "start");
  let wid = widget_named xd "low_res" in
  rt_change_background wid (PixmapPn gm.grille_pix);
  rt_clear_widget wid;
  draw_board wid gm
};

value one_generation xa xd gm _ = do {
  if not gm.step then do {
    let _ = c_one_step gm.board cWID.val cHEI.val in
    update_board xd gm;
    if not (c_update gm.board cWID.val cHEI.val) then stop_fun xa xd gm ()
    else ();
    gm.time := gm.time + 1;
    ()
  }
  else do {
    if not gm.inter then
      let _ = c_one_step gm.board cWID.val cHEI.val in
      update_board xd gm
    else do {
      update_board xd gm;
      if not (c_update gm.board cWID.val cHEI.val) then stop_fun xa xd gm ()
      else ();
      gm.time := gm.time + 1;
      ()
    };
    gm.inter := not gm.inter;
    ()
  };
  set_time xd gm
};

value quit_fun xa _ = rt_stop_main_loop xa;

value clear_fun xd gm _ = do {
  c_fill gm.board cWID.val cHEI.val noElem;
  let wid = widget_named xd "low_res" in
  rt_clear_widget wid;
  gm.time := 0;
  gm.x_shift := 0;
  gm.y_shift := 0;
  set_time xd gm
};

value rec timeout xa xd gm = do {
  rt_cancel_timeout xa "aa";
  rt_set_timeout xa "aa" (rt_current_time xa + gm.period)
    (fun () -> do { timeout xa xd gm; one_generation xa xd gm () })
};

value start_fun xa xd gm _ = do {
  gm.running := True;
  rt_raise_widget (widget_named xd "stop");
  let wid = widget_named xd "low_res" in
  rt_change_background (widget_named xd "low_res") (ColorPn gm.backg);
  rt_clear_widget wid;
  draw_board wid gm;
  one_generation xa xd gm ();
  timeout xa xd gm
};

value step_fun xd gm _ = do {
  gm.step := True;
  rt_raise_widget (widget_named xd "normal")
};

value normal_fun xd gm _ = do {
  gm.step := False;
  gm.inter := False;
  update_board xd gm;
  let _ = c_update gm.board cWID.val cHEI.val in
  rt_raise_widget (widget_named xd "step")
};

value high_res_fun xd _ = do {
  rt_raise_widget (widget_named xd "high_res");
  rt_raise_widget (widget_named xd "low")
};

value low_res_fun xd _ = do {
  rt_raise_widget (widget_named xd "low_res");
  rt_raise_widget (widget_named xd "high")
};

value genc_fun xd gm genc _ = do {
  rt_raise_widget (widget_named xd ("code_" ^ string_of_int gm.genc ^ "0"));
  gm.genc := genc;
  rt_raise_widget (widget_named xd ("code_" ^ string_of_int gm.genc ^ "1"))
};

value expose_fun gm wid (x, y, width, height) = draw_board wid gm;

value set_elem draw wid gm i j x y =
  if elem_tag gm.board.(i).(j) == noElem then do {
    let vm1 = gm.board.(if i == 0 then cWID.val - 1 else i - 1)
    and v = gm.board.(i)
    and vp1 = gm.board.(if i == cWID.val - 1 then 0 else i + 1)
    and jm1 = if j == 0 then cHEI.val - 1 else j - 1
    and jp1 = if j == cHEI.val - 1 then 0 else j + 1 in
    vm1.(jm1) := vm1.(jm1) + 1;
    vm1.(j) := vm1.(j) + 1;
    vm1.(jp1) := vm1.(jp1) + 1;
    v.(jm1) := v.(jm1) + 1;
    v.(jp1) := v.(jp1) + 1;
    vp1.(jm1) := vp1.(jm1) + 1;
    vp1.(j) := vp1.(j) + 1;
    vp1.(jp1) := vp1.(jp1) + 1;
    v.(j) := normElem + make_genc gm.genc + elem_sum v.(j);
    rt_select_color gm.foreg.(gm.genc);
    rt_draw_point (PixmapDr gm.board_pix) (i, j);
    if draw then
      rt_fill_rectangle (WidgetDr wid)
        (x / cSQ * cSQ + 2, y / cSQ * cSQ + 2, cSQ - 3, cSQ - 3)
    else ()
  }
  else ()
;

value unset_elem draw wid gm i j x y =
  if elem_tag gm.board.(i).(j) == normElem then do {
    let vm1 = gm.board.(if i == 0 then cWID.val - 1 else i - 1)
    and v = gm.board.(i)
    and vp1 = gm.board.(if i == cWID.val - 1 then 0 else i + 1)
    and jm1 = if j == 0 then cHEI.val - 1 else j - 1
    and jp1 = if j == cHEI.val - 1 then 0 else j + 1 in
    vm1.(jm1) := vm1.(jm1) - 1;
    vm1.(j) := vm1.(j) - 1;
    vm1.(jp1) := vm1.(jp1) - 1;
    v.(jm1) := v.(jm1) - 1;
    v.(jp1) := v.(jp1) - 1;
    vp1.(jm1) := vp1.(jm1) - 1;
    vp1.(j) := vp1.(j) - 1;
    vp1.(jp1) := vp1.(jp1) - 1;
    v.(j) := noElem + elem_sum v.(j);
    rt_select_color gm.backg;
    rt_draw_point (PixmapDr gm.board_pix) (i, j);
    if draw then
      rt_fill_rectangle (WidgetDr wid)
        (x / cSQ * cSQ + 2, y / cSQ * cSQ + 2, cSQ - 3, cSQ - 3)
    else ()
  }
  else ()
;

value breleased_fun xd gm wid =
  if gm.reading then
    my_text_button xd (widget_named xd "text_file") (0, 0, 0, 2)
  else ()
;

value bpressed_fun gm wid (x, y, _, _, but) =
  if not gm.reading then
    let i = (gm.x_shift + x / cSQ) mod cWID.val
    and j = (gm.y_shift + y / cSQ) mod cHEI.val in
    if elem_tag gm.board.(i).(j) == noElem then
      if but.item == 2 || but.item == 3 then set_elem True wid gm i j x y
      else ()
    else if elem_tag gm.board.(i).(j) == normElem then
      if but.item == 1 || but.item == 2 then unset_elem True wid gm i j x y
      else ()
    else ()
  else ()
;

value read_fun xd gm _ =
  fun
  [ ButtonEvRelease x y _ -> do {
      gm.reading := True;
      rt_map_transient_widget (widget_named xd "enter_file") x y
    }
  | _ -> () ]
;

value input_line ic buff beg len =
  input_rec 0 where rec input_rec i =
    match input_char ic with
    [ '\n' -> i
    | c ->
        if i >= len then i
        else do { set_nth_char buff (beg + i) c; input_rec (succ i) } ]
;

value input_int ic =
  let c =
    skip_no_digit_rec (input_char ic) where rec skip_no_digit_rec c =
      match c with
      [ '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> c
      | _ -> skip_no_digit_rec (input_char ic) ]
  in
  let (c, sign) = if c == '-' then (input_char ic, -1) else (c, 1) in
  sign *
  (input_rec c 0 where rec input_rec c v =
     match c with
     [ '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
         input_rec (input_char ic)
           (v * 10 + (int_of_char c - int_of_char '0'))
     | _ -> v ])
;

value pmod x y =
  let r = x mod y in
  if r >= 0 then r else r + y
;

value include_file =
  let buff = create_string 1000 in
  fun xd gm fname ->
    try do {
      let ic = try open_in fname with _ -> raise Not_found in
      try do {
        let len = input_line ic buff 0 (string_length buff) in
        if len < 2 || nth_char buff 0 != '#' then failwith "incorrect 1"
        else ();
        let c = nth_char buff 1
        and wid = widget_named xd "low_res" in
        let (imin, jmin, imax, jmax) =
          match c with
          [ 'P' ->
              fill_rec 0 0 0 where rec fill_rec j imax jmax =
                try do {
                  let len = input_line ic buff 0 (string_length buff) in
                  for i = 0 to len - 1 do {
                    (match nth_char buff i with
                     [ '.' -> unset_elem
                     | '*' -> set_elem
                     | _ -> failwith "incorrect char" ])
                      False wid gm ((i + gm.x_shift) mod cWID.val)
                      ((j + gm.y_shift) mod cHEI.val) 0 0;
                  };
                  fill_rec (succ j) (max len imax) (max j jmax)
                }
                with
                [ End_of_file -> (0, 0, imax, jmax) ]
          | 'R' | _ -> do {
              if c == 'R' then set_elem False wid gm 0 0 0 0 else ();
              let rec fill_rec imin jmin imax jmax =
                try do {
                  let i = input_int ic
                  and j = input_int ic in
                  set_elem False wid gm (pmod (i + gm.x_shift) cWID.val)
                    (pmod (j + gm.y_shift) cHEI.val) 0 0;
                  fill_rec (min i imin) (min j jmin) (max i imax) (max j jmax)
                }
                with
                [ End_of_file -> (imin, jmin, imax, jmax) ]
              in
              fill_rec 1000 1000 (-1000) (-1000)
            } ]
        in
        print_string "size ";
        print_int (imax - imin + 1);
        print_string " ";
        print_int (jmax - jmin + 1);
        print_newline ();
        gm.x_shift :=
          pmod (gm.x_shift + (imax + imin - widget_width wid / cSQ) / 2)
            cWID.val;
        gm.y_shift :=
          pmod (gm.y_shift + (jmax + jmin - widget_height wid / cSQ) / 2)
            cHEI.val;
        rt_clear_widget wid;
        draw_board wid gm
      }
      with
      [ Failure x -> do { print_string x; print_newline () }
      | _ -> do { print_string "uncaught exception"; print_newline () } ];
      close_in ic
    }
    with not_found -> do {
      print_string "file not found: ";
      print_string fname;
      print_newline ()
    }
;

value read_file_name xd gm (wid, km) =
  match km with
  [ {item = K_Return} -> do {
      gm.reading := False;
      rt_unmap_widget (widget_named xd "enter_file");
      term_emphasize_from wid 0 0;
      let (_, len) = term_size wid in
      term_emphasize_to wid 0 len;
      let fname = term_get_emphasized wid in
      term_emphasize_from wid 0 0;
      let fname =
        loop (string_length fname) where rec loop i =
          if i == 0 then ""
          else
            match fname.[i-1] with
            [ ' ' -> loop (i - 1)
            | _ -> sub_string fname 0 (i - 1) ]
      in
      include_file xd gm fname
    }
  | {item = K_Ascii 'g'; control = True} -> do {
      gm.reading := False;
      rt_unmap_widget (widget_named xd "enter_file")
    }
  | {item = K_Ascii 'u'; control = True} -> term_send wid "\027[2J\027[H"
  | {item = K_BackSpace | K_Delete} ->
      let (_, len) = term_current_position wid in
      if len > 0 then term_send wid "\008 \008" else ()
  | {item = K_Ascii c} -> term_send wid (make_string 1 c)
  | _ -> () ]
;

value save_fun xd gm _ =
  try do {
    let oc = open_out "vie.save" in
    try output_value oc gm.board with _ ->
      do { print_string "can't save"; print_newline () };
    close_out oc
  }
  with _ -> do {
    print_string "can't open or close save file";
    print_newline ()
  }
;

value restore_fun xd gm _ = do {
  try do {
    let ic = open_in "vie.save" in
    try do { gm.board := input_value ic; () } with _ ->
      do { print_string "can't restore"; print_newline () };
    close_in ic
  }
  with _ -> do {
    print_string "can't open or close save file";
    print_newline ()
  };
  let wid = widget_named xd "low_res" in
  rt_clear_widget wid;
  draw_board wid gm
};

value keyp_fun xa xd gm wid km =
  if gm.reading then read_file_name xd gm (widget_named xd "text_file", km)
  else
    match km.item with
    [ K_Ascii 'q' -> quit_fun xa ()
    | K_Ascii 'g' -> start_fun xa xd gm ()
    | K_Ascii 's' -> stop_fun xa xd gm ()
    | K_Ascii '1' -> one_generation xa xd gm ()
    | K_Ascii 'c' -> clear_fun xd gm ()
    | K_Ascii '=' -> step_fun xd gm ()
    | K_Ascii '-' -> normal_fun xd gm ()
    | K_Ascii 'v' -> save_fun xd gm ()
    | K_Ascii 'r' -> restore_fun xd gm ()
    | K_Right -> do {
        gm.x_shift := (gm.x_shift + 1) mod cWID.val;
        let wid = widget_named xd "low_res" in
        let drw = WidgetDr wid
        and width = widget_width wid
        and height = widget_height wid in
        rt_copy_area drw drw (cSQ, 0, width - cSQ, height) (0, 0);
        rt_clear_area wid (width - cSQ, 0, cSQ, height);
        draw_board_column wid gm (width / cSQ - 1)
      }
    | K_Left -> do {
        gm.x_shift := (gm.x_shift + cWID.val - 1) mod cWID.val;
        let wid = widget_named xd "low_res" in
        let drw = WidgetDr wid
        and width = widget_width wid
        and height = widget_height wid in
        rt_copy_area drw drw (0, 0, width - cSQ, height) (cSQ, 0);
        rt_clear_area wid (0, 0, cSQ, height);
        draw_board_column wid gm 0
      }
    | K_Down -> do {
        gm.y_shift := (gm.y_shift + 1) mod cHEI.val;
        let wid = widget_named xd "low_res" in
        let drw = WidgetDr wid
        and width = widget_width wid
        and height = widget_height wid in
        rt_copy_area drw drw (0, cSQ, width, height - cSQ) (0, 0);
        rt_clear_area wid (0, height - cSQ, width, cSQ);
        draw_board_line wid gm (height / cSQ - 1)
      }
    | K_Up -> do {
        gm.y_shift := (gm.y_shift + cHEI.val - 1) mod cHEI.val;
        let wid = widget_named xd "low_res" in
        let drw = WidgetDr wid
        and width = widget_width wid
        and height = widget_height wid in
        rt_copy_area drw drw (0, 0, width, height - cSQ) (0, cSQ);
        rt_clear_area wid (0, 0, width, cSQ);
        draw_board_line wid gm 0
      }
    | _ -> () ]
;

value pack1_desc attr args = pack_desc attr args (fun _ _ -> ());
value button1_desc attr txt = button_desc attr (txt, None);

value popup_desc attr sfl =
  pack1_desc []
    (Vertical,
     map
       (fun (s, f) ->
          button1_desc [LeftJustifAtt; BorderAtt 0] s
            (fun _ ->
               fun
               [ ButtonEvRelease _ _ _ -> f ()
               | _ -> () ]))
       sfl)
;

value button_act f wid =
  fun
  [ ButtonEvRelease _ _ _ -> f wid
  | _ -> () ]
;

value vie dname = do {
  let xd = rt_initialize dname in
  try do {
    let xa = rt_args [xd] in
    let is_color = is_colored xd && cCOLOR.val in
    let black = rt_black_color xd
    and white = rt_white_color xd in
    let gm =
      {board = make_vect cWID.val [| |]; x_shift = 0; y_shift = 0;
       backg = if is_color then rt_create_color xd (144, 220, 188) else white;
       foreg =
         [| if is_color then rt_create_color xd (65, 100, 85) else black;
            if is_color then rt_create_color xd (110, 100, 169) else black;
            if is_color then rt_create_color xd (155, 100, 169) else black;
            if is_color then rt_create_color xd (200, 100, 85) else black |];
       birth = if is_color then rt_create_color xd (255, 118, 118) else black;
       death = if is_color then rt_create_color xd (94, 117, 200) else white;
       board_pix = rt_create_pixmap xd cWID.val cHEI.val;
       grille_pix = rt_create_pixmap xd cSQ cSQ; period = 0; running = False;
       step = False; inter = False; time = 0; invisible = False;
       reading = False; genc = 1}
    in
    for i = 0 to vect_length gm.board - 1 do {
      gm.board.(i) := make_vect cHEI.val noElem;
    };
    let file_wid =
      rt_create_popup_widget xd
        (pack1_desc []
           (Vertical,
            [button1_desc [] "read file" (read_fun xd gm);
             button1_desc [] "save (v)" (button_act (save_fun xd gm));
             button1_desc [] "restore (r)" (button_act (restore_fun xd gm));
             button1_desc [] "quit (q)" (button_act (quit_fun xa))]))
    and edit_wid =
      rt_create_popup_widget xd
        (popup_desc [] [("clear (c)", clear_fun xd gm)])
    and control_wid =
      rt_create_popup_widget xd
        (pack1_desc []
           (Vertical,
            [pack1_desc [BandAtt 0; BorderAtt 0]
               (InDepth,
                [button1_desc [NameAtt "start"] "start (g)"
                   (button_act (start_fun xa xd gm));
                 button1_desc [NameAtt "stop"] "stop (s)"
                   (button_act (stop_fun xa xd gm))]);
             button1_desc [] "one generation (1)"
               (button_act (one_generation xa xd gm));
             button1_desc [] "" (fun _ _ -> ());
             pack1_desc [BandAtt 0; BorderAtt 0]
               (InDepth,
                [button1_desc [NameAtt "step"] "step by step (=)"
                   (button_act (step_fun xd gm));
                 button1_desc [NameAtt "normal"] "normal (-)"
                   (button_act (normal_fun xd gm))]);
             button1_desc [] "" (fun _ _ -> ());
             pack1_desc [BandAtt 0; BorderAtt 0]
               (InDepth,
                [button1_desc [NameAtt "high"] "high resolution"
                   (button_act (high_res_fun xd));
                 button1_desc [NameAtt "low"] "low resolution"
                   (button_act (low_res_fun xd))])]))
    and genetic_wid =
      rt_create_popup_widget xd
        (pack1_desc []
           (Vertical,
            [pack1_desc [BorderAtt 0; BandAtt 0]
               (InDepth,
                [button1_desc [NameAtt "code_00"] "no gene"
                   (button_act (genc_fun xd gm 0));
                 title_desc [BorderAtt 0; NameAtt "code_01"] "no gene"
                   (fun _ _ -> ())]);
             pack1_desc [BorderAtt 0; BandAtt 0]
               (InDepth,
                [button1_desc [NameAtt "code_10"] "1 gene / 3"
                   (button_act (genc_fun xd gm 1));
                 title_desc [BorderAtt 0; NameAtt "code_11"] "1 gene / 3"
                   (fun _ _ -> ())]);
             pack1_desc [BorderAtt 0; BandAtt 0]
               (InDepth,
                [button1_desc [NameAtt "code_20"] "2 genes / 3"
                   (button_act (genc_fun xd gm 2));
                 title_desc [BorderAtt 0; NameAtt "code_21"] "2 genes / 3"
                   (fun _ _ -> ())]);
             pack1_desc [BorderAtt 0; BandAtt 0]
               (InDepth,
                [button1_desc [NameAtt "code_30"] "all genes"
                   (button_act (genc_fun xd gm 3));
                 title_desc [BorderAtt 0; NameAtt "code_31"] "all genes"
                   (fun _ _ -> ())])]))
    in
    genc_fun xd gm gm.genc ();
    rt_select_color gm.backg;
    rt_fill_rectangle (PixmapDr gm.board_pix) (0, 0, cWID.val, cHEI.val);
    rt_fill_rectangle (PixmapDr gm.grille_pix) (0, 0, cSQ, cSQ);
    rt_select_color black;
    rt_draw_line (PixmapDr gm.grille_pix) (0, 0) (0, cSQ);
    rt_draw_line (PixmapDr gm.grille_pix) (0, 0) (cSQ, 0);
    let wid =
      rt_create_widget xd "vie" "vie" AutoPosition
        (Some (fun _ -> quit_fun xa ()))
        (pack1_desc []
           (Vertical,
            [pack1_desc []
               (Horizontal,
                [button1_desc [] "File" (map_popup xd file_wid);
                 button1_desc [] "Edit" (map_popup xd edit_wid);
                 button1_desc [] "Control" (map_popup xd control_wid);
                 button1_desc [] "Genetic" (map_popup xd genetic_wid);
                 pack1_desc [BorderAtt 0; FillerAtt] (Horizontal, []);
                 pack1_desc [BorderAtt 0; BandAtt 0]
                   (InDepth,
                    [button1_desc [NameAtt "invisible"] "invisible"
                       (button_act
                          (fun _ -> do {
                             gm.invisible := True;
                             rt_raise_widget (widget_named xd "visible")
                           }));
                     button1_desc [NameAtt "visible"] "visible"
                       (button_act
                          (fun _ -> do {
                             gm.invisible := False;
                             let wid = widget_named xd "low_res" in
                             rt_clear_widget wid;
                             draw_board wid gm;
                             rt_raise_widget (widget_named xd "invisible")
                           }))]);
                 term_desc [NameAtt "time"] (1, 12, 0) (fun _ _ -> ())]);
             pack1_desc [FillerAtt]
               (InDepth,
                [raw_desc
                   [NameAtt "low_res"; BackgroundAtt (PixmapPn gm.grille_pix)]
                   (50 * cSQ, 30 * cSQ, 3,
                    [SelExposure; SelButtonPress; SelButtonRelease;
                     SelKeyPress; SelStructureNotify])
                   (fun wid ->
                      fun
                      [ RawEvExpose a b c d -> expose_fun gm wid (a, b, c, d)
                      | RawEvButtonPress a b c d e ->
                          bpressed_fun gm wid (a, b, c, d, e)
                      | RawEvButtonRelease a b c d e ->
                          breleased_fun xd gm (a, b, c, d, e)
                      | RawEvKeyPress a -> keyp_fun xa xd gm wid a
                      | _ -> () ]);
                 raw_desc
                   [NameAtt "high_res"; BackgroundAtt (PixmapPn gm.board_pix)]
                   (cWID.val, cHEI.val, 3, []) (fun _ _ -> ())]);
             pack1_desc []
               (Horizontal,
                [title_desc [] "period in ms " (fun _ _ -> ());
                 scroll_desc [FillerAtt; NameAtt "scroll"]
                   (Horizontal, -3, 103, 6)
                   (my_scroll_set 0 100 3
                      (fun valu -> do {
                         gm.period := valu * 20;
                         if gm.running then timeout xa xd gm else ();
                         set_val xd gm.period
                       }));
                 term_desc [NameAtt "period_txt"] (1, 4, 0)
                   (fun _ _ -> ())])]))
    in
    scroll_set (widget_named xd "scroll") (gm.period / 20 + 3);
    set_val xd gm.period;
    term_send (widget_named xd "time") "time";
    set_time xd gm;
    let _read_wid =
      rt_create_transient_widget wid "enter file name" (Some (fun _ -> ()))
        (pack1_desc [NameAtt "enter_file"]
           (Horizontal,
            [title_desc [] "file: " (fun _ _ -> ());
             term_desc [NameAtt "text_file"] (1, 60, 0)
               (fun wid ->
                  fun
                  [ TermEvKeyPress km -> read_file_name xd gm (wid, km)
                  | TermEvButtonRelease a b c d ->
                      my_text_button xd wid (a, b, c, d)
                  | _ -> () ])]))
    in
    rt_map_widget wid;
    rt_main_loop xa
  }
  with x -> do { rt_end xd; raise x };
  rt_end xd
};
