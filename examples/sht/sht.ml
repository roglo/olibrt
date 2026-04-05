(* $Id: sht.ml,v 1.5 2007/07/05 18:13:12 deraugla Exp $
 *
 * Seahaven towers
 *)

open Rt;
open Sys;
open Time;

value line_buff = Bytes.create 200 and seed = ref 0;
value input_line ic =
  String.sub line_buff 0
    (input_rec 0 where rec input_rec i =
       match input_char ic with
       [ '\n' -> i
       | c -> do { Bytes.set line_buff i c; input_rec (succ i) } ])
and modify_vect f v =
  let len = Array.length v in
  modify_rec 0 where rec modify_rec i =
    if i < len then do { v.(i) := f v.(i); modify_rec (succ i) } else ()
and max x y = if x > y then x else y
and min x y = if x < y then x else y
and b f g x = f (g x)
and it_vect f a v =
  it_vect_f a 0 where rec it_vect_f a n =
    if n >= Array.length v then a else it_vect_f (f a v.(n)) (succ n)
and hd =
  fun
  [ [x :: l] -> x
  | _ -> failwith "hd" ]
and tl =
  fun
  [ [x :: l] -> l
  | _ -> failwith "tl" ]
and item =
  item_rec where rec item_rec =
    fun
    [ [a :: l] ->
        fun
        [ 0 -> a
        | i -> item_rec l (i - 1) ]
    | _ -> failwith "item" ]
and sqrt x = truncate (sqrt (float x))
and eq (x, y) = x == y
and random n = do {
  seed.val := 25173 * seed.val + 13849;
  (if seed.val < 0 then -seed.val else seed.val) mod n
}
and init_random n = do { seed.val := n; () }
and chop_list n l =
  chop_aux n ([], l) where rec chop_aux p0 p1 =
    match (p0, p1) with
    [ (0, (l1, l2)) -> (List.rev l1, l2)
    | (n, (l1, [x :: l2])) -> chop_aux (pred n) ([x :: l1], l2)
    | (_, _) -> failwith "chop_list" ]
and null =
  fun
  [ [] -> True
  | _ -> False ]
and do_list_i f =
  do_list_f where rec do_list_f i =
    fun
    [ [] -> ()
    | [x :: l] -> do { f i x; do_list_f (succ i) l } ]
and iter f =
  do_list_f where rec do_list_f =
    fun
    [ [] -> ()
    | [x :: l] -> do { f x; do_list_f l } ]
and message s = do { print_string s; print_newline () }
and gc_alarm _ = ()
and implode_ascii l =
  let len = List.length l in
  let s = String.make len ' ' in
  implode_rec l 0 where rec implode_rec p0 p1 =
    match (p0, p1) with
    [ ([x :: l], i) -> do {
        Bytes.set s i (Char.chr x);
        implode_rec l (succ i)
      }
    | (_, _) -> s ]
and iterate f =
  iterate_f where rec iterate_f n x =
    if n > 0 then iterate_f (pred n) (f x) else ()
and prim_rec f =
  prim_loop where rec prim_loop init =
    fun
    [ 0 -> init
    | n -> prim_loop (f init n) (n - 1) ]
;

value gloop f p =
  looprec where rec looprec x = if p x then x else looprec (f x)
;

value vect_exists p v =
  exists_p 0 where rec exists_p i =
    if i = Array.length v then False else p v.(i) || exists_p (i + 1)
;    

type suit = [ C'Clubs | C'Diamonds | C'Hearts | C'Spades ]
and val0 =
  [ C'Ace
  | C'King
  | C'Queen
  | C'Jack
  | C'Plain of int ]
;

type card =
  { suit : suit; val0 : val0; gm : game; position : mutable position }
and game_state =
  [ NotStarted
  | NormalMove
  | AutoMove of widget and position and int and int ]
and game =
  { xargs : xargs;
    towers : array (list widget);
    buffers : array (list widget);
    columns : array (list widget);
    wid_list : list widget;
    mover : widget;
    state : mutable game_state;
    undo : mutable list (widget * position);
    redo : mutable list (widget * position);
    bp : mutable bool;
    mwid : mutable list widget;
    xg : mutable int;
    yg : mutable int }
and position =
  [ Column of int
  | Buff of int
  | Tower of int
  | Nowhere ]
;

value bw = ref False and dname = ref "" and cards_file = ref "./cards";

let i = ref 1 in
while i.val < Array.length argv do {
  match argv.(i.val) with
  [ "-bw" -> bw.val := True
  | "-c" -> do { incr i; cards_file.val := argv.(i.val) }
  | "-d" -> do { incr i; cards_file.val := argv.(i.val) ^ "/cards" }
  | s -> dname.val := s ];
  incr i
};

value buff = Bytes.create 1024;
value (wID, hEI, data) = do {
  let ic = open_in cards_file.val in
  let x =
    try do {
      let width = int_of_string (input_line ic)
      and height = int_of_string (input_line ic)
      and data = Array.make 52 "" in
      modify_vect
        (fun _ -> do {
           let len = (width + 7) / 8 * height in
           really_input ic buff 0 len;
           String.sub buff 0 len
         })
        data;
      (width, height, data)
    }
    with x -> do { close_in ic; raise x }
  in
  close_in ic;
  x
}
and bD = 0
and leftB = 20
and rightB = 20
and upperB = 20
and lowerB = 20
and iC = 20
and sC = 50
and rC = 25;

value oW = bD + wID + bD
and oH = bD + hEI + bD
and sPEED = 500
and pERIOD = 60;

value gW = leftB + 10 * (oW + iC) - iC + rightB
and gH = upperB + oH + sC + 17 * rC + oH + lowerB
and dELTA = max 1 (sPEED * pERIOD / 1000);

value (card, get_card) : user_info_func card = user_info "card" (ref UNone)
and (game, get_game) : user_info_func game = user_info "game" (ref UNone);

value item_suit = Array.get [| C'Hearts; C'Diamonds; C'Clubs; C'Spades |]
and item_val =
  Array.get
    [| C'Ace; C'Plain 2; C'Plain 3; C'Plain 4; C'Plain 5; C'Plain 6;
       C'Plain 7; C'Plain 8; C'Plain 9; C'Plain 10; C'Jack; C'Queen; C'King |]
;

value suit_item =
  fun
  [ C'Hearts -> 0
  | C'Diamonds -> 1
  | C'Clubs -> 2
  | C'Spades -> 3 ]
and val_item =
  fun
  [ C'Ace -> 0
  | C'King -> 12
  | C'Queen -> 11
  | C'Jack -> 10
  | C'Plain n -> n - 1 ]
;

value suit_txt = b (Array.get [| "C"; "K"; "T"; "P" |]) suit_item
and val_txt =
  b
    (Array.get
       [| "A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"; "V"; "D"; "R" |])
    val_item
;

value col_x n = leftB + n * (oW + iC);

value tower_x = Array.get [| col_x 0; col_x 1; col_x 8; col_x 9 |]
and tower_y _ = upperB
and buff_x = Array.get [| col_x 3; col_x 4; col_x 5; col_x 6 |]
and buff_y _ = upperB
and column_x = col_x
and column_y gm nb = upperB + oH + sC + rC * List.length gm.columns.(nb);

value nth_free_buff gm =
  rec_nth 0 where rec rec_nth i j =
    if gm.buffers.(j) = [] then
      if i = 0 then Buff j else rec_nth (i - 1) (j + 1)
    else rec_nth i (j + 1)
and nb_free_buffers gm =
  it_vect (fun i widl -> i + (if widl = [] then 1 else 0)) 0 gm.buffers
;

value move_to pos wid = do {
  let card = get_card wid in
  let gm = card.gm in
  match card.position with
  [ Tower nb -> do { gm.towers.(nb) := tl gm.towers.(nb); () }
  | Buff nb -> do { gm.buffers.(nb) := tl gm.buffers.(nb); () }
  | Column nb -> do { gm.columns.(nb) := tl gm.columns.(nb); () }
  | Nowhere -> () ];
  match pos with
  [ Tower nb -> do {
      rt_move_widget wid (tower_x nb) (tower_y nb);
      gm.towers.(nb) := [wid :: gm.towers.(nb)];
      ()
    }
  | Buff nb -> do {
      rt_move_widget wid (buff_x nb) (buff_y nb);
      gm.buffers.(nb) := [wid :: gm.buffers.(nb)];
      ()
    }
  | Column nb -> do {
      rt_move_widget wid (column_x nb) (column_y gm nb);
      gm.columns.(nb) := [wid :: gm.columns.(nb)];
      ()
    }
  | Nowhere -> () ];
  card.position := pos;
  ()
};

value dist x1 y1 x2 y2 =
  let dx = x1 - x2
  and dy = y1 - y2 in
  sqrt (dx * dx + dy * dy)
;

value rec auto_move gm = do {
  let what dest card =
    what_rec 0 where rec what_rec i =
      if i = Array.length dest then -1
      else
        match dest.(i) with
        [ [swid :: _] ->
            let scard = get_card swid in
            if card.suit = scard.suit then
              if eq (dest, gm.towers) then
                if val_item card.val0 - val_item scard.val0 = 1 then i else -1
              else if val_item card.val0 - val_item scard.val0 = -1 then i
              else what_rec (i + 1)
            else what_rec (i + 1)
        | _ ->
            if eq (dest, gm.towers) && card.val0 = C'Ace then i
            else what_rec (i + 1) ]
  in
  let test_push_on dest =
    fun
    [ [wid :: _] ->
        let card = get_card wid in
        let nb = what dest card in
        if nb <> -1 then do {
          gm.state :=
            if eq (dest, gm.towers) then
              AutoMove wid (Tower nb) (tower_x nb) (tower_y nb)
            else AutoMove wid (Column nb) (column_x nb) (column_y gm nb);
          rt_set_timeout gm.xargs "" (rt_current_time gm.xargs + pERIOD)
            (woops gm);
          True
        }
        else False
    | _ -> False ]
  in
  gm.state := NormalMove;
  let _ =
    vect_exists (test_push_on gm.towers) gm.buffers ||
    vect_exists (test_push_on gm.towers) gm.columns ||
    vect_exists (test_push_on gm.columns) gm.buffers
  in
  ()
}
and woops gm _ =
  match gm.state with
  [ AutoMove wid dest dx dy ->
      let card = get_card wid in
      let xw = widget_x wid
      and yw = widget_y wid in
      let d = dist dx dy xw yw in
      let nxw = xw + dELTA * (dx - xw) / d
      and nyw = yw + dELTA * (dy - yw) / d in
      let nd = dist dx dy nxw nyw in
      if nd < dELTA / 2 || nd >= d then do {
        gm.undo := [(wid, card.position) :: gm.undo];
        gm.redo := [];
        move_to dest wid;
        auto_move gm
      }
      else do {
        rt_set_timeout gm.xargs "" (rt_current_time gm.xargs + pERIOD)
          (woops gm);
        rt_move_widget wid nxw nyw
      }
  | _ -> failwith "woops" ]
;

value reset_game gm = do {
  modify_vect (fun _ -> []) gm.towers;
  modify_vect (fun _ -> []) gm.buffers;
  modify_vect (fun _ -> []) gm.columns;
  gm.state := NormalMove;
  rt_cancel_timeout gm.xargs "";
  gm.undo := [];
  gm.redo := [];
  iter (fun wid -> (get_card wid).position := Nowhere) gm.wid_list
};

value new_game gm = do {
  reset_game gm;
  let _ =
    gloop
      (fun wl -> do {
         let len = List.length wl in
         let r = random len in
         let (b0, e) = chop_list r wl in
         let wid = hd e in
         if len <= 2 then move_to (Buff len) wid
         else move_to (Column ((52 - len) / 5)) wid;
         b0 @ tl e
       })
      null gm.wid_list
  in
  auto_move gm;
  ()
}
and save_game gm = do {
  print_string "tours:";
  print_newline ();
  let _ =
    it_vect
      (fun sep ->
         fun
         [ [wid :: _] -> do {
             let card = get_card wid in
             print_string sep;
             print_string (val_txt card.val0);
             print_string (suit_txt card.suit);
             " "
           }
         | _ -> sep ])
      "" gm.towers
  in
  print_string ";";
  print_newline ();
  print_string "buffers:";
  print_newline ();
  let _ =
    it_vect
      (fun sep ->
         fun
         [ [wid :: _] -> do {
             let card = get_card wid in
             print_string sep;
             print_string (val_txt card.val0);
             print_string (suit_txt card.suit);
             " "
           }
         | _ -> sep ])
      "" gm.buffers
  in
  print_string ";";
  print_newline ();
  print_string "colonnes:";
  print_newline ();
  let _ =
    it_vect
      (fun sep widl -> do {
         print_string sep;
         let _ =
           List.fold_left
             (fun sep wid -> do {
                let card = get_card wid in
                print_string sep;
                print_string (val_txt card.val0);
                print_string (suit_txt card.suit);
                " "
              })
             "" widl
         in
         ",\n"
       })
      "" gm.columns
  in
  print_string ";";
  print_newline ()
}
and find_game gm =
  let is_space c = c = " " || c = "\n" || c = "\013" || c = ""
  and is_sep c = c = "," || c = ";" || c = ":" in
  let rec first_no_space c =
    if is_space c then first_no_space (String.make 1 (input_char stdin))
    else c
  in
  let next_tok c =
    let rec read_tok c =
      let nc = String.make 1 (input_char stdin) in
      if is_sep nc || is_space nc then (c, nc)
      else
        let (t, nc) = read_tok nc in
        (c ^ t, nc)
    in
    let c = first_no_space c in
    if is_sep c then (c, "") else read_tok c
  and wid_of_card (v, s) = item gm.wid_list (suit_item s * 13 + val_item v) in
  let rec read_cards (t, c) =
    try
      let v =
        match String.get t 0 with
        [ 'A' -> C'Ace
        | 'R' -> C'King
        | 'D' -> C'Queen
        | 'V' -> C'Jack
        | 'X' | '0' -> C'Plain 10
        | s -> C'Plain (Char.code s) ]
      and s =
        match String.get t 1 with
        [ 'P' -> C'Spades
        | 'C' -> C'Hearts
        | 'K' -> C'Diamonds
        | 'T' -> C'Clubs
        | _ -> failwith "read cards" ]
      in
      [(v, s) :: read_cards (next_tok c)]
    with _ -> []
  in
  let rec read_cols (t, c) n =
    let col = read_cards (t, c) in
    if n = 1 then [col] else [col :: read_cols (next_tok c) (n - 1)]
  in
  let skip_header txt =
    let (t, c) = next_tok "" in
    let (t, c) = if t = txt then next_tok c else (t, c) in
    if t = ":" then next_tok c else (t, c)
  in
  try do {
    let towers = read_cards (skip_header "tours")
    and buffers = read_cards (skip_header "buffers")
    and columns = read_cols (skip_header "colonnes") 10 in
    reset_game gm;
    do_list_i
      (fun i col ->
         iter
           (fun c ->
              let wid = wid_of_card c in
              move_to (Column i) wid)
           (List.rev col))
      0 columns;
    do_list_i
      (fun i c ->
         let wid = wid_of_card c in
         move_to (Buff i) wid)
      0 buffers;
    do_list_i
      (fun i c ->
         let wid = wid_of_card c in
         move_to (Tower i) wid)
      0 towers;
    ()
  }
  with _ -> message "incorrect"
;

exception Bad_position;
value motion (wid, x, y) =
  let gm = get_game wid in
  if gm.state = NormalMove then
    match gm.mwid with
    [ [mwid :: _] -> do {
        let nx = widget_x gm.mover + x - gm.xg
        and ny = widget_y gm.mover + y - gm.yg
        and w = widget_width gm.mover
        and h = widget_height gm.mover in
        let new_x = max 0 (min (widget_width wid - w) nx)
        and new_y = max 0 (min (widget_height wid - h) ny) in
        rt_move_widget gm.mover new_x new_y;
        gm.xg := x + new_x - nx;
        gm.yg := y + new_y - ny;
        ()
      }
    | [] -> () ]
  else ()
and enter_wind wid =
  let _ = random 1 in
  let gm = (get_card wid).gm in
  if not gm.bp then do { gm.mwid := [wid]; () } else ()
and leave_wind wid =
  let gm = (get_card wid).gm in
  if not gm.bp then do { gm.mwid := []; () } else ()
and bpressed (wid, b0, x, y) =
  let gm = get_game wid in
  let tagada twid =
    tagada_rec where rec tagada_rec =
      fun
      [ [wid :: widl] as widls ->
          if eq (wid, twid) then widls else tagada_rec widl
      | _ -> failwith "tagada" ]
  in
  if gm.state = NormalMove then do {
    gm.bp := True;
    match gm.mwid with
    [ [twid :: _] ->
        let card = get_card twid in
        if match card.position with
           [ Tower nb -> False
           | Buff nb -> do {
               gm.mwid := tagada twid (List.rev gm.buffers.(nb));
               True
             }
           | Column nb -> do {
               gm.mwid := tagada twid (List.rev gm.columns.(nb));
               True
             }
           | _ -> False ]
        then do {
          rt_move_resize_widget gm.mover (widget_x twid) (widget_y twid) wID
            (hEI + (List.length gm.mwid - 1) * rC);
          rt_map_widget gm.mover;
          gm.xg := x;
          gm.yg := y;
          iter (fun wid -> rt_unmap_widget wid) gm.mwid
        }
        else do { gm.mwid := []; () }
    | [] -> () ];
    ()
  }
  else ()
and breleased pwid =
  let gm = get_game pwid in
  if gm.state = NormalMove then do {
    gm.bp := False;
    match gm.mwid with
    [ [mwid :: widl] as widls -> do {
        let card = get_card mwid in
        try do {
          let nb = (gm.xg - leftB + iC / 2) / (oW + iC) in
          if nb < 0 || nb > 9 then raise Bad_position else ();
          if gm.yg >= upperB + oH + sC / 2 then do {
            do_list_i
              (fun i swid ->
                 let scard = get_card swid in
                 if scard.suit <> card.suit ||
                    val_item scard.val0 <> val_item card.val0 - i
                 then
                   raise Bad_position
                 else ())
              1 widl;
            let len = List.length widl in
            if len >= 1 && len > nb_free_buffers gm then raise Bad_position
            else ();
            match gm.columns.(nb) with
            [ [wid :: _] ->
                let scard = get_card wid in
                if card.suit <> scard.suit ||
                   val_item card.val0 <> val_item scard.val0 - 1
                then
                  raise Bad_position
                else ()
            | _ -> if card.val0 <> C'King then raise Bad_position else () ];
            iter
              (fun wid ->
                 let card = get_card wid in
                 gm.undo := [(wid, card.position) :: gm.undo])
              (List.rev widl);
            gm.undo := [(mwid, card.position) :: gm.undo];
            gm.redo := [];
            move_to (Column nb) mwid;
            do_list_i
              (fun i wid -> do {
                 gm.undo := [(wid, nth_free_buff gm i) :: gm.undo];
                 move_to (Column nb) wid
               })
              0 widl
          }
          else if nb >= 3 && nb <= 6 then do {
            let len = List.length widl in
            if len >= nb_free_buffers gm then raise Bad_position else ();
            let nb = nb - 3 in
            do_list_i
              (fun i wid -> do {
                 gm.undo := [(wid, card.position) :: gm.undo];
                 gm.redo := [];
                 move_to
                   (if i = 0 && gm.buffers.(nb) = [] then Buff nb
                    else nth_free_buff gm 0)
                   wid
               })
              0 (List.rev widls)
          }
          else if nb <= 1 || nb >= 8 then do {
            if widl <> [] then raise Bad_position else ();
            let nb = if nb <= 1 then nb else nb - 6 in
            match gm.towers.(nb) with
            [ [wid :: _] ->
                let scard = get_card wid in
                if card.suit <> scard.suit ||
                   val_item card.val0 <> val_item scard.val0 + 1
                then
                  raise Bad_position
                else ()
            | _ -> if card.val0 <> C'Ace then raise Bad_position else () ];
            gm.undo := [(mwid, card.position) :: gm.undo];
            gm.redo := [];
            move_to (Tower nb) mwid
          }
          else raise Bad_position
        }
        with
        [ Bad_position -> () ];
        rt_unmap_widget gm.mover;
        iter (fun wid -> rt_map_widget wid) widls;
        auto_move gm
      }
    | [] -> () ];
    gm.mwid := [];
    ()
  }
  else ()
;

value expose_tower (wid, _, _, _, _) = do {
  let gm = get_game wid in
  if gm.state = NotStarted then new_game gm else ();
  let draw = WidgetDr wid in
  rt_draw_line draw (0, 0) (oW - 2, oH - 2);
  rt_draw_line draw (0, oH - 2) (oW - 2, 0)
}
and expose_buffer (wid, _, _, _, _) = do {
  let draw = WidgetDr wid in
  rt_draw_line draw (0, 0) (oW - 2, oH - 2);
  rt_draw_line draw (0, oH - 2) (oW - 2, 0)
};

value keyp xargs (wid, s) =
  let gm = get_game wid in
  match s with
  [ "q" -> rt_stop_main_loop xargs
  | "u" ->
      if gm.state = NormalMove then do {
        match gm.undo with
        [ [(wid, pos) :: m] -> do {
            let card = get_card wid in
            gm.redo := [(wid, card.position) :: gm.redo];
            move_to pos wid;
            gm.undo := m;
            ()
          }
        | _ -> () ];
        ()
      }
      else ()
  | "r" ->
      if gm.state = NormalMove then do {
        match gm.redo with
        [ [(wid, pos) :: m] -> do {
            let card = get_card wid in
            gm.undo := [(wid, card.position) :: gm.undo];
            move_to pos wid;
            gm.redo := m;
            ()
          }
        | _ -> () ];
        ()
      }
      else ()
  | "f" -> find_game gm
  | "s" -> save_game gm
  | "n" -> new_game gm
  | _ -> () ]
;

value create_tower =
  let pl = [SelExposure] in
  fun col (pwid, nb) -> do {
    let x = tower_x nb
    and y = tower_y nb in
    let wid =
      rt_create_subwidget pwid x y
        (raw_desc [BackgroundAtt (ColorPn col)] (oW - 2, oH - 2, 1, pl)
           (fun wid ->
              fun
              [ RawEvExpose x y width height ->
                  expose_tower (wid, x, y, width, height)
              | _ -> () ]))
    in
    rt_map_widget wid;
    wid
  }
and create_buff =
  let pl = [SelExposure] in
  fun col (pwid, nb) -> do {
    let x = buff_x nb
    and y = buff_y nb in
    let wid =
      rt_create_subwidget pwid x y
        (raw_desc [BackgroundAtt (ColorPn col)] (oW - 2, oH - 2, 1, pl)
           (fun wid ->
              fun
              [ RawEvExpose x y width height ->
                  expose_buffer (wid, x, y, width, height)
              | _ -> () ]))
    in
    rt_map_widget wid;
    wid
  }
and create_card =
  let pl = [SelEnterWindow; SelLeaveWindow] in
  fun xd pwid data -> do {
    let pixmap = rt_create_pixmap xd wID hEI in
    let image = rt_create_image xd data wID hEI 1 in
    rt_put_image (PixmapDr pixmap) image (0, 0, wID, hEI) (0, 0);
    let wid =
      rt_create_subwidget pwid (-2 * oW) (-2 * oH)
        (raw_desc [BackgroundAtt (PixmapPn pixmap)] (wID, hEI, bD, pl)
           (fun wid ->
              fun
              [ RawEvEnterNotify _ _ _ _ -> enter_wind wid
              | RawEvLeaveNotify -> leave_wind wid
              | _ -> () ]))
    in
    rt_map_widget wid;
    wid
  }
;

value gen_sht bw dname = do {
  init_random (ftime ()).time;
  let xd = rt_initialize dname in
  try do {
    gc_alarm True;
    let bg =
      if is_colored xd && not bw then
        ColorPn (rt_create_color xd (153, 256, 153))
      else do {
        let pixm = rt_create_pixmap xd 8 8 in
        let image =
          rt_create_image xd (implode_ascii [1; 8; 64; 4; 128; 16; 2; 32]) 8 8
            1
        in
        rt_put_image (PixmapDr pixm) image (0, 0, 8, 8) (0, 0);
        PixmapPn pixm
      }
    in
    let xargs = rt_args [xd] in
    let main_wid =
      rt_create_widget xd "Seahaven towers" "sht" AutoPosition
        (Some (fun _ -> rt_stop_main_loop xargs))
        (raw_desc [BackgroundAtt bg]
           (gW, gH, 0,
            [SelKeyPress; SelButtonPress; SelButtonRelease; SelButtonMotion])
           (fun wid ->
              fun
              [ RawEvKeyPress k -> keyp xargs (wid, string_of_keysym k)
              | RawEvButtonPress x y _ _ {item = b0} ->
                  bpressed (wid, b0, x, y)
              | RawEvButtonRelease _ _ _ _ _ -> breleased wid
              | RawEvMotionNotify x y -> motion (wid, x, y)
              | _ -> () ]))
    in
    let mover =
      rt_create_subwidget main_wid 0 0
        (raw_desc [BackgroundAtt NonePn] (0, 0, 0, []) (fun _ _ -> ()))
    in
    let black_col = rt_black_color xd in
    let red_col =
      if is_colored xd && not bw then rt_create_color xd (190, 38, 0)
      else black_col
    in
    let wid_list =
      prim_rec
        (fun widl i -> do {
           if i = 26 then rt_select_color red_col else ();
           [create_card xd main_wid data.(i - 1) :: widl]
         })
        [] 52
    in
    rt_select_color black_col;
    let gm =
      {xargs = xargs; towers = Array.make 4 []; buffers = Array.make 4 [];
       columns = Array.make 10 []; wid_list = wid_list; mover = mover;
       state = NotStarted; undo = []; redo = []; bp = False; mwid = [];
       xg = 0; yg = 0}
    in
    rt_set_user_info main_wid (game gm);
    do_list_i
      (fun i wid ->
         let suit = item_suit (i / 13)
         and val0 = item_val (i mod 13) in
         rt_set_user_info wid
           (card {val0 = val0; suit = suit; position = Nowhere; gm = gm}))
      0 gm.wid_list;
    let create_tower =
      create_tower
        (if is_colored xd && not bw then rt_create_color xd (0, 255, 255)
         else rt_white_color xd)
    in
    iterate
      (fun x -> do {
         let wid = create_tower (main_wid, x) in
         rt_set_user_info wid (game gm);
         x + 1
       })
      4 0;
    let create_buff =
      create_buff
        (if is_colored xd && not bw then rt_create_color xd (255, 143, 76)
         else rt_white_color xd)
    in
    iterate
      (fun x ->
         let _ = create_buff (main_wid, x) in
         x + 1)
      4 0;
    rt_map_widget main_wid;
    rt_main_loop gm.xargs
  }
  with x -> do { gc_alarm False; rt_end xd; raise x };
  gc_alarm False;
  rt_end xd
};

try gen_sht bw.val dname.val with
[ Failure s -> do {
    print_string "failure: ";
    print_string s;
    print_newline ()
  }
| Invalid_argument s -> do {
    print_string "Invalid_argument: ";
    print_string s;
    print_newline ()
  }
| x -> do {
    print_string "Unknown exception in ssht";
    print_newline ();
    raise x
  } ];
