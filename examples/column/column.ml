(* $Id: column.ml,v 1.9 2008/07/21 09:50:13 deraugla Exp $
 *
 * xcolumn
 *)

open Rt;
open Sys;

value tmout = ref 0 and tmout_fun = ref (fun () -> ());
value rt_set_timeout xa tm = do {
  tmout.val := tm;
  Rt.rt_cancel_timeout xa "";
  Rt.rt_set_timeout xa "" tmout.val tmout_fun.val
}
and rt_set_timeout_fun xa tmf = do {
  tmout_fun.val := tmf;
  Rt.rt_cancel_timeout xa "";
  Rt.rt_set_timeout xa "" tmout.val tmout_fun.val
}
and rt_reset_timeout xa = Rt.rt_cancel_timeout xa "";

type option0 'a =
  [ None0
  | Some0 of 'a ]
;

value make_array len f = do {
  let v = Array.make len (Obj.magic 0) in
  for i = 0 to len - 1 do { v.(i) := f i };
  v
};

value mkw w =
  let s = Bytes.create (List.length w) in
  mkw_rec 0 w where rec mkw_rec i =
    fun
    [ [] -> s
    | [c :: w] -> do { Bytes.set s i c; mkw_rec (succ i) w } ]
and line_buff = Bytes.create 200;
  
value iterate f =
  iterate_f where rec iterate_f n x =
    if n > 0 then iterate_f (pred n) (f x) else ()
and break_string sep s =
  break_rec [] 0 where rec break_rec w i =
    if i = String.length s then if w = [] then [] else [mkw (List.rev w)]
    else
      let c = String.get s i in
      if c = String.get sep 0 then [mkw (List.rev w) :: break_rec [] (succ i)]
      else break_rec [c :: w] (succ i)
and input_line ic =
  String.sub line_buff 0
    (input_rec 0 where rec input_rec i =
       match input_char ic with
       [ '\n' -> i
       | c -> do { Bytes.set line_buff i c; input_rec (succ i) } ])
and nth =
  nth_rec where rec nth_rec p0 p1 =
    match (p0, p1) with
    [ ([x :: l], 1) -> x
    | ([x :: l], i) -> nth_rec l (pred i)
    | (_, _) -> failwith "nth" ]
and do_list_i f =
  do_list_f where rec do_list_f i =
    fun
    [ [] -> ()
    | [x :: l] -> do { f i x; do_list_f (succ i) l } ]
and output_line ch s = do {
  output ch s 0 (String.length s);
  output ch "\n" 0 1
};
value except_assq e =
  except_e where rec except_e =
    fun
    [ [((x, _) as y) :: l] -> if x == e then l else [y :: except_e l]
    | _ -> [] ]
and tee (f, g) x = (f x, g x)
and curry f x y = f (x, y)
and i x = x
and b f g x = f (g x)
and read ic len =
  let s = Bytes.create len in
  let _ = input ic s 0 len in
  s
and rtime t2 = (ftime ()).time - t2;
value line_buff = Bytes.create 200;
value read_line ic =
  let rec input_rec i0 =
    match input_char ic with
    [ '\n' -> i0
    | c -> do { Bytes.set line_buff i0 c; input_rec (succ i0) } ]
  in
  String.sub line_buff 0 (input_rec 0)
;
value seed = ref 7 and maxint = 536870911 * 2 + 1;
value random n = do {
  seed.val := 25173 * seed.val + 13849;
  (if seed.val < 0 then -seed.val else seed.val) / (maxint / n)
}
and init_random n = do { seed.val := n; () };
value implode_ascii l = do {
  let len = List.length l in
  let s = String.make len ' ' in
  iterate
    (fun (i0, l) -> do {
       Bytes.set s i0 (Char.chr (List.hd l));
       (i0 + 1, List.tl l)
     })
    len (0, l);
  s
};

(*let column_version = nth (words "$Revision: 1.9 $") 2;;*)

value pack1_desc attr args = pack_desc attr args (fun _ _ -> ());
value button1_desc attr txt = button_desc attr (txt, None);

value filler_desc = pack1_desc [FillerAtt; BorderAtt 0] (Vertical, []);

type t 'a = { hd : 'a; tl : mutable option0 (t 'a) };

type game_data =
  { xd : xdata;
    dname : string;
    pname : string;
    pint : int;
    patt : glop;
    game_type : mutable game_type;
    score : mutable int;
    time_beginning_of_game : mutable int;
    time_beginning_of_level : mutable int;
    falling_height : mutable int;
    state : mutable game_state;
    ask : mutable ask;
    level : mutable int;
    min_level : mutable int;
    runner_state : mutable runner_state;
    timeout : mutable option0 int;
    col : fall_col;
    board : array (array int);
    mark : array (array bool) }
and glop =
  [ C'Pattern of array pattern
  | C'Color of array color ]
and global_data =
  { xargs : xargs;
    init_time : int;
    gstate : mutable global_state;
    gwl : mutable list (game_data * drawable) }
and global_state =
  [ GSRunning
  | GSReadingDisplay
  | GSReadingName of xdata and string and int ]
and fall_col = { x : mutable int; y : mutable int; squ : array int }
and game_type =
  [ C'Normal
  | C'Tournament of t (int * array int) ]
and game_state = [ C'NotStarted | C'Running | C'Pausing | C'Ended ]
and runner_state = [ C'TouchedDown | C'Falling | C'Exploding ]
and ask = [ C'AskForAbort | C'AskForQuit | C'NoAsk ]
and score_tab = { player : mutable string; bscore : mutable int }
and my_patterns =
  [ C'WhiteP
  | C'BlackP
  | C'DiagonalP
  | C'SandP
  | C'ChBoardP
  | C'VerticalP
  | C'GrayP ]
;

value patterns =
  [| implode_ascii
       [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0; 0];
     implode_ascii
       [255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255;
        255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255;
        255; 255; 255; 255];
     implode_ascii
       [240; 240; 120; 120; 60; 60; 30; 30; 15; 15; 135; 135; 195; 195; 225;
        225; 240; 240; 120; 120; 60; 60; 30; 30; 15; 15; 135; 135; 195; 195;
        225; 225];
     implode_ascii
       [1; 1; 8; 8; 64; 64; 4; 4; 128; 128; 16; 16; 2; 2; 32; 32; 1; 1; 8; 8;
        64; 64; 4; 4; 128; 128; 16; 16; 2; 2; 32; 32];
     implode_ascii
       [255; 0; 255; 0; 255; 0; 255; 0; 255; 0; 255; 0; 255; 0; 255; 0; 0;
        255; 0; 255; 0; 255; 0; 255; 0; 255; 0; 255; 0; 255; 0; 255];
     implode_ascii
       [60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60;
        60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60; 60];
     implode_ascii
       [85; 85; 170; 170; 85; 85; 170; 170; 85; 85; 170; 170; 85; 85; 170;
        170; 85; 85; 170; 170; 85; 85; 170; 170; 85; 85; 170; 170; 85; 85;
        170; 170] |]
and colors =
  [| (255, 255, 255); (0, 0, 0); (203, 46, 201); (135, 206, 255);
     (255, 255, 135); (34, 139, 24); (255, 143, 52) |]
;

value select_pattern (gm, n) =
  let i0 =
    match n with
    [ C'WhiteP -> 0
    | C'BlackP -> 1
    | C'DiagonalP -> 2
    | C'SandP -> 3
    | C'ChBoardP -> 4
    | C'VerticalP -> 5
    | C'GrayP -> 6 ]
  in
  match gm.patt with
  [ C'Pattern patt -> rt_select_pattern patt.(i0) (0, 0)
  | C'Color col -> rt_select_color col.(i0) ]
;

value patt_init xd =
  let sZ = 16 in
  let patt =
    make_array (Array.length patterns)
      (fun i0 -> rt_create_pattern xd patterns.(i0) sZ sZ)
  in
  C'Pattern patt
;

value col_init xd =
  let patt =
    make_array (Array.length colors)
      (fun i0 ->
         let (r, g, b0) = colors.(i0) in
         rt_create_color xd (r, g, b0))
  in
  C'Color patt
;

value column_wizard = ref False and trace_games = ref True;

value sZ = 24
and w = 6
and h = 20
and upperB = 64
and leftB = 8
and lowerB = 8
and rightB = 150
and (speed_tab, nlevels) =
  tee (Array.get, Array.length)
    [| 1000; 800; 600; 500; 440; 400; 360; 320; 280; 240; 200; 180; 160; 140;
       120; 100; 80; 60; 40; 20 |]
and change_level =
  Array.get
    [| 10; 20; 30; 40; 50; 60; 70; 80; 90; 100; 110; 120; 130; 140; 150; 160;
       170; 180; 190; 200; 210; 220; 230; 240; 250; 260; 270; 280; 290; 300 |]
and sCORE_FILE = "./column.sc"
and (tab_colors, ncolors) =
  tee (Array.get, Array.length)
    [| C'BlackP; C'DiagonalP; C'SandP; C'ChBoardP; C'VerticalP; C'GrayP |]
;

value gW = leftB + w * sZ + rightB and gH = upperB + h * sZ + lowerB;

value clear_square gm drw x y = do {
  select_pattern (gm, C'WhiteP);
  rt_fill_rectangle drw (x * sZ + leftB, y * sZ + upperB, sZ, sZ)
}
and draw_square gm drw x y k = do {
  select_pattern (gm, tab_colors (k - 1));
  rt_fill_rectangle drw
    (x * sZ + leftB + 1, y * sZ + upperB + 1, sZ - 1, sZ - 1);
  select_pattern (gm, C'BlackP);
  rt_draw_rectangle drw (x * sZ + leftB, y * sZ + upperB, sZ - 1, sZ - 1)
}
and draw_score gm drw = do {
  select_pattern (gm, C'BlackP);
  rt_erase_draw_string drw (leftB + sZ / 3, 25)
    (if column_wizard.val then "score <" ^ string_of_int gm.score ^ ">   "
     else "score " ^ string_of_int gm.score ^ "   ")
}
and draw_level gm drw = do {
  select_pattern (gm, C'BlackP);
  rt_erase_draw_string drw (gW - rightB - sZ / 3 - 50, 25)
    ("level " ^
     (let lev = gm.level in
      (if lev < 10 then " " else "") ^ string_of_int lev))
}
and draw_state gm drw = do {
  select_pattern (gm, C'BlackP);
  rt_erase_draw_string drw (gW - rightB - sZ / 3 - 70, upperB - 15)
    (match gm.state with
     [ C'Pausing -> "    <pause>"
     | C'Ended -> "<game over>"
     | _ -> "           " ])
}
and draw_player_score gm1 gm drw = do {
  let s = "     " ^ string_of_int gm1.score in
  let s = String.sub s (String.length s - 6) 6 in
  select_pattern (gm, C'BlackP);
  rt_erase_draw_string drw (gW - 40, upperB + 20 + gm1.pint * 20) s
}
and display_player gm = do {
  print_string "*** player ";
  print_int gm.pint;
  print_string " (";
  print_string gm.pname;
  print_string "): "
}
and display_closing_display dname = do {
  print_string "*** closing display \"";
  print_string dname;
  print_string "\"";
  print_newline ()
};

value gen_timeout gd =
  match
    List.fold_left
      (fun v (gm, _) ->
         match v with
         [ None0 -> gm.timeout
         | Some0 tm1 ->
             match gm.timeout with
             [ None0 -> Some0 tm1
             | Some0 tm2 -> Some0 (min tm1 tm2) ] ])
      None0 gd.gwl
  with
  [ None0 -> rt_reset_timeout gd.xargs
  | Some0 wt -> rt_set_timeout gd.xargs wt ]
;

value set_gm_timeout gd gm wait_time = do {
  gm.timeout := Some0 (rt_current_time gd.xargs + wait_time);
  gen_timeout gd
}
and reset_gm_timeout gd gm = do { gm.timeout := None0; gen_timeout gd };

value draw_players gd gm drw = do {
  select_pattern (gm, C'WhiteP);
  rt_fill_rectangle drw (gW - rightB + 10, upperB, 0, 0);
  List.iter
    (fun (gm1, _) -> do {
       select_pattern (gm, C'BlackP);
       rt_erase_draw_string drw
         (gW - rightB + 15, upperB + 20 + gm1.pint * 20)
         (string_of_int gm1.pint ^ ": " ^ gm1.pname);
       draw_player_score gm1 gm drw
     })
    gd.gwl
};

value draw_column gm drw =
  let draw = draw_square gm drw gm.col.x in
  for i0 = 0 to Array.length gm.col.squ - 1 do {
    draw (gm.col.y + i0) gm.col.squ.(i0);
  }
and clear_column gm drw =
  let clear = clear_square gm drw gm.col.x in
  for i0 = 0 to Array.length gm.col.squ - 1 do { clear (gm.col.y + i0) }
and explose gm drw = do {
  let draw = draw_square gm drw
  and clear = clear_square gm drw in
  for x = 0 to Array.length gm.mark - 1 do {
    let c = gm.mark.(x) in
    let draw = draw x
    and clear = clear x in
    for y = 0 to Array.length c - 1 do {
      let m = c.(y) in
      if m then do {
        let c = gm.board.(x)
        and i0 = ref y in
        while i0.val > 0 && c.(i0.val - 1) <> 0 do {
          c.(i0.val) := c.(i0.val - 1);
          draw i0.val c.(i0.val);
          decr i0
        };
        c.(i0.val) := 0;
        clear i0.val
      }
      else ();
    };
  };
  gm.runner_state := C'TouchedDown
}
and explosive gm drw x y = do {
  gm.mark.(x).(y) := True;
  let a = sZ / 3 in
  select_pattern (gm, C'WhiteP);
  rt_fill_rectangle drw
    (x * sZ + leftB + a, y * sZ + upperB + a, sZ - 2 * a, sZ - 2 * a)
}
and incr_score gd gm drw = do {
  gm.score := gm.score + (9 + gm.falling_height) * succ gm.min_level;
  let t = rtime gd.init_time in
  if t - gm.time_beginning_of_level >= change_level gm.level then
    if gm.level < nlevels - 1 then do {
      gm.time_beginning_of_level := t;
      gm.level := gm.level + 1;
      set_gm_timeout gd gm (speed_tab gm.level);
      draw_level gm drw
    }
    else ()
  else ()
}
and rand_gen () =
  {hd =
     (random w,
      [| random ncolors + 1; random ncolors + 1; random ncolors + 1 |]);
   tl = None0}
;

value new_column gm drw = do {
  gm.min_level := gm.level;
  match gm.game_type with
  [ C'Normal -> do {
      gm.col.x := random w;
      for i0 = 0 to Array.length gm.col.squ - 1 do {
        gm.col.squ.(i0) := random ncolors + 1;
      }
    }
  | C'Tournament c -> do {
      let nc =
        match c.tl with
        [ Some0 nc -> nc
        | None0 -> do {
            let nc = rand_gen () in
            c.tl := Some0 nc;
            nc
          } ]
      in
      gm.game_type := C'Tournament nc;
      let (x, y) = c.hd in
      gm.col.x := x;
      for i0 = 0 to Array.length gm.col.squ - 1 do {
        gm.col.squ.(i0) := y.(i0);
      }
    } ];
  gm.col.y := 0;
  draw_column gm drw
}
and pause gd gm drw = do {
  gm.state := C'Pausing;
  let t = rtime gd.init_time in
  gm.time_beginning_of_game := gm.time_beginning_of_game - t;
  gm.time_beginning_of_level := gm.time_beginning_of_level - t;
  reset_gm_timeout gd gm;
  if not column_wizard.val then do {
    select_pattern (gm, C'GrayP);
    rt_fill_rectangle drw (leftB, upperB, w * sZ, h * sZ);
    if trace_games.val then do {
      display_player gm;
      print_string "pausing";
      print_newline ()
    }
    else ()
  }
  else ();
  draw_state gm drw
}
and restart gd gm drw = do {
  gm.state := C'Running;
  let t = rtime gd.init_time in
  gm.time_beginning_of_game := gm.time_beginning_of_game + t;
  gm.time_beginning_of_level := gm.time_beginning_of_level + t;
  set_gm_timeout gd gm (speed_tab gm.level);
  if not column_wizard.val then do {
    select_pattern (gm, C'WhiteP);
    rt_fill_rectangle drw (leftB, upperB, w * sZ, h * sZ);
    let draw = draw_square gm drw in
    for x = 0 to Array.length gm.board - 1 do {
      let c = gm.board.(x) in
      let draw = draw x in
      for y = 0 to Array.length c - 1 do {
        let e = c.(y) in
        if e <> 0 then draw y e else ();
      };
    };
    draw_column gm drw;
    if trace_games.val then do {
      display_player gm;
      print_string "restarting";
      print_newline ()
    }
    else ()
  }
  else ();
  draw_state gm drw
};

value start_new_game gd gm drw = do {
  Array.iter (fun x -> for i0 = 0 to Array.length x - 1 do { x.(i0) := 0 })
    gm.board;
  select_pattern (gm, C'WhiteP);
  rt_fill_rectangle drw (leftB, upperB, sZ * w, sZ * h);
  gm.level := 0;
  gm.min_level := 0;
  gm.score := 0;
  gm.falling_height := 0;
  gm.time_beginning_of_game := rtime gd.init_time;
  gm.time_beginning_of_level := gm.time_beginning_of_game;
  gm.state := C'Running;
  draw_score gm drw;
  draw_state gm drw;
  draw_level gm drw;
  new_column gm drw;
  set_gm_timeout gd gm (speed_tab 0)
}
and mark_explosions gd gm drw = do {
  Array.iter
    (fun x -> for i0 = 0 to Array.length x - 1 do { x.(i0) := False })
    gm.mark;
  let explosive = explosive gm drw
  and incr_score = incr_score gd gm in
  for x = 0 to Array.length gm.board - 1 do {
    let c = gm.board.(x) in
    for y = 0 to Array.length c - 1 do {
      let e = c.(y) in
      if e <> 0 then do {
        if x + 2 < w && e = gm.board.(x + 1).(y) && e = gm.board.(x + 2).(y)
        then do {
          iterate (fun i0 -> do { explosive (x + i0) y; i0 + 1 }) 3 0;
          incr_score drw;
          gm.runner_state := C'Exploding;
          ()
        }
        else ();
        if y + 2 < h && e = c.(y + 1) && e = c.(y + 2) then do {
          iterate (fun i0 -> do { explosive x (y + i0); i0 + 1 }) 3 0;
          incr_score drw;
          gm.runner_state := C'Exploding;
          ()
        }
        else ();
        if x + 2 < w && y + 2 < h && e = gm.board.(x + 1).(y + 1) &&
           e = gm.board.(x + 2).(y + 2)
        then do {
          iterate (fun i0 -> do { explosive (x + i0) (y + i0); i0 + 1 }) 3 0;
          incr_score drw;
          gm.runner_state := C'Exploding;
          ()
        }
        else ();
        if x - 2 >= 0 && y + 2 < h && e = gm.board.(x - 1).(y + 1) &&
           e = gm.board.(x - 2).(y + 2)
        then do {
          iterate (fun i0 -> do { explosive (x - i0) (y + i0); i0 + 1 }) 3 0;
          incr_score drw;
          gm.runner_state := C'Exploding;
          ()
        }
        else ()
      }
      else ();
    };
  };
  if gm.runner_state = C'Exploding then do {
    draw_score gm drw;
    let draw = draw_player_score gm in
    List.iter (fun (gm1, drw1) -> draw gm1 drw1) gd.gwl;
    if column_wizard.val then pause gd gm drw else set_gm_timeout gd gm 300
  }
  else if gm.level < 10 then set_gm_timeout gd gm 1000
  else if gm.level < 20 then set_gm_timeout gd gm 500
  else if gm.level < 25 then set_gm_timeout gd gm 250
  else set_gm_timeout gd gm 125
};

value record_if_high_score gm = do {
  let rec score_list ch =
    try
      let il = break_string "@" (input_line ch) in
      let scv = int_of_string (nth il 2) in
      [{player = List.hd il; bscore = scv} :: score_list ch]
    with _ -> []
  and add_score =
    fun
    [ [sh :: st] as ls ->
        if gm.score > sh.bscore then
          [{player = gm.pname; bscore = -gm.score} :: ls]
        else [sh :: add_score st]
    | _ -> [{player = gm.pname; bscore = -gm.score}] ]
  in
  let ls =
    add_score
      (try do {
         let ch = open_in sCORE_FILE in
         let ls = score_list ch in
         close_in ch;
         ls
       }
       with _ -> [])
  in
  let twid = widget_named gm.xd "high scores" in
  let print_string s = term_send twid s in
  let print_int = b print_string string_of_int
  and print_newline () = print_string "\n" in
  term_send twid "\027[2J\027[?35h\027[20h";
  print_string "*** best scores:\n\n";
  do_list_i
    (fun i0 b0 -> do {
       if b0.bscore < 0 then do {
         b0.bscore := -b0.bscore;
         print_string " -> "
       }
       else print_string "    ";
       print_int i0;
       print_string "\t";
       print_string b0.player;
       print_string "\t";
       print_int b0.bscore;
       print_newline ()
     })
    1 ls;
  try do {
    let ch = open_out sCORE_FILE in
    try
      do_list_i
        (fun i0 b0 ->
           if i0 <= 19 then
             output_line ch (b0.player ^ "@" ^ string_of_int b0.bscore ^ "@")
           else ())
        1 ls
    with _ -> ();
    close_out ch
  }
  with _ -> do { print_string "Can't write score file"; print_newline () };
  rt_raise_widget (widget_named gm.xd "scores")
};

value touch_down gd gm drw = do {
  gm.runner_state := C'TouchedDown;
  if gm.col.y < 1 then do {
    gm.state := C'Ended;
    reset_gm_timeout gd gm;
    draw_state gm drw;
    if trace_games.val then do {
      display_player gm;
      print_string "game over, score: ";
      print_int gm.score;
      print_newline ()
    }
    else ();
    if not column_wizard.val then record_if_high_score gm else ()
  }
  else do {
    let c = gm.board.(gm.col.x) in
    for i0 = 0 to Array.length gm.col.squ - 1 do {
      let e = gm.col.squ.(i0) in
      if gm.col.y + i0 >= 0 then do { c.(gm.col.y + i0) := e; () } else ();
    };
    mark_explosions gd gm drw;
    new_column gm drw
  }
};

value quit_game gd gm = do {
  if trace_games.val then do {
    display_player gm;
    print_string "quit";
    if gm.state <> C'Ended then do {
      print_string ", score: ";
      print_int gm.score
    }
    else ();
    print_newline ()
  }
  else ();
  if gm.state <> C'Ended && not column_wizard.val then record_if_high_score gm
  else ();
  gd.gwl := except_assq gm gd.gwl;
  List.iter (fun (gm, drw) -> draw_players gd gm drw) gd.gwl;
  if trace_games.val then display_closing_display gm.dname else ();
  rt_end gm.xd;
  if gd.gwl = [] then rt_stop_main_loop gd.xargs
  else rt_unselect_xdata gd.xargs gm.xd
};

value woops_fun gd _ =
  List.iter
    (fun (gm, drw) ->
       match gm.timeout with
       [ None0 ->
           ()
       | Some0 timeout ->
           if rt_current_time gd.xargs >= timeout then do {
             if gm.state <> C'Running then failwith "erreur dans woops_fun"
             else ();
             if gm.runner_state = C'Exploding then do {
               explose gm drw;
               mark_explosions gd gm drw
             }
             else do {
               if gm.runner_state = C'TouchedDown then do {
                 gm.runner_state := C'Falling;
                 ()
               }
               else ();
               set_gm_timeout gd gm (speed_tab gm.level);
               if gm.col.y + 3 = h || gm.board.(gm.col.x).(gm.col.y + 3) <> 0
               then do {
                 gm.falling_height := 0;
                 touch_down gd gm drw
               }
               else do {
                 clear_square gm drw gm.col.x gm.col.y;
                 gm.col.y := gm.col.y + 1;
                 draw_column gm drw
               }
             }
           }
           else () ])
    gd.gwl
;

value expose gd gm wid = do {
  let drw = WidgetDr wid in
  select_pattern (gm, C'BlackP);
  rt_draw_rectangle drw (leftB - 1, upperB - 1, w * sZ + 1, h * sZ + 1);
  draw_score gm drw;
  draw_state gm drw;
  draw_level gm drw;
  draw_players gd gm drw;
  if gm.state = C'NotStarted then do {
    gm.state := C'Running;
    new_column gm drw;
    set_gm_timeout gd gm (speed_tab 0);
    if trace_games.val then do {
      display_player gm;
      print_string "starting";
      print_newline ()
    }
    else ()
  }
  else if gm.state = C'Pausing && not column_wizard.val then do {
    select_pattern (gm, C'GrayP);
    rt_fill_rectangle drw (leftB, upperB, w * sZ, h * sZ)
  }
  else do {
    let draw = draw_square gm drw in
    for x = 0 to Array.length gm.board - 1 do {
      let c = gm.board.(x) in
      let draw = draw x in
      for y = 0 to Array.length c - 1 do {
        let e = c.(y) in
        if e <> 0 then draw y e else ();
      };
    };
    draw_column gm drw
  }
};

value wid_q gd gm wid =
  let wid_named = widget_named gm.xd in
  try wid_named "question" with _ ->
    let drw = WidgetDr wid in
    let answer b0 = do {
      rt_unmap_widget (wid_named "question");
      if b0 then
        match gm.ask with
        [ C'AskForQuit -> quit_game gd gm
        | C'AskForAbort -> do {
            gm.state := C'Ended;
            draw_state gm drw;
            if trace_games.val then do {
              display_player gm;
              print_string "game aborted, score: ";
              print_int gm.score;
              print_newline ()
            }
            else ();
            if not column_wizard.val then record_if_high_score gm else ()
          }
        | _ -> () ]
      else restart gd gm drw;
      gm.ask := C'NoAsk;
      ()
    }
    in
    rt_create_subwidget wid 10 10
      (pack1_desc [NameAtt "question"]
         (Vertical,
          [title_desc [] "Abort game ?" (fun _ _ -> ());
           pack1_desc []
             (Horizontal,
              [button1_desc [] "yes"
                 (fun p0 p1 ->
                    match (p0, p1) with
                    [ (_, ButtonEvRelease _ _ _) -> answer True
                    | (_, _) -> () ]);
               button1_desc [] "cancel"
                 (fun p0 p1 ->
                    match (p0, p1) with
                    [ (_, ButtonEvRelease _ _ _) -> answer False
                    | (_, _) -> () ])])]))
;

value keyp gd gm wid k =
  if gm.ask = C'NoAsk then
    let drw = WidgetDr wid in
    match k with
    [ "q" ->
        if gm.state = C'Ended then quit_game gd gm
        else do {
          gm.ask := C'AskForQuit;
          pause gd gm drw;
          let wid_q = wid_q gd gm wid in
          let w0 = widget_width wid
          and h0 = widget_height wid
          and sw = widget_width wid_q
          and sh = widget_height wid_q in
          rt_move_widget wid_q ((w0 - sw) / 2) ((h0 - sh) / 2);
          rt_map_widget wid_q
        }
    | "p" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if gm.state = C'Running then pause gd gm drw
        else ()
    | "a" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if gm.state = C'Running then do {
          gm.ask := C'AskForAbort;
          pause gd gm drw;
          let wid_q = wid_q gd gm wid in
          let w0 = widget_width wid
          and h0 = widget_height wid
          and sw = widget_width wid_q
          and sh = widget_height wid_q in
          rt_move_widget wid_q ((w0 - sw) / 2) ((h0 - sh) / 2);
          rt_map_widget wid_q
        }
        else ()
    | "n" | "w" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if gm.state = C'Ended && (k = "n" || column_wizard.val) then do {
          if k = "n" then do { column_wizard.val := False; () } else ();
          gm.game_type := C'Normal;
          start_new_game gd gm drw;
          draw_players gd gm drw;
          if trace_games.val then do {
            display_player gm;
            print_string "starting a new game";
            print_newline ()
          }
          else ()
        }
        else ()
    | "u" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if gm.level < nlevels - 1 then do {
          gm.time_beginning_of_level := rtime gd.init_time;
          gm.level := gm.level + 1;
          set_gm_timeout gd gm (speed_tab gm.level);
          draw_level gm drw
        }
        else ()
    | "d" ->
        if gm.state = C'Pausing then restart gd gm drw
        else
          let pred_level = gm.level - 1 in
          if pred_level >= 0 then do {
            let t = ref (rtime gd.init_time - gm.time_beginning_of_game)
            and i0 = ref 0 in
            while t.val > 0 do {
              t.val := t.val - change_level i0.val;
              incr i0
            };
            gm.level := i0.val;
            gm.min_level := i0.val;
            set_gm_timeout gd gm (speed_tab gm.level);
            draw_level gm drw
          }
          else ()
    | "Left" | "R10" | "j" | "K4" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if
          gm.state = C'Running && gm.col.x > 0 && gm.col.y + 2 >= 0 &&
          gm.board.(gm.col.x - 1).(gm.col.y + 2) = 0
        then do {
          clear_column gm drw;
          gm.col.x := gm.col.x - 1;
          draw_column gm drw
        }
        else ()
    | "R11" | "K5" | "k" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if gm.state = C'Running then do {
          let f = gm.col.squ.(2) in
          gm.col.squ.(2) := gm.col.squ.(1);
          gm.col.squ.(1) := gm.col.squ.(0);
          gm.col.squ.(0) := f;
          draw_column gm drw
        }
        else ()
    | "Right" | "R12" | "l" | "K6" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if
          gm.state = C'Running && gm.col.x < w - 1 && gm.col.y + 2 >= 0 &&
          gm.board.(gm.col.x + 1).(gm.col.y + 2) = 0
        then do {
          clear_column gm drw;
          gm.col.x := gm.col.x + 1;
          draw_column gm drw
        }
        else ()
    | " " | "Ins" | "K0" ->
        if gm.state = C'Pausing then restart gd gm drw
        else if gm.state = C'Running && gm.col.y + 2 >= 0 then do {
          clear_column gm drw;
          gm.falling_height := 0;
          while
            gm.col.y + 3 < h && gm.board.(gm.col.x).(gm.col.y + 3) = 0
          do {
            gm.col.y := gm.col.y + 1;
            gm.falling_height := gm.falling_height + 1
          };
          draw_column gm drw;
          touch_down gd gm drw
        }
        else ()
    | _ -> if gm.state = C'Pausing then restart gd gm drw else () ]
  else ()
;

value bW = ref False;

value one_column (dname, pint, gd) = do {
  let xd = rt_initialize dname in
  let dname = rt_display_name dname in
  print_string "name of player ";
  print_int pint;
  print_string " on ";
  print_string dname;
  print_string " (";
  let default_name = try getenv "USER" with _ -> dname in
  print_string default_name;
  print_string "): ";
  flush stdout;
  gd.gstate := GSReadingName xd dname pint
};

value one_column_continued (dname, pint, gd, xd) pname =
  try do {
    let dname = rt_display_name dname in
    let default_name = try getenv "USER" with _ -> dname in
    let pname = if pname = "" then default_name else pname in
    let gm =
      {xd = xd; dname = dname; pname = pname; pint = pint;
       patt =
         if is_colored xd && not bW.val then col_init xd else patt_init xd;
       game_type = C'Normal; score = 0; falling_height = 0;
       state = C'NotStarted; ask = C'NoAsk; level = 0; min_level = 0;
       time_beginning_of_game = 0; time_beginning_of_level = 0;
       runner_state = C'TouchedDown; timeout = None0;
       col = {x = 0; y = 0; squ = Array.make 3 0};
       board = Array.make w [| |]; mark = Array.make w [| |]}
    in
    for i0 = 0 to Array.length gm.board - 1 do {
      gm.board.(i0) := Array.make h 0;
    };
    for i0 = 0 to Array.length gm.mark - 1 do {
      gm.mark.(i0) := Array.make h False;
    };
    let wdesc =
      pack1_desc []
        (InDepth,
         [raw_desc [NameAtt "raw"] (gW, gH, 0, [SelExposure; SelKeyPress])
            (fun wid ->
               fun
               [ RawEvExpose _ _ _ _ -> expose gd gm wid
               | RawEvKeyPress ksm -> keyp gd gm wid (string_of_keysym ksm)
               | _ -> () ]);
          pack1_desc [NameAtt "scores"]
            (Vertical,
             [title_desc [] "high scores" (fun _ _ -> ()); filler_desc;
              term_desc [NameAtt "high scores"] (25, 1, 0) (fun _ _ -> ());
              filler_desc;
              button1_desc [] "ok"
                (fun p0 p1 ->
                   match (p0, p1) with
                   [ (_, ButtonEvRelease _ _ _) ->
                       rt_raise_widget (widget_named xd "raw")
                   | (_, _) -> () ])])])
    in
    let (width, height) = widget_size xd wdesc in
    let wid =
      rt_create_widget xd ("player " ^ string_of_int pint) "column"
        (UserPosition ((screen_width xd - width) / 2)
           ((screen_height xd - height) / 2))
        (Some (fun wid -> keyp gd gm wid "q")) wdesc
    in
    rt_map_widget wid;
    rt_select_xdata gd.xargs xd;
    gd.gwl := [(gm, WidgetDr (widget_named xd "raw")) :: gd.gwl];
    ()
  }
  with x -> do {
    print_string "*** error while creating game on display \"";
    print_string dname;
    print_string "\"";
    print_newline ();
    display_closing_display dname;
    rt_end xd;
    raise x
  }
;

value stdin_act gd c =
  match c with
  [ "c" -> do {
      print_string "*** available commands in game:";
      print_newline ();
      print_string "u: one level up";
      print_newline ();
      print_string "d: one level down";
      print_newline ();
      print_string "q: quit game";
      print_newline ();
      print_string "p: pause (any key to restart)";
      print_newline ();
      print_string "a: abort game";
      print_newline ();
      print_string "n: new game (if game over)";
      print_newline ();
      print_string "j: column left";
      print_newline ();
      print_string "k: column rotate";
      print_newline ();
      print_string "l; column right";
      print_newline ();
      print_string "<space>: column touch down";
      print_newline ()
    }
  | "n" -> do {
      print_string "display name: ";
      flush stdout;
      gd.gstate := GSReadingDisplay
    }
  | "q" -> rt_stop_main_loop gd.xargs
  | "s" ->
      List.iter
        (fun (gm, _) -> do {
           display_player gm;
           print_string
             (match gm.state with
              [ C'NotStarted -> "not started"
              | C'Running -> "running"
              | C'Pausing -> "pause"
              | C'Ended -> "game over" ]);
           print_string ", score: ";
           print_int gm.score;
           print_newline ()
         })
        gd.gwl
  | "t" -> do {
      let g = C'Tournament (rand_gen ()) in
      List.iter
        (fun (gm, drw) -> do { gm.game_type := g; start_new_game gd gm drw })
        gd.gwl;
      List.iter (fun (gm, drw) -> draw_players gd gm drw) gd.gwl;
      print_string "tournament";
      print_newline ()
    }
  | "v" -> do {
      trace_games.val := not trace_games.val;
      if not trace_games.val then print_string "un" else ();
      print_string "tracing games";
      print_newline ()
    }
  | "\n" -> ()
  | _ -> do {
      print_string "*** available commands:";
      print_newline ();
      print_string "s: status of games";
      print_newline ();
      print_string "t: tournament";
      print_newline ();
      print_string "n: new player";
      print_newline ();
      print_string "c: game commands";
      print_newline ();
      print_string "v: ";
      if trace_games.val then print_string "un" else ();
      print_string "trace games";
      print_newline ();
      print_string "q: general quit";
      print_newline ()
    } ]
;

value stdin_fun gd buff len =
  let len =
    if len > 0 && String.get buff (len - 1) == '\n' then len - 1 else len
  in
  match gd.gstate with
  [ GSRunning ->
      for i0 = 0 to len - 1 do {
        stdin_act gd (String.make 1 (String.get buff i0));
      }
  | GSReadingDisplay -> do {
      gd.gstate := GSRunning;
      let dname = String.sub buff 0 len in
      try one_column (dname, (fst (List.hd gd.gwl)).pint + 1, gd) with _ ->
        do {
          print_string "*** Can't open display \"";
          print_string dname;
          print_string "\"";
          print_newline ()
        }
    }
  | GSReadingName xd dname pint -> do {
      gd.gstate := GSRunning;
      one_column_continued (dname, pint, gd, xd) (String.sub buff 0 len);
      List.iter (fun (gm, drw) -> draw_players gd gm drw) gd.gwl
    } ]
;

value columns dnamel = do {
  let rec start_columns gd =
    fun
    [ [dname :: dnamel] -> do {
        start_columns gd dnamel;
        try one_column (dname, succ (List.length gd.gwl), gd) with _ ->
          do {
            print_string "*** Can't open display \"";
            print_string dname;
            print_string "\"";
            print_newline ()
          }
      }
    | _ -> () ]
  and exec_columns gd =
    try do {
Printf.eprintf "*** exec_columns 1\n"; flush stderr;
      rt_main_loop gd.xargs;
Printf.eprintf "*** exec_columns 2\n"; flush stderr;
      List.iter
        (fun (gm, _) -> do {
Printf.eprintf "*** exec_columns 3\n"; flush stderr;
           if trace_games.val then display_closing_display gm.dname else ();
           rt_end gm.xd
         })
        gd.gwl
    }
    with x -> do {
      print_newline ();
      print_string "*** error in execution";
      print_newline ();
      List.iter
        (fun (gm, _) -> do {
           display_closing_display gm.dname;
           try rt_end gm.xd with _ -> ()
         })
        gd.gwl;
      raise x
    }
  in
  init_random ((ftime ()).time mod 32768);
  trace_games.val := True;
  let gd =
    {xargs = rt_args []; init_time = (ftime ()).time; gstate = GSRunning;
     gwl = []}
  in
  start_columns gd dnamel;
  rt_select_file gd.xargs 0 (stdin_fun gd);
  rt_set_timeout_fun gd.xargs (woops_fun gd);
  exec_columns gd
};

value column dname = columns [dname];

value dname = ref "";

let i0 = ref 1 in
while i0.val < Array.length argv do {
  match argv.(i0.val) with
  [ "-bw" -> bW.val := True
  | "-d" -> incr i0
  | s -> dname.val := s ];
  incr i0
};

try columns [dname.val] with
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
| Xlib.X_failure s -> do {
    print_string "X_failure: ";
    print_string s;
    print_newline ()
  }
| x -> do {
    print_string "Unknown exception in column";
    print_newline ();
    raise x
  } ];
