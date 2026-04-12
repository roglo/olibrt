(* $Id: sokoban.ml,v 1.55 2017/05/14 01:11:49 deraugla Exp $ *)

open Rt;
open Printf;

type case =
  [ Wall
  | Dest
  | Empty ]
;

type board =
  { tab : mutable array (array case);
    obj : mutable array (array bool);
    nlin : mutable int;
    ncol : mutable int;
    hero : mutable (int * int);
    move : mutable int;
    undo : mutable list (int * int * bool);
    redo : mutable list (int * int) }
;

type game = { curr : mutable board; lev : mutable int };

type state =
  [ Normal
  | AskForGoto
  | AskForNext
  | AskForPrev
  | Goto of int
  | Jumpto of int
  | Run
  | AutoPushSelectingObj
  | AutoPushSelectingDest of (int * int)
  | Help ]
;

type config =
  { xa : xargs;
    xd : xdata;
    wall_col : color;
    empty_col : color;
    goal_col : color;
    hero_col : color;
    obj_col : color;
    lang : string;
    lexicon : Hashtbl.t string string;
    state : mutable state;
    level_displayed : mutable bool;
    in_window : mutable bool;
    blocked : mutable list (int * int);
    display_blocked : mutable bool;
    solution_size : mutable int;
    board_width : mutable int;
    board_height : mutable int }
;
value square_len = ref 28;

Rt.button_font.val := "-*-terminus-bold-r-*-32-*";
Rt.title_font.val := "-*-terminus-bold-o-*-32-*";
Rt.term_font.(0) := "-*-terminus-medium-r-*-32-*";
Rt.term_font.(1) := "-*-terminus-bold-r-*-32-*";
Rt.term_font.(2) := "-*-terminus-medium-o-*-32-*";
Rt.term_font.(3) := "-*-terminus-bold-o-*-32-*";

value dsol = ref "usol";
value file_level n = "screens/screen." ^ string_of_int n;
value file_solution n = Filename.concat dsol.val (sprintf "solution.%d" n);

value copy_matrix m =
  Array.init (Array.length m)
    (fun i ->
       let a = Array.unsafe_get m i in
       Array.init (Array.length a) (Array.unsafe_get a))
;

module Buff =
  struct
    value buff = ref (Bytes.create 80);
    value store len x =
      do {
        if len >= String.length buff.val then
          buff.val := buff.val ^ Bytes.create (String.length buff.val)
        else ();
        Bytes.set buff.val len x;
        succ len
      }
    ;
    value mstore len s =
      add_rec len 0 where rec add_rec len i =
        if i == String.length s then len
        else add_rec (store len s.[i]) (succ i)
    ;
    value get len = String.sub buff.val 0 len;
  end
;

value input_lexicon lang =
  let t = Hashtbl.create 501 in
  try
    let ic = open_in "lexicon.txt" in
    let derived_lang =
      try String.sub lang 0 (String.index lang '-') with [ Not_found -> "" ]
    in
    try
      do {
        try
          while True do {
            let k =
              find_key (input_line ic) where rec find_key line =
                if String.length line < 4 then find_key (input_line ic)
                else if String.sub line 0 4 <> "    " then
                  find_key (input_line ic)
                else line
            in
            let k = String.sub k 4 (String.length k - 4) in
            let rec loop line =
              match
                try Some (String.index line ':') with [ Not_found -> None ]
              with
              [ Some i ->
                  let line_lang = String.sub line 0 i in
                  do {
                    if line_lang = lang ||
                       line_lang = derived_lang && not (Hashtbl.mem t k)
                    then
                      let v =
                        if i + 1 = String.length line then ""
                        else
                          String.sub line (i + 2) (String.length line - i - 2)
                      in
                      Hashtbl.add t k v
                    else ();
                    loop (input_line ic)
                  }
              | None -> () ]
            in
            loop (input_line ic)
          }
        with
        [ End_of_file -> () ];
        close_in ic;
        t
      }
    with e ->
      do { close_in ic; raise e }
  with
  [ Sys_error _ -> t ]
;

value transl conf s =
  try Hashtbl.find conf.lexicon s with [ Not_found -> "[" ^ s ^ "]" ]
;

value failed_format s : format 'a 'b 'c = Obj.magic ("[" ^ s ^ "]");

value valid_format ini_fmt (r : string) =
  let s : string = Obj.magic (ini_fmt : format 'a 'b 'c) in
  let rec loop i j =
    if i < String.length s - 1 && j < String.length r - 1 then
      match (s.[i], s.[i + 1], r.[j], r.[j + 1]) with
      [ ('%', x, '%', y) ->
          if x = y then loop (i + 2) (j + 2) else failed_format s
      | ('%', _, _, _) -> loop i (j + 1)
      | (_, _, '%', _) -> loop (i + 1) j
      | _ -> loop (i + 1) (j + 1) ]
    else if i < String.length s - 1 then
      if s.[i] == '%' then failed_format s else loop (i + 1) j
    else if j < String.length r - 1 then
      if r.[j] == '%' then failed_format s else loop i (j + 1)
    else (Obj.magic r : format 'a 'b 'c)
  in
  loop 0 0
;

value ftransl conf s = valid_format s (transl conf (Obj.magic s : string));

value skip_lang s =
  loop where rec loop i =
    if i = String.length s then None
    else
      match s.[i] with
      [ 'a'..'z' | '-' -> loop (i + 1)
      | _ -> Some i ]
;

value transl_inline lang macro_char macro s =
  let lang = lang ^ ":" in
  let derived_lang =
    try
      let i = String.index lang '-' in
      String.sub lang 0 i ^ ":"
    with
    [ Not_found -> "" ]
  in
  let rec loop alt_version bol i =
    if i = String.length s then
      match alt_version with
      [ Some s -> s
      | None -> ".........." ]
    else if bol then
      match skip_lang s i with
      [ Some j when s.[j] = ':' ->
          let curr_lang = String.sub s i (j + 1 - i) in
          if curr_lang = lang || curr_lang = derived_lang || curr_lang = "en:"
          then
            let (s, i) =
              let j = if s.[j + 1] = ' ' then j + 1 else j in
              let rec loop len j =
                if j = String.length s then (Buff.get len, j)
                else if s.[j] = '\n' then
                  if j + 1 < String.length s && s.[j + 1] = ' ' then
                    let j =
                      loop (j + 1) where rec loop j =
                        if j < String.length s && s.[j] = ' ' then
                          loop (j + 1)
                        else j
                    in
                    loop (Buff.store len '\n') j
                  else (Buff.get len, j)
                else if s.[j] == macro_char then
                  let (s, j) = macro s (j + 1) in
                  loop (Buff.mstore len s) j
                else loop (Buff.store len s.[j]) (j + 1)
              in
              loop 0 (j + 1)
            in
            if curr_lang = lang then s
            else
              let alt_version =
                if curr_lang = derived_lang then Some s
                else if alt_version = None then
                  let s = if s = "" then s else "[" ^ s ^ "]" in
                  Some s
                else alt_version
              in
              loop alt_version True i
          else loop alt_version (s.[i] = '\n') (i + 1)
      | _ -> loop alt_version (s.[i] = '\n') (i + 1) ]
    else loop alt_version (s.[i] = '\n') (i + 1)
  in
  loop None True 0
;

value rec skip_comm s i =
  if i = String.length s then ("", i)
  else if s.[i] = '\n' then ("", i + 1)
  else skip_comm s (i + 1)
;

value rec interp_stream print lang strm =
  try
    let rec loop =
      fun
      [ '#' ->
          let rec skip_to_eol c =
            if c = '\n' then loop (Stream.next strm)
            else skip_to_eol (Stream.next strm)
          in
          skip_to_eol (Stream.next strm)
      | '\\' ->
          let c =
            match Stream.next strm with
            [ 't' -> '\t'
            | c -> c ]
          in
          do { print c; loop (Stream.next strm) }
      | '[' ->
          let c = Stream.next strm in
          if c = '\n' then do {
            let s =
              let b = Buffer.create 50 in
              let rec loop =
                fun
                [ '\n' ->
                    let c = Stream.next strm in
                    if c = ']' then do {
                      let s = Buffer.contents b in
                      Buffer.clear b;
                      interp_stream (Buffer.add_char b) lang
                        (Stream.of_string s);
                      Buffer.contents b
                    }
                    else do { Buffer.add_char b '\n'; loop c }
                | c -> do { Buffer.add_char b c; loop (Stream.next strm) } ]
              in
              loop (Stream.next strm)
            in
            String.iter print (transl_inline lang '#' skip_comm s);
            loop (Stream.next strm)
          }
          else do { print '['; loop c }
      | c -> do { print c; loop (Stream.next strm) } ]
    in
    loop (Stream.next strm)
  with
  [ Stream.Failure -> () ]
;

value interp_file print lang fname =
  try
    let ic = open_in fname in
    do { interp_stream print lang (Stream.of_channel ic); close_in ic }
  with
  [ Sys_error _ ->
      String.iter print (sprintf "Cannot open file \"%s\"" fname) ]
;

value help_term_nlin = 21;
value help_term_ncol = 50;

value bell xd = ();

value expose_empty conf drw x y width height =
  let poly =
    [(x + width / 2, y + width); (x + width, y + width / 2);
     (x + width / 2, y); (x, y + width / 2)]
  in
  do {
    rt_select_color (rt_white_color conf.xd);
    rt_fill_rectangle drw (x, y, width, height);
    rt_select_color conf.empty_col;
    rt_fill_polygon drw poly
  }
;

value expose_goal conf drw x y width height =
  let xc = x + width / 2 in
  let yc = y + height / 2 in
  let widc = 1 * width / 4 in
  let heic = 1 * height / 4 in
  do {
    rt_select_color conf.goal_col;
    rt_fill_arc drw (xc - widc / 2, yc - heic / 2, widc, heic, 0, 360 * 64)
  }
;

value expose_obj conf drw x y width height =
  let xc = x + width / 2 in
  let yc = y + height / 2 in
  let widc = 2 * width / 4 in
  let heic = 2 * height / 4 in
  do {
    rt_select_color conf.obj_col;
    rt_fill_arc drw (xc - widc / 2, yc - heic / 2, widc, heic, 0, 360 * 64)
  }
;

value expose_hero conf drw x y width height =
  let xc = x + width / 2 in
  let yc = y + height / 2 in
  let widc = 1 * width / 4 in
  let heic = 1 * height / 4 in
  do {
    rt_select_color conf.hero_col;
    let width = if conf.in_window then 3 else 2 in
    rt_set_line_width conf.xd width;
    rt_draw_arc drw (xc - widc / 2, yc - heic / 2, widc, heic, 0, 360 * 64)
  }
;

value xy_of_ij conf brd (i, j) =
  let x =
    (conf.board_width - brd.ncol * square_len.val) / 2 + j * square_len.val
  in
  let y =
    (conf.board_height - (brd.nlin - 1) * square_len.val) / 2 +
     i * square_len.val
  in
  (x, y)
;

value ij_of_xy conf brd (x, y) =
  let i =
    (y - (conf.board_height - (brd.nlin - 1) * square_len.val) / 2) /
    square_len.val
  in
  let j = (x - (conf.board_width - brd.ncol * square_len.val) / 2) /
    square_len.val in
  (i, j)
;

value expose_case conf brd wid (i, j) =
  let drw = WidgetDr wid in
  let (x, y) = xy_of_ij conf brd (i, j) in
  let width = square_len.val in
  let height = square_len.val in
  do {
    if brd.tab.(i).(j) = Wall then do {
      rt_select_color conf.wall_col;
      rt_fill_rectangle drw (x, y, width, height)
    }
    else expose_empty conf drw x y width height;
    if brd.obj.(i).(j) then expose_obj conf drw x y width height else ();
    if brd.tab.(i).(j) = Dest then expose_goal conf drw x y width height
    else ();
    if i = fst brd.hero && j = snd brd.hero then
      let (x, y) = xy_of_ij conf brd brd.hero in
      expose_hero conf drw x y square_len.val square_len.val
    else ()
  }
;

value expose_blocked conf brd wid (i, j) =
  let drw = WidgetDr wid in
  let (x, y) = xy_of_ij conf brd (i, j) in
  let width = square_len.val in
  let height = square_len.val in
  let e = width / 4 in
  let f = height / 4 in
  do {
    rt_select_color conf.obj_col;
    rt_set_line_width conf.xd 2;
    rt_draw_line drw (x + e, y + f) (x + width - e, y + height - f);
    rt_draw_line drw (x + e, y + height - f) (x + width - e, y + f)
  }
;

value erase_case conf brd wid (i, j) =
  let (x, y) = xy_of_ij conf brd (i, j) in
  let width = square_len.val in
  let height = square_len.val in
  do {
    rt_select_color (rt_white_color conf.xd);
    rt_fill_rectangle (WidgetDr wid) (x, y, width, height)
  }
;

value expose_board conf brd wid =
  do {
    for i = 0 to brd.nlin - 1 do {
      for j = 0 to brd.ncol - 1 do { expose_case conf brd wid (i, j) }
    };
    if conf.display_blocked then
      List.iter (expose_blocked conf brd wid) conf.blocked
    else ()
  }
;

value show_cursor = "\027[?35l";
value hide_cursor = "\027[?35h";
value set_newline_mode = "\027[20h";
value clear_screen = "\027[2J";
value home = "\027[H";

value clear_txt conf =
  let twid = widget_named conf.xd "dialog" in
  term_send twid (sprintf "%s%s" clear_screen home)
;

value display_txt conf txt =
  let twid = widget_named conf.xd "dialog" in
  do { term_send twid txt; conf.level_displayed := False }
;

value display_move conf gm =
  let twid = widget_named conf.xd "move" in
  let brd = gm.curr in
  do {
    term_send twid (sprintf "%s%s" clear_screen home);
    if brd.move = 0 then ()
    else term_send twid (sprintf "%6s" ("(" ^ string_of_int brd.move ^ ")"))
  }
;

value display_help conf =
  let twid = widget_named conf.xd "help_term" in
  do {
    term_send twid set_newline_mode;
    interp_file (fun c -> term_send twid (String.make 1 c)) conf.lang
      "help.txt"
  }
;

value is_wall brd i j =
  if i < 0 || j < 0 || i >= brd.nlin || j >= brd.ncol then True
  else brd.tab.(i).(j) = Wall
;

value is_obj brd i j =
  if i < 0 || j < 0 || i >= brd.nlin || j >= brd.ncol then False
  else brd.obj.(i).(j)
;

value rec cannot_move di dj ijl brd i j =
  if List.mem (i, j) ijl then True
  else
    let ijl = [(i, j) :: ijl] in
    is_wall brd (i + di) (j + dj) ||
    is_obj brd (i + di) (j + dj) &&
    (cannot_move dj di ijl brd (i + di) (j + dj) ||
     cannot_move (- dj) (- di) ijl brd (i + di) (j + dj))
;

value cannot_move_left = cannot_move 0 (-1) [];
value cannot_move_right = cannot_move 0 1 [];
value cannot_move_up = cannot_move (-1) 0 [];
value cannot_move_down = cannot_move 1 0 [];

value rec borded_by_wall_upto_wall brd i j di dj ndest nobj =
  let i = i + di in
  let j = j + dj in
  if is_wall brd i j then (ndest, nobj)
  else if
    is_wall brd (i - abs dj) (j - abs di) ||
    is_wall brd (i + abs dj) (j + abs di)
  then
    let ndest = if brd.tab.(i).(j) = Dest then ndest + 1 else ndest in
    let nobj = if brd.obj.(i).(j) then nobj + 1 else nobj in
    borded_by_wall_upto_wall brd i j di dj ndest nobj
  else (100, 0)
;

value is_blocked_obj brd i j =
  if brd.tab.(i).(j) = Dest then False
  else if
    (cannot_move_left brd i j || cannot_move_right brd i j) &&
    (cannot_move_up brd i j || cannot_move_down brd i j) then True
  else if is_wall brd (i - 1) j || is_wall brd (i + 1) j then
    let (ndest, nobj) = borded_by_wall_upto_wall brd i j 0 (-1) 0 0 in
    let (ndest, nobj) = borded_by_wall_upto_wall brd i j 0 1 ndest nobj in
    ndest <= nobj
  else if is_wall brd i (j - 1) || is_wall brd i (j + 1) then
    let (ndest, nobj) = borded_by_wall_upto_wall brd i j (-1) 0 0 0 in
    let (ndest, nobj) = borded_by_wall_upto_wall brd i j 1 0 ndest nobj in
    ndest <= nobj
  else False
;

value find_blocked brd =
  loop [] 0 0 where rec loop list i j =
    if i = brd.nlin then list
    else if j = brd.ncol then loop list (i + 1) 0
    else if brd.obj.(i).(j) && is_blocked_obj brd i j then
      loop [(i, j) :: list] i (j + 1)
    else loop list i (j + 1)
;

value display_blocked conf gm wid =
  do {
    List.iter (expose_case conf gm.curr wid) conf.blocked;
    conf.blocked := find_blocked gm.curr;
    if conf.display_blocked then
      List.iter (expose_blocked conf gm.curr wid) conf.blocked
    else ()
  }
;

value display_level conf gm =
  if conf.level_displayed then ()
  else do {
    clear_txt conf;
    display_txt conf (sprintf "%s %d" (transl conf "Level") gm.lev);
    if Sys.file_exists (file_solution gm.lev) then display_txt conf " *"
    else ();
    conf.level_displayed := True
  }
;

value check_won brd =
  loop 0 where rec loop i =
    if i == brd.nlin then True
    else
      let rec loop_j j =
        if j == brd.ncol then loop (i + 1)
        else if brd.obj.(i).(j) then
          if brd.tab.(i).(j) = Dest then loop_j (j + 1) else False
        else loop_j (j + 1)
      in
      loop_j 0
;

value rec input_move ic =
  match input_char ic with
  [ 'U' -> (-1, 0)
  | 'D' -> (1, 0)
  | 'L' -> (0, -1)
  | 'R' -> (0, 1)
  | '\n' -> input_move ic
  | _ -> raise End_of_file ]
;

value output_move oc (di, dj) =
  match (di, dj) with
  [ (-1, 0) -> output_char oc 'U'
  | (1, 0) -> output_char oc 'D'
  | (0, -1) -> output_char oc 'L'
  | (0, 1) -> output_char oc 'R'
  | _ -> assert False ]
;

value output_move_nl oc dij = do { output_move oc dij; output_char oc '\n' };

value input_moves ic =
  loop [] where rec loop ml =
    match try Some (input_move ic) with [ End_of_file -> None ] with
    [ Some m -> loop [m :: ml]
    | None -> ml ]
;

value load_solution conf lev =
  try
    let ic = open_in (file_solution lev) in
    let list = input_moves ic in
    do { close_in ic; List.rev list }
  with
  [ Sys_error _ -> [] ]
;

value store_solution conf gm =
  let list = List.rev gm.curr.undo in
  let old_list = load_solution conf gm.lev in
  if old_list = [] || List.length list < List.length old_list then
    let fname = file_solution gm.lev in
    try
      do {
        if old_list = [] then ()
        else do {
          try Sys.remove (fname ^ ".old") with [ Sys_error _ -> () ];
          Sys.rename fname (fname ^ ".old")
        };
        let oc = open_out fname in
        List.iter (fun (di, dj, _) -> output_move_nl oc (di, dj)) list;
        close_out oc;
        let s =
          if old_list = [] then transl conf "Solution saved"
          else transl conf "Better solution saved"
        in
        display_txt conf (sprintf " %s." s)
      }
    with
    [ Sys_error _ ->
        do { eprintf "Cannot write \"%s\"\n" fname; flush stderr } ]
  else ()
;

value forward brd (di, dj) =
  let (i, j) = brd.hero in
  let i = i + di in
  let j = j + dj in
  do {
    brd.hero := (i, j);
    let obj_moved = brd.obj.(i).(j) in
    if obj_moved then do {
      brd.obj.(i + di).(j + dj) := True;
      brd.obj.(i).(j) := False;
    }
    else ();
    brd.undo := [(di, dj, obj_moved) :: brd.undo];
    brd.move := brd.move + 1
  }
;

value backward brd (di, dj, obj_moved) =
  let (i, j) = brd.hero in
  let i = i - di in
  let j = j - dj in
  do {
    brd.hero := (i, j);
    if obj_moved then do {
      brd.obj.(i + di).(j + dj) := True;
      brd.obj.(i + 2 * di).(j + 2 * dj) := False
    }
    else ();
    brd.redo := [(di, dj) :: brd.redo];
    brd.move := brd.move - 1
  }
;

value move conf gm wid (di, dj) =
  let brd = gm.curr in
  let (i, j) = brd.hero in
  if i + di < 0 || i + di >= brd.nlin || j + dj < 0 || j + dj >= brd.ncol then
    bell conf.xd
  else if brd.tab.(i + di).(j + dj) = Wall then bell conf.xd
  else if brd.obj.(i + di).(j + dj) then
    if i + 2 * di < 0 || i + 2 * di >= brd.nlin || j + 2 * dj < 0 ||
       j + 2 * dj >= brd.ncol
    then
      bell conf.xd
    else if brd.tab.(i + 2 * di).(j + 2 * dj) = Wall then bell conf.xd
    else if brd.obj.(i + 2 * di).(j + 2 * dj) then bell conf.xd
    else do {
      brd.hero := (i + di, j + dj);
      brd.obj.(i + 2 * di).(j + 2 * dj) := True;
      brd.obj.(i + di).(j + dj) := False;
      erase_case conf brd wid (i, j);
      expose_case conf brd wid (i, j);
      erase_case conf brd wid (i + di, j + dj);
      expose_case conf brd wid (i + di, j + dj);
      erase_case conf brd wid (i + 2 * di, j + 2 * dj);
      expose_case conf brd wid (i + 2 * di, j + 2 * dj);
      brd.move := brd.move + 1;
      brd.undo := [(di, dj, True) :: brd.undo];
      brd.redo := [];
      conf.solution_size := brd.move;
      display_move conf gm;
      if check_won brd then do {
        clear_txt conf;
        display_txt conf
          (sprintf "%s %d %s." (transl conf "Level") gm.lev
             (transl conf "done"));
        store_solution conf gm;
        if Sys.file_exists (file_level (gm.lev + 1)) then
          display_txt conf
            (sprintf " %s." (transl conf "Hit 'n' for next level"))
        else ()
      }
      else do { display_level conf gm; display_blocked conf gm wid }
    }
  else do {
    brd.hero := (i + di, j + dj);
    erase_case conf brd wid (i, j);
    expose_case conf brd wid (i, j);
    erase_case conf brd wid (i + di, j + dj);
    expose_case conf brd wid (i + di, j + dj);
    brd.move := brd.move + 1;
    brd.undo := [(di, dj, False) :: brd.undo];
    brd.redo := [];
    conf.solution_size := brd.move;
    display_move conf gm;
    display_level conf gm;
    display_blocked conf gm wid
  }
;

value move_obj conf gm wid (i, j) (di, dj) =
  let brd = gm.curr in
  do {
    brd.hero := (i, j);
    brd.obj.(i + di).(j + dj) := True;
    brd.obj.(i).(j) := False;
    erase_case conf brd wid (i - di, j - dj);
    expose_case conf brd wid (i - di, j - dj);
    expose_case conf brd wid (i, j);
    expose_case conf brd wid (i + di, j + dj)
  }
;

value inside brd i j = i >= 0 && i < brd.nlin && j >= 0 && j < brd.ncol;

exception Found of list (int * int) and int;
value wait_time = 50;

value rec run conf gm wid path () =
  match path with
  [ [(di, dj) :: path] ->
      do {
        move conf gm wid (di, dj);
        if path = [] then conf.state := Normal
        else do {
          conf.state := Run;
          rt_set_timeout conf.xa "run" (rt_immediate_time conf.xa + wait_time)
            (run conf gm wid path)
        }
      }
  | [] -> () ]
;

module PrioStack =
  struct
    type t 'a =
      { tab : mutable array (list 'a);
        first : mutable int;
        len : mutable int }
    ;
    value make sz = {tab = Array.make sz []; first = sz; len = 0};
    value rec push x p s =
      if p >= Array.length s.tab then do {
        s.tab :=
          Array.append s.tab (Array.make (p - Array.length s.tab + 1) []);
        push x p s
      }
      else do {
        s.tab.(p) := [x :: s.tab.(p)];
        s.first := min s.first p;
        s.len := s.len + 1
      }
    ;
    value rec pop s =
      if s.first >= Array.length s.tab then None
      else
        match s.tab.(s.first) with
        [ [x :: l] -> do { s.tab.(s.first) := l; s.len := s.len - 1; Some x }
        | [] ->
            do {
              s.first := s.first + 1;
              (* printf "... end level %d\n%!" s.first; *) 
              pop s
            } ]
    ;
  end
;

type search_path =
  { io : int;
    jo : int;
    opath : list (int * int);
    olen : int;
    ih : int;
    jh : int;
    rhpath : list (int * int);
    hlen : int }
;

value dir_list = [(0, 0, -1); (1, -1, 0); (2, 0, 1); (3, 1, 0)];

value find_path brd (ori_i, ori_j) (dst_i, dst_j) omax =
  let maxlen =
    match omax with
    [ Some x -> x
    | _ -> max_int ]
  in
  let m =
    let m = Array.make_matrix brd.nlin brd.ncol True in
    (* to be faster: mark (by "False") the connex component of the
       destination: if the origin is outside, the access is impossible *)
    let rec loop i j =
      if inside brd i j && brd.tab.(i).(j) <> Wall && not brd.obj.(i).(j) &&
         m.(i).(j)
      then do {
        m.(i).(j) := False;
        List.iter (fun (_, di, dj) -> loop (i + di) (j + dj)) dir_list
      }
      else ()
    in
    do { loop dst_i dst_j; m }
  in
  if m.(ori_i).(ori_j) then
    (* not accessible *)
    None
  else
    let rec loop res new_spaths =
      fun
      [ [(i, j, path, len) :: spaths] ->
          if len >= maxlen then loop None new_spaths spaths
          else if i = dst_i && j = dst_j then
            (* path found: making a path by deltas (di, dj) *)
            let (_, _, r, len) =
              List.fold_left
                (fun (i2, j2, path, len) (i1, j1) ->
                   (i1, j1, [(i2 - i1, j2 - j1) :: path], len + 1))
                (dst_i, dst_j, [], 0) path
            in
            loop (Some (r, len)) [] []
          else do {
            (* advancing in all directions *)
            m.(i).(j) := True;
            let path = [(i, j) :: path] in
            let new_spaths =
              List.fold_left
                (fun new_spaths (_, di, dj) ->
                   let (i, j) = (i + di, j + dj) in
                   if inside brd i j && not m.(i).(j) &&
                      brd.tab.(i).(j) <> Wall && not brd.obj.(i).(j)
                   then
                     [(i, j, path, len + 1) :: new_spaths]
                   else new_spaths)
                new_spaths dir_list
            in
            loop None new_spaths spaths
          }
      | [] -> if new_spaths = [] then res else loop None [] new_spaths ]
    in
    loop None [] [(ori_i, ori_j, [], 0)]
;

value find_path_for_obj brd (ih, jh) (ori_i, ori_j) (dst_i, dst_j) conf wid =
  let m =
    Array.init brd.nlin
      (fun _ -> Array.init brd.ncol (fun _ -> Array.make 4 max_int))
  in
  let best = ref None in
  try
    let spaths = PrioStack.make 20 in
    let rec loop =
      fun
      [ Some sp ->
          let _ =
            match best.val with
            [ Some (rhpath, len) when len <= sp.hlen ->
                raise (Found (List.rev rhpath) len)
            | _ ->
                if sp.io = dst_i && sp.jo = dst_j then
                  best.val := Some (sp.rhpath, sp.hlen)
                else () ]
          in
          do {
            List.iter
              (fun (dir, di, dj) ->
                 let (ih, jh) = (sp.io - di, sp.jo - dj) in
                 let (io, jo) = (sp.io + di, sp.jo + dj) in
                 if inside brd io jo && brd.tab.(io).(jo) <> Wall &&
                    inside brd ih jh && brd.tab.(ih).(jh) <> Wall
                 then do {
                   brd.obj.(ori_i).(ori_j) := False;
                   brd.obj.(sp.io).(sp.jo) := True;
                   let hpo =
                     if brd.obj.(io).(jo) then None
                     else if is_blocked_obj brd sp.io sp.jo then None
                     else
                       find_path brd (sp.ih, sp.jh) (ih, jh)
                         (Some m.(sp.io).(sp.jo).(dir))
                   in
                   brd.obj.(sp.io).(sp.jo) := False;
                   brd.obj.(ori_i).(ori_j) := True;
                   match hpo with
                   [ Some (hpath, len) ->
                       do {
                         m.(sp.io).(sp.jo).(dir) := len;
                         let opath = [(sp.io, sp.jo) :: sp.opath] in
                         let rhpath =
                           [(di, dj) :: List.rev_append hpath sp.rhpath]
                         in
                         let spath =
                           {io = io; jo = jo; opath = opath;
                            olen = sp.olen + 1; ih = sp.io; jh = sp.jo;
                            rhpath = rhpath; hlen = sp.hlen + 1 + len}
                         in
                         PrioStack.push spath spath.hlen spaths
                       }
                   | None -> () ]
                 }
                 else ())
              dir_list;
            loop (PrioStack.pop spaths)
          }
      | None ->
          match best.val with
          [ Some (rhpath, len) -> Some (List.rev rhpath, len)
          | None -> None ] ]
    in
    let ori_spath =
      {io = ori_i; jo = ori_j; opath = []; olen = 0; ih = ih; jh = jh;
       rhpath = []; hlen = 0}
    in
    loop (Some ori_spath)
  with
  [ Found path len -> Some (path, len) ]
;

value run_to conf gm wid (i, j) =
  if inside gm.curr i j then
    match find_path gm.curr gm.curr.hero (i, j) None with
    [ Some (path, _) -> run conf gm wid path ()
    | None -> bell conf.xd ]
  else bell conf.xd
;

value redo conf gm wid =
  match gm.curr.redo with
  [ [(di, dj) :: redo] ->
      do { move conf gm wid (di, dj); gm.curr.redo := redo }
  | [] -> bell conf.xd ]
;

value undo conf gm wid =
  let brd = gm.curr in
  let (i, j) = brd.hero in
  match brd.undo with
  [ [(di, dj, obj_moved) :: hist] ->
      let i = i - di in
      let j = j - dj in
      do {
        brd.hero := (i, j);
        if obj_moved then do {
          brd.obj.(i + di).(j + dj) := True;
          brd.obj.(i + 2 * di).(j + 2 * dj) := False;
          erase_case conf brd wid (i + 2 * di, j + 2 * dj);
          expose_case conf brd wid (i + 2 * di, j + 2 * dj)
        }
        else ();
        erase_case conf brd wid (i + di, j + dj);
        expose_case conf brd wid (i + di, j + dj);
        erase_case conf brd wid (i, j);
        expose_case conf brd wid (i, j);
        brd.move := brd.move - 1;
        brd.undo := hist;
        brd.redo := [(di, dj) :: brd.redo];
        display_move conf gm;
        display_level conf gm;
        display_blocked conf gm wid
      }
  | [] -> bell conf.xd ]
;

value jumpto conf gm wid =
  do {
    clear_txt conf;
    display_txt conf
      (sprintf "%s: %s" (transl conf "Go to move") show_cursor);
    conf.state := Jumpto (-1)
  }
;

(*
value back_to_prev_move conf gm wid =
  let brd = gm.curr in
  let obj_moved =
    match brd.undo with
    [ [(_, _, obj_moved) :: _] -> obj_moved
    | [] -> 0 ]
  in
  let rec loop =
    fun
    [ [(di, dj, om) :: undo] ->
        if om = 0 || om = obj_moved then
          do {
            backward brd (di, dj, om);
            brd.undo := undo;
            loop undo
          }
        else ()
    | [] -> () ]
  in
  do {
    loop brd.undo;
    expose_board conf brd wid;
    display_move conf gm;
    display_level conf gm;
    display_blocked conf gm wid;
  }
;

value forw_to_next_move conf gm wid =
  let brd = gm.curr in
  let rec loop obj_moved =
    fun
    [ [(di, dj) :: redo] ->
        do {
          forward brd (di, dj);
          brd.redo := redo;
          let (_, _, om) = List.hd brd.undo in
          if obj_moved = 0 then loop om redo
          else if om = 0 then loop obj_moved redo
          else if om = obj_moved then loop obj_moved redo
          else
            loop brd.undo where rec loop =
              fun
              [ [((_, _, om) as u) :: undo] ->
                  if om = obj_moved then ()
                  else do {
                    backward brd u;
                    brd.undo := undo;
                    loop undo;
                  }
              | [] -> () ]
        }
    | [] -> () ]
  in
  do {
    loop 0 brd.redo;
    expose_board conf brd wid;
    display_move conf gm;
    display_level conf gm;
    display_blocked conf gm wid;
  }
;
*)

value set_move conf gm wid move =
  do {
    if move >= 0 then do {
      let brd = gm.curr in
      let rec loop () =
        if brd.move < move then
          match brd.redo with
          [ [r :: redo] -> do { forward brd r; brd.redo := redo; loop () }
          | _ -> () ]
        else if brd.move > move then
          match brd.undo with
          [ [u :: undo] -> do { backward brd u; brd.undo := undo; loop () }
          | _ -> () ]
        else ()
      in
      loop ();
      expose_board conf brd wid;
      display_move conf gm
    }
    else ();
    display_level conf gm;
    display_blocked conf gm wid;
    conf.state := Normal
  }
;

value restart conf gm wid =
  let brd = gm.curr in
  let rec loop () =
    match brd.undo with
    [ [u :: undo] -> do { backward brd u; brd.undo := undo; loop () }
    | [] -> () ]
  in
  do {
    loop ();
    conf.state := Normal;
    expose_board conf brd wid;
    display_move conf gm;
    display_level conf gm;
    display_blocked conf gm wid
  }
;

value add_walls (hi, hj) tab =
  let nlin = Array.length tab in
  let ncol = Array.length tab.(0) in
  let f = Array.init nlin (fun _ -> Array.make ncol False) in
  let rec loop i j =
    if i >= 0 && i < nlin && j >= 0 && j < ncol && not f.(i).(j) &&
       tab.(i).(j) <> Wall
    then do {
      f.(i).(j) := True;
      loop (i + 1) j;
      loop i (j + 1);
      loop (i - 1) j;
      loop i (j - 1)
    }
    else ()
  in
  do {
    loop hi hj;
    for i = 0 to nlin - 1 do {
      for j = 0 to ncol - 1 do {
        if not f.(i).(j) then tab.(i).(j) := Wall else ()
      }
    }
  }
;

value decode_lines max_lin max_col list =
  let tab = Array.init max_lin (fun _ -> Array.make max_col Empty) in
  let obj = Array.init max_lin (fun _ -> Array.make max_col False) in
  let hero = ref (-1, -1) in
  let rec loop i =
    fun
    [ [] -> (tab, obj, hero.val)
    | [line :: list] ->
        do {
          for j = 0 to max_col - 1 do {
            let v =
              if j >= String.length line then Empty
              else
                match line.[j] with
                [ '#' -> Wall
                | ' ' -> Empty
                | '.' -> Dest
                | '$' -> do { obj.(i).(j) := True; Empty }
                | '*' -> do { obj.(i).(j) := True; Dest }
                | '+' -> do { hero.val := (i, j); Dest }
                | '@' -> do { hero.val := (i, j); Empty }
                | _ -> Empty ]
            in
            tab.(i).(j) := v
          };
          loop (i + 1) list
        } ]
  in
  loop 0 list
;

value make_board i lev =
  let (max_lin, max_col, list) = lev in
  let (tab, obj, hero) = decode_lines max_lin max_col list in
  if hero = (-1, -1) then
    let _ = do { eprintf "Bad level %d: no hero\n" i; flush stderr } in
    None
  else do {
    add_walls hero tab;
    let brd =
      {tab = tab; obj = obj; nlin = max_lin; ncol = max_col; hero = hero;
       move = 0; undo = []; redo = []}
    in
    Some brd
  }
;

value read_level i =
  match try Some (open_in (file_level i)) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let rec loop list max_lin max_col =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ None -> do { close_in ic; Some (max_lin, max_col, List.rev list) }
        | Some line ->
            loop [line :: list] (max_lin + 1)
              (max max_col (String.length line)) ]
      in
      loop [] 0 0
  | None -> None ]
;

value read_board i =
  match read_level i with
  [ Some lev -> make_board i lev
  | None -> None ]
;

value set_level conf gm wid nlev =
  do {
    match read_board nlev with
    [ Some brd ->
        let lev_txt = sprintf "%s %d" (transl conf "Level") nlev in
        do {
          gm.curr := brd;
          gm.lev := nlev;
          conf.level_displayed := False;
          conf.solution_size := 0;
          conf.blocked := [];
          rt_clear_widget wid;
          expose_board conf brd wid;
          display_move conf gm;
          display_level conf gm;
          rt_change_widget_name (widget_named conf.xd "main")
            ("Sokoban " ^ lev_txt) ("Sokoban " ^ lev_txt)
        }
    | None ->
        do {
          clear_txt conf;
          match conf.state with
          [ Goto lev ->
              if lev <= 0 then display_level conf gm
              else
                display_txt conf
                  (sprintf "%s %d." (transl conf "No level") lev)
          | _ ->
              display_txt conf
                (sprintf "%s." (transl conf "No more levels")) ]
        } ];
    conf.state := Normal
  }
;

value next conf gm wid = set_level conf gm wid (gm.lev + 1);
value prev conf gm wid = set_level conf gm wid (gm.lev - 1);

value save_state conf gm =
  let oc = open_out "sokoban.state" in
  do {
    fprintf oc "%s\n" conf.lang;
    fprintf oc "%d\n" gm.lev;
    fprintf oc "%s\n" (if conf.display_blocked then "B" else "");
    List.iter (fun (di, dj, _) -> output_move oc (di, dj)) gm.curr.undo;
    fprintf oc "\n";
    List.iter (output_move oc) gm.curr.redo;
    fprintf oc "\n";
    close_out oc
  }
;

value create_game lev =
  fun
  [ Some brd -> {curr = brd; lev = lev}
  | None -> failwith "create_board" ]
;

value checked_forward brd (di, dj) =
  let (i, j) = brd.hero in
  let i = i + di in
  let j = j + dj in
  if brd.tab.(i).(j) = Wall then failwith "forward"
  else if brd.obj.(i).(j) && brd.tab.(i).(j) = Wall then failwith "forward"
  else forward brd (di, dj)
;

value restore_state () =
  try
    let ic = open_in "sokoban.state" in
    let lang = input_line ic in
    let lev = int_of_string (input_line ic) in
    let line = input_line ic in
    let display_blocked = String.length line >= 1 && line.[0] = 'B' in
    let undo = input_moves ic in
    let redo = input_moves ic in
    do {
      close_in ic;
      let brd = read_board lev in
      let gm = create_game lev brd in
      List.iter (checked_forward gm.curr) undo;
      gm.curr.redo := List.rev redo;
      (gm, lang, display_blocked)
    }
  with
  [ Sys_error _ | Failure _ ->
      let brd = read_board 1 in
      (create_game 1 brd, "en", False) ]
;

value quit conf gm wid = do { save_state conf gm; rt_stop_main_loop conf.xa };

value help conf gm wid =
  do {
    conf.state := Help;
    rt_raise_widget (widget_named conf.xd "help");
    clear_txt conf;
    display_txt conf (transl conf "Type space to return to board");
    display_txt conf "."
  }
;

value goto conf gm wid =
  do {
    clear_txt conf;
    display_txt conf
      (sprintf "%s: %s" (transl conf "Go to level") show_cursor);
    conf.state := Goto (-1)
  }
;

value yes conf gm wid =
  do {
    display_txt conf hide_cursor;
    match conf.state with
    [ AskForGoto -> goto conf gm wid
    | AskForNext -> next conf gm wid
    | AskForPrev -> prev conf gm wid
    | _ -> () ]
  }
;

value no conf gm wid =
  do {
    display_txt conf hide_cursor;
    display_level conf gm;
    display_blocked conf gm wid;
    conf.state := Normal
  }
;

value ask_for_next conf gm wid =
  do {
    clear_txt conf;
    display_txt conf
      (sprintf "%s (%s)? %s" (transl conf "Give up current level")
         (transl conf "y/n") show_cursor);
    conf.state := AskForNext
  }
;

value ask_for_prev conf gm wid =
  do {
    clear_txt conf;
    display_txt conf
      (sprintf "%s (%s)? %s" (transl conf "Give up current level")
         (transl conf "y/n") show_cursor);
    conf.state := AskForPrev
  }
;

value ask_for_goto conf gm wid =
  do {
    clear_txt conf;
    display_txt conf
      (sprintf "%s (%s)? %s" (transl conf "Give up current level")
         (transl conf "y/n") show_cursor);
    conf.state := AskForGoto
  }
;

(* Automatic push *)

value auto_push_select_obj conf gm wid =
  do {
    clear_txt conf;
    display_txt conf (transl conf "Click the object to push");
    conf.state := AutoPushSelectingObj
  }
;

value auto_push_select_dest conf gm wid (i, j) =
  if inside gm.curr i j && gm.curr.obj.(i).(j) then do {
    clear_txt conf;
    display_txt conf (transl conf "Click the destination");
    conf.state := AutoPushSelectingDest (i, j)
  }
  else do {
    display_level conf gm; display_blocked conf gm wid; conf.state := Normal
  }
;

value auto_push conf gm wid (ini_i, ini_j) (i, j) =
  let brd = gm.curr in
  if inside brd i j && not brd.obj.(i).(j) && brd.tab.(i).(j) <> Wall then
    let _ =
      do {
        clear_txt conf;
        display_txt conf (sprintf "%s..." (transl conf "Searching"));
        rt_flush conf.xd
      }
    in
    match find_path_for_obj brd brd.hero (ini_i, ini_j) (i, j) conf wid with
    [ Some (path, _) -> run conf gm wid path ()
    | None ->
        do {
          display_level conf gm;
          display_blocked conf gm wid;
          conf.state := Normal
        } ]
  else do {
    display_level conf gm; display_blocked conf gm wid; conf.state := Normal
  }
;

(* Completion (command 'C') from the saved solution *)

value complete conf gm wid =
  match read_board gm.lev with
  [ Some brd ->
      let redo = load_solution conf gm.lev in
      let rec loop =
        fun
        [ [r :: redo] ->
            do {
              forward brd r;
              if brd.hero = gm.curr.hero && brd.obj = gm.curr.obj then do {
                clear_txt conf;
                display_txt conf
                  (sprintf (ftransl conf "Completed from move %d") brd.move);
                gm.curr.redo := redo
              }
              else loop redo
            }
        | [] -> () ]
      in
      loop redo
  | None -> () ]
;

(* Optiminisation (command 'O') by shortening paths *)

value copy_board brd = (copy_matrix brd.obj, brd.hero, brd.move);

value optim conf gm wid =
  let brd = gm.curr in
  do {
    clear_txt conf;
    display_txt conf (sprintf "%s..." (transl conf "Optimizing"));
    rt_flush conf.xd;
    let move = brd.move in
    (* jump to end *)
    let rec loop () =
      match brd.redo with
      [ [r :: redo] -> do { forward brd r; brd.redo := redo; loop () }
      | [] -> () ]
    in
    loop ();
    (* jump backward from end to curr, getting states list *)
    let states =
      loop (brd.move - move) [copy_board brd] brd.hero where rec loop
        n states (i, j) =
        match brd.undo with
        [ [u :: undo] when n > 0 ->
            do {
              backward brd u;
              brd.undo := undo;
              loop (n - 1) [copy_board brd :: states] (i, j)
            }
        | _ -> states ]
    in
    (* build reverted states, keeping, to be faster, only the last state
       when there are consecutive states with no object moved (only hero
       moved) *)
    let rev_states =
      loop [] states where rec loop rev_states states =
        match (rev_states, states) with
        [ ([(obj1, _, _) :: l], [(obj2, _, _) :: _]) when obj1 = obj2 ->
            loop l states
        | (_, [x :: l]) -> loop [x :: rev_states] l
        | (_, []) -> rev_states ]
    in
    (* scanning states, searching for the furthest state with same objects
       positions, trying then to find a path for the hero between the two
       states: this 1/ deletes unuseful possible positions with possibly
       moved objects (and moved back) and 2/ optimizes hero moves *)
    let rec loop =
      fun
      [ [(obj1, hero1, move1) :: states] ->
          (* in remaining states, delete, to be faster, the next states
             with same objects positions *)
          let states =
            loop states where rec loop =
              fun
              [ [(obj2, _, _) :: rest] when obj2 = obj1 -> loop rest
              | rest -> rest ]
          in
          (* scanning *)
          let rec loop1 =
            fun
            [ [(obj2, hero2, move2) :: rev_states] ->
                if move2 <= move1 then loop1 []
                else if obj2 = obj1 then
                  (* state found: trying a shorter path; stopping if found *)
                  match find_path brd hero1 hero2 None with
                  [ Some (path, pathlen) ->
                      let n = move2 - move1 in
                      if pathlen < n then do {
                        display_txt conf home;
                        display_txt conf clear_screen;
                        display_txt conf
                          (sprintf
                             (ftransl conf "Saved %d moves at position (%d)")
                             (n - pathlen) move1);
                        for i = 1 to n do {
                          match brd.redo with
                          [ [r :: redo] -> brd.redo := redo
                          | [] -> () ]
                        };
                        brd.redo := path @ brd.redo
                      }
                      else loop1 rev_states
                  | None -> loop1 rev_states ]
                else loop1 rev_states
            | [] ->
                (* no matching state found: moving forward and trying next
                   state *)
                let move2 =
                  match states with
                  [ [(_, _, move2) :: _] -> move2
                  | _ -> 0 ]
                in
                do {
                  for i = move1 to move2 - 1 do {
                    match brd.redo with
                    [ [r :: redo] -> do { forward brd r; brd.redo := redo }
                    | [] -> () ]
                  };
                  loop states
                } ]
          in
          loop1 rev_states
      | [] ->
          (* end of level *)
          do {
            display_level conf gm;
            if brd.move < conf.solution_size then
              display_txt conf
                (sprintf " (%t)"
                   (fun () ->
                      sprintf (ftransl conf "%d moves instead of %d") brd.move
                        conf.solution_size))
            else ()
          } ]
    in
    loop states;
    expose_board conf brd wid;
    display_move conf gm
  }
;

(* Events *)

value read_number conf num change_state action =
  fun
  [ K_Ascii ('0'..'9' as n) ->
      let nnum = 10 * max 0 num + Char.code n - Char.code '0' in
      if nnum <= num then ()
      else do {
        if num = 0 then display_txt conf "\b \b" else ();
        display_txt conf (String.make 1 n);
        change_state nnum
      }
  | K_BackSpace | K_Delete ->
      if num < 0 then ()
      else do {
        let num = num / 10 in
        display_txt conf "\b \b";
        change_state (if num = 0 then -1 else num)
      }
  | K_Return -> do { display_txt conf hide_cursor; action num }
  | _ -> bell conf.xd ]
;

value key_pressed conf gm wid km =
  match conf.state with
  [ Normal ->
      match km.item with
      [ K_Ascii 'q' -> quit conf gm wid
      | K_Ascii 'h' -> help conf gm wid
      | K_Ascii 'g' ->
          if check_won gm.curr || gm.curr.undo = [] then goto conf gm wid
          else ask_for_goto conf gm wid
      | K_Ascii 'n' ->
          if check_won gm.curr || gm.curr.undo = [] then next conf gm wid
          else ask_for_next conf gm wid
      | K_Ascii 'p' ->
          if check_won gm.curr || gm.curr.undo = [] then prev conf gm wid
          else ask_for_prev conf gm wid
      | K_Ascii 'u' -> undo conf gm wid
      | K_Ascii 'r' -> redo conf gm wid
      | K_Ascii 'j' -> jumpto conf gm wid
(*
      | K_Ascii '{' -> back_to_prev_move conf gm wid
      | K_Ascii '}' -> forw_to_next_move conf gm wid
*)
      | K_Ascii 'B' ->
          do {
            conf.level_displayed := False;
            conf.display_blocked := not conf.display_blocked;
            display_level conf gm;
            display_blocked conf gm wid;
            if conf.display_blocked && conf.blocked = [] then
              display_txt conf (sprintf " (%s)" (transl conf "not blocked"))
            else ()
          }
      | K_Ascii 'C' -> complete conf gm wid
      | K_Ascii 'O' -> optim conf gm wid
      | K_Ascii 'R' -> restart conf gm wid
      | K_Ascii 'L' ->
          if gm.curr.undo = [] then
            let list = load_solution conf gm.lev in
            if list <> [] then do {
              gm.curr.redo := list;
              conf.solution_size := List.length list;
              clear_txt conf;
              display_txt conf
                (sprintf "%s %d %s. %s." (transl conf "Level") gm.lev
                   (transl conf "loaded")
                   (transl conf "Use several 'r' to see the solution"))
            }
            else ()
          else ()
      | K_Ascii '`' -> auto_push_select_obj conf gm wid
      | K_Down -> move conf gm wid (1, 0)
      | K_Up -> move conf gm wid (-1, 0)
      | K_Right -> move conf gm wid (0, 1)
      | K_Left -> move conf gm wid (0, -1)
      | _ -> bell conf.xd ]
  | AskForNext | AskForPrev | AskForGoto ->
      match km.item with
      [ K_Ascii r ->
          let (y, n) =
            let t = transl conf "y/n" in
            if String.length t = 3 then (t.[0], t.[2]) else ('y', 'n')
          in
          if r = y then yes conf gm wid
          else if r = n then no conf gm wid
          else bell conf.xd
      | _ -> bell conf.xd ]
  | Goto lev ->
      read_number conf lev (fun lev -> conf.state := Goto lev)
        (fun lev -> set_level conf gm wid lev) km.item
  | Jumpto mov ->
      read_number conf mov (fun mov -> conf.state := Jumpto mov)
        (fun mov -> set_move conf gm wid mov) km.item
  | Run -> bell conf.xd
  | AutoPushSelectingObj | AutoPushSelectingDest _ ->
      do {
        display_level conf gm;
        display_blocked conf gm wid;
        conf.state := Normal
      }
  | Help ->
      if km.item = K_Ascii ' ' then do {
        display_level conf gm;
        display_blocked conf gm wid;
        rt_raise_widget (widget_named conf.xd "board_pack");
        conf.state := Normal
      }
      else () ]
;

value action_board conf gm wid =
  fun
  [ RawEvExpose x y width height -> expose_board conf gm.curr wid
  | RawEvKeyPress km -> key_pressed conf gm wid km
  | RawEvButtonPress x y x_root y_root bm ->
      let (i, j) = ij_of_xy conf gm.curr (x, y) in
      match conf.state with
      [ AutoPushSelectingObj -> auto_push_select_dest conf gm wid (i, j)
      | AutoPushSelectingDest ini_ij -> auto_push conf gm wid ini_ij (i, j)
      | _ -> run_to conf gm wid (i, j) ]
  | RawEvConfigureNotify _ (width, height, border) ->
      do { conf.board_width := width; conf.board_height := height }
  | RawEvEnterNotify _ _ _ _ ->
      do { conf.in_window := True; expose_case conf gm.curr wid gm.curr.hero }
  | RawEvLeaveNotify ->
      do {
        conf.in_window := False; expose_case conf gm.curr wid gm.curr.hero
      }
  | _ -> () ]
;

value pack1_desc attr args = pack_desc attr args (fun _ _ -> ());
value action_term conf brd wid _ = ();
value action_term conf brd wid _ = ();

value make_widget xa xd (gm, lang, disp_block) =
  let width = square_len.val * 30 in
  let height = square_len.val * 16 in
  let conf =
    {xa = xa; xd = xd; wall_col = rt_closest_color xd (128, 0, 128);
     empty_col = rt_closest_color xd (220, 255, 180);
     goal_col = rt_closest_color xd (0, 0, 255); hero_col = rt_black_color xd;
     obj_col = rt_closest_color xd (255, 0, 0); lang = lang; state = Normal;
     lexicon = input_lexicon lang; level_displayed = False; in_window = False;
     blocked = []; display_blocked = disp_block;
     solution_size = List.length gm.curr.undo + List.length gm.curr.redo;
     board_width = width; board_height = height}
  in
  let filler = pack1_desc [FillerAtt; BorderAtt 0] (Vertical, []) in
  let wid =
    let max_term_len =
      String.length
        (sprintf "%s 0000 %s. %s. %s." (transl conf "Level")
           (transl conf "done") (transl conf "Better solution saved")
           (transl conf "Hit 'n' for next level"))
    in
    rt_create_widget xd "Sokoban" "Sokoban" AutoPosition (Some (quit conf gm))
      (pack1_desc [NameAtt "main"]
         (Vertical,
          [pack1_desc [BandAtt 0; FillerAtt]
             (InDepth,
              [pack1_desc
                 [NameAtt "board_pack"; BorderAtt 0; BandAtt 0; InterAtt 0]
                 (Vertical,
                  [raw_desc
                     [NameAtt "board"; FillerAtt;
                      BackgroundAtt (ColorPn conf.wall_col)]
                     (width, height, 0,
                      [SelExposure; SelKeyPress; SelButtonPress;
                       SelEnterWindow; SelLeaveWindow; SelStructureNotify])
                     (action_board conf gm);
                   pack1_desc [BorderAtt 0; BandAtt 0; InterAtt 0]
                     (Horizontal,
                      [pack1_desc
                         [FillerAtt; BackgroundAtt (ColorPn conf.wall_col);
                          BorderAtt 0]
                         (Vertical, []);
                       term_desc
                         [NameAtt "move"; BorderAtt 0;
                          BackgroundAtt (ColorPn conf.wall_col);
                          ForegroundAtt (ColorPn (rt_white_color conf.xd))]
                         (1, 6, 0) (action_term conf gm)])]);
               pack1_desc [NameAtt "help"; BorderAtt 0]
                 (Vertical,
                  [filler;
                   pack1_desc [BorderAtt 0]
                     (Horizontal,
                      [filler;
                       term_desc [NameAtt "help_term"]
                         (help_term_nlin, help_term_ncol, 0)
                         (action_term conf gm);
                       filler]);
                   filler])]);
           term_desc [NameAtt "dialog"] (1, max_term_len, 0)
             (action_term conf gm)]))
  in
  do {
    display_help conf;
    display_level conf gm;
    display_blocked conf gm wid;
    display_txt conf (sprintf " (%s)" (transl conf "type h for help"));
    display_move conf gm;
    let tit = sprintf "Sokoban %s %d" (transl conf "Level") gm.lev in
    rt_change_widget_name wid tit tit;
    wid
  }
;

value lang = ref "";
value usage_msg = "Usage: sokoban [options]";
value speclist =
  [("-lang", Arg.Set_string lang, "<code>: select language");
   ("-dsol", Arg.Set_string dsol,
    sprintf "<dir>: solutions directory (default = %s)" dsol.val)];
value anon_fun _ = do { Arg.usage speclist usage_msg; exit 1 };

value main dname =
  do {
    Arg.parse speclist anon_fun usage_msg;
    let xd = rt_initialize dname in
    square_len.val := 2 * square_len.val;
    let xa = rt_args [xd] in
    let (gm, slang, disp_block) = restore_state () in
    let lang = if lang.val = "" then slang else lang.val in
    let main_wid = make_widget xa xd (gm, lang, disp_block) in
    rt_redirect_key_press_to (widget_named xd "board");
    rt_map_widget main_wid;
    rt_main_loop xa
  }
;

main "";
