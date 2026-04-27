(* $Id: xabalone.ml,v 1.11 2008/07/21 09:50:13 deraugla Exp $ *)

open Rt;
open Common;
open Computer;

value piec_diam = ref 40;	(* diameter of pieces *)
value hex_outl = ref 3;		(* outline of hexagones holding pieces *)
value hex_bord = ref 3;		(* hexagones border *)

value round x = truncate (x +. 0.5);
value widt2side dm = round (float dm /. sqrt 3.);
value side2widt sd = round (float sd *. sqrt 3.);

(*
  hexagon:

               hex_widt
             <--------->
                  *      |
               *     *   |
           | *         * |
 hex_side  | *         * | 2 * hex_side
           | *         * |
               *     *   |
                  *      |

  board:

                     bd_ins_wid     bd_right  bd_int
            bd_outl       |             |      |   bd_outl
               <--> <---------------> <---->  <->  <-->
               --------------------------------------
               |   |                 |     | |  | |  |  | bd_outl
               |-- |-----------------|-----|-|--|-|--|
            |  |   |    x x x x x    |     | |  | |  |
            |  |   |   x x x x x x   |     | |  | |  |
            |  |   |  . . x x x . .  |     |x|  |o|  |
            |  |   | . . . . . . . . |     |x|  |o|  |
 bd_ins_hei |  |   |. . . . . . . . .|     |x|  |o|  |
            |  |   | . . . . . . . . |     |x|  |o|  |
            |  |   |  . . o o o . .  |     |x|  |o|  |
            |  |   |   o o o o o o   |     |-|--|-|--|
            |  |   |    o o o o o    |     | |  | |  |  | bd_lost
               |-- |-----------------|-----|-|--|-|--|
               |   |                 |     | |  | |  |  | bd_low
               |-- |-----------------|-----|-|--|-|--|
               |   |   x  PLAYER     |     | |  | |  |  | 2 * hex_side
               |-- |-----------------|-----|-|--|-|--|
               |   |   |             |     | |  | |  |  | bd_outl
               -------------------------------------
                   <--->
                   bd_comm
 *)

value hex_widt = ref 0;
value hex_side = ref 0;
value bd_outl = ref 0;
value bd_low = ref 0;
value bd_right = ref 0;
value bd_int = ref 0;
value bd_lost = ref 0;
value bd_comm = ref 0;
value bd_ins_wid = ref 0;
value bd_ins_hei = ref 0;
value bd_wid = ref 0;
value bd_hei = ref 0;

value init_const () = do {
  hex_widt.val := piec_diam.val + 2 * hex_outl.val + hex_bord.val;
  hex_side.val := widt2side hex_widt.val;
  bd_outl.val := 10;
  bd_low.val := 40;
  bd_right.val := 20;
  bd_int.val := 10;
  bd_lost.val := 50;
  bd_comm.val := 100;
  bd_ins_wid.val := (2 * side + 1) * hex_widt.val;
  bd_ins_hei.val := ((2 * side + 1) * 3 + 1) * hex_side.val / 2;
  bd_wid.val :=
    bd_outl.val + bd_ins_wid.val + bd_right.val + piec_diam.val + bd_int.val +
    piec_diam.val + bd_outl.val;
  bd_hei.val :=
    bd_outl.val + bd_ins_hei.val + bd_low.val + 2 * hex_side.val + bd_outl.val
};

init_const ();

value hex_drawn = do {
  let hex_drawn = Array.make (2 * side + 1) [| |] in
  for i = 0 to Array.length hex_drawn - 1 do {
    hex_drawn.(i) := Array.make (2 * side + 1) False;
  };
  hex_drawn
};

value init_hex_drawn () =
  for i = 0 to Array.length hex_drawn - 1 do {
    for j = 0 to Array.length hex_drawn - 1 do { hex_drawn.(i).(j) := False };
  }
;

value not_drawn i j = try not hex_drawn.(i).(j) with _ -> True;

value draw_vhex wid xc yc wd (i, j) = do {
  let sd = widt2side wd in
  if not_drawn (i - 1) j then
    rt_draw_line (WidgetDr wid) (xc, yc - sd) (xc + wd / 2, yc - sd / 2)
  else ();
  if not_drawn i (j + 1) then
    rt_draw_line (WidgetDr wid) (xc + wd / 2, yc - sd / 2)
      (xc + wd / 2, yc + sd / 2)
  else ();
  if not_drawn (i + 1) (j + 1) then
    rt_draw_line (WidgetDr wid) (xc + wd / 2, yc + sd / 2) (xc, yc + sd)
  else ();
  if not_drawn (i + 1) j then
    rt_draw_line (WidgetDr wid) (xc, yc + sd) (xc - wd / 2, yc + sd / 2)
  else ();
  if not_drawn i (j - 1) then
    rt_draw_line (WidgetDr wid) (xc - wd / 2, yc + sd / 2)
      (xc - wd / 2, yc - sd / 2)
  else ();
  if not_drawn (i - 1) (j - 1) then
    rt_draw_line (WidgetDr wid) (xc - wd / 2, yc - sd / 2) (xc, yc - sd)
  else ()
};

type xinfo =
  { xd : xdata;
    xa : xargs;
    watch_mouse : mouse;
    backg_col : unit -> unit;
    piece_col : array (unit -> unit);
    piece_bd_col : array (unit -> unit);
    line_col : color;
    garb_col : color;
    mark_col : array (unit -> unit);
    quit_ask : ref (frozen widget);
    new_game_ask : ref (frozen widget);
    gf : game_fun }
;

value inverted = ref False;

value xpos i j =
  let (i, j) =
    if inverted.val then (2 * side - i, 2 * side - j) else (i, j)
  in
  bd_outl.val + (side - i + 1) * hex_widt.val / 2 + j * hex_widt.val
;

value ypos i j =
  let (i, j) =
    if inverted.val then (2 * side - i, 2 * side - j) else (i, j)
  in
  bd_outl.val + hex_side.val * (3 * i + 2) / 2
;

value pos_of_xy x y =
  let k = (2 * y - 2 * bd_outl.val + hex_side.val) / hex_side.val + 1 in
  let a =
    if k mod 3 <> 2 then
      (x - bd_outl.val + k / 3 * hex_widt.val / 2) / hex_widt.val - side / 2 -
      1
    else -1
  and i = if k mod 3 <> 2 then k / 3 - 1 else -1 in
  let j = if k mod 3 <> 2 then a else -1 in
  let (i, j) =
    if inverted.val then (2 * side - i, 2 * side - j) else (i, j)
  in
  (i, j)
;

value draw_piece_with_col wid inside_col border_col x y = do {
  let rad = piec_diam.val / 2 in
  inside_col ();
  rt_fill_arc (WidgetDr wid)
    (x - rad, y - rad, piec_diam.val, piec_diam.val, 0, 360 * 64);
  border_col ();
  rt_draw_arc (WidgetDr wid)
    (x - rad, y - rad, piec_diam.val, piec_diam.val, 0, 360 * 64)
};

value state_player_of_state =
  fun
  [ Running p -> Running p
  | SelectedMoves p _ -> Running p
  | Tracing p _ -> Running p
  | Playing p _ -> Running p
  | PiecesMarked p -> Running p
  | Wait_confirm _ _ | Terminated | Editing _ as s -> s ]
;

value draw_state wid xinf state = do {
  rt_clear_area wid
    (bd_outl.val, bd_outl.val + bd_ins_hei.val + bd_low.val, bd_ins_wid.val,
     2 * hex_side.val);
  match state_player_of_state state with
  [ Running p -> do {
      let k = player_index p in
      draw_piece_with_col wid xinf.piece_col.(k) xinf.piece_bd_col.(k)
        (bd_outl.val + bd_comm.val + hex_side.val)
        (bd_outl.val + bd_ins_hei.val + bd_low.val + hex_side.val);
      rt_select_color (rt_black_color xinf.xd);
      rt_draw_string (WidgetDr wid)
        (bd_outl.val + bd_comm.val + 2 * hex_side.val,
         bd_outl.val + bd_ins_hei.val + bd_low.val + hex_side.val)
        "Player"
    }
  | Terminated -> do {
      rt_select_color (rt_black_color xinf.xd);
      rt_draw_string (WidgetDr wid)
        (bd_outl.val + bd_comm.val + 2 * hex_side.val,
         bd_outl.val + bd_ins_hei.val + bd_low.val + hex_side.val)
        "Terminated"
    }
  | Editing p -> do {
      rt_select_color (rt_black_color xinf.xd);
      rt_draw_string (WidgetDr wid)
        (bd_outl.val + bd_comm.val + 2 * hex_side.val,
         bd_outl.val + bd_ins_hei.val + bd_low.val + hex_side.val)
        "Editing (end with \"e\")"
    }
  | _ -> () ]
};

value xlost k n =
  bd_outl.val + bd_ins_wid.val + bd_right.val +
  (if k == 0 then 0 else piec_diam.val + bd_int.val) + piec_diam.val / 2
and ylost k n =
  bd_outl.val + bd_ins_hei.val - bd_lost.val -
  (piec_diam.val + hex_outl.val) * n
;

value lost_xmin k = xlost k 0 - piec_diam.val / 2
and lost_xmax k = xlost k 0 + piec_diam.val / 2
and lost_ymin k = ylost k to_kill
and lost_ymax k = ylost k 0;

value loose_piece wid xinf play =
  let k = player_index play in
  let n = cnt.(k) in
  draw_piece_with_col wid xinf.piece_col.(k) xinf.piece_bd_col.(k) (xlost k n)
    (ylost k n)
;

value unloose_piece wid xinf play =
  let k = player_index play in
  let n = cnt.(k) in
  draw_piece_with_col wid xinf.backg_col xinf.backg_col (xlost k n)
    (ylost k n)
;

value draw_piece wid xinf (i, j) =
  let k = player_index (player_of board.(i).(j)) in
  draw_piece_with_col wid xinf.piece_col.(k) xinf.piece_bd_col.(k) (xpos i j)
    (ypos i j)
;

value erase_piece wid xinf (i, j) =
  draw_piece_with_col wid xinf.backg_col xinf.backg_col (xpos i j) (ypos i j)
;

value draw_dir wid xinf (i, j) (i', j') = do {
  draw_piece wid xinf (i, j);
  let p = player_index (player_of board.(i).(j)) in
  xinf.mark_col.(p) ();
  let drw = WidgetDr wid
  and x = xpos i j
  and y = ypos i j in
  rt_set_line_width xinf.xd 3;
  if i' == i then do {
    let k = piec_diam.val / 4 in
    let k = if inverted.val then -k else k in
    rt_draw_line drw (x - k, y) (x + k, y);
    if j' == j + 1 then
      rt_draw_lines drw
        [(x + k / 2, y - k / 2); (x + k, y); (x + k / 2, y + k / 2)]
    else
      rt_draw_lines drw
        [(x - k / 2, y - k / 2); (x - k, y); (x - k / 2, y + k / 2)]
  }
  else if j' == j then do {
    let k1 = piec_diam.val / 6
    and k2 = piec_diam.val / 5 in
    let k1 = if inverted.val then -k1 else k1 in
    let k2 = if inverted.val then -k2 else k2 in
    rt_draw_line drw (x - k1, y + k2) (x + k1, y - k2);
    if i' == i - 1 then
      rt_draw_lines drw [(x, y - k2); (x + k1, y - k2); (x + k1, y)]
    else rt_draw_lines drw [(x, y + k2); (x - k1, y + k2); (x - k1, y)]
  }
  else do {
    let k1 = piec_diam.val / 6
    and k2 = piec_diam.val / 5 in
    let k1 = if inverted.val then -k1 else k1 in
    let k2 = if inverted.val then -k2 else k2 in
    rt_draw_line drw (x + k1, y + k2) (x - k1, y - k2);
    if i' == i - 1 then
      rt_draw_lines drw [(x, y - k2); (x - k1, y - k2); (x - k1, y)]
    else rt_draw_lines drw [(x, y + k2); (x + k1, y + k2); (x + k1, y)]
  };
  rt_set_line_width xinf.xd 0
};

value mark wid mark_col (i, j) = do {
  mark_col.(player_index (player_of board.(i).(j))) ();
  let rad = 2 in
  List.iter
    (fun f ->
       f (WidgetDr wid)
         (xpos i j - rad, ypos i j - rad, 2 * rad, 2 * rad, 0, 360 * 64))
    [rt_fill_arc; rt_draw_arc]
};

value display_move wid xinf play (pieces, di, dj, nb_out) = do {
  let rec display =
    fun
    [ [(i, j)] -> draw_dir wid xinf (i, j) (i + di, j + dj)
    | [(i, j) :: l] -> do {
        if board.(i).(j) == bd_piece_of play then
          mark wid xinf.mark_col (i, j)
        else ();
        display l
      }
    | [] -> invalid_arg "display_move" ]
  in
  Printf.printf "  %d  \013" nb_out;
  flush stdout;
  display pieces
};

value undisplay_move wid xinf play (pieces, _, _, _) =
  undisplay pieces where rec undisplay =
    fun
    [ [(i, j)] -> draw_piece wid xinf (i, j)
    | [(i, j) :: l] -> do {
        if board.(i).(j) == bd_piece_of play then
          mark wid xinf.piece_col (i, j)
        else ();
        undisplay l
      }
    | [] -> invalid_arg "undisplay_move" ]
;

value expose_bd xinf wid (x, y, width, height) = do {
  if x < bd_outl.val + bd_ins_wid.val && y < bd_outl.val + bd_ins_hei.val
  then do {
    init_hex_drawn ();
    rt_set_line_width xinf.xd hex_bord.val;
    rt_select_color xinf.line_col;
    let inverted_save = inverted.val in
    inverted.val := False;
    for i = 0 to 2 * side do {
      for j = 0 to 2 * side do {
        match board.(i).(j) with
        [ Bd_piece _ | Bd_empty -> do {
            draw_vhex wid (xpos i j) (ypos i j) hex_widt.val (i, j);
            hex_drawn.(i).(j) := True
          }
        | _ -> () ];
      };
    };
    rt_set_line_width xinf.xd 0;
    rt_select_color xinf.garb_col;
    for i = 0 to 2 * side do {
      for j = 0 to 2 * side do {
        match board.(i).(j) with
        [ Bd_garb -> do {
            draw_vhex wid (xpos i j) (ypos i j) hex_widt.val (i, j);
            hex_drawn.(i).(j) := True
          }
        | _ -> () ];
      };
    };
    inverted.val := inverted_save;
    for i = 0 to 2 * side do {
      for j = 0 to 2 * side do {
        match board.(i).(j) with
        [ Bd_piece _ -> draw_piece wid xinf (i, j)
        | _ -> () ];
      };
    }
  }
  else ();
  if x + width >= bd_outl.val + bd_ins_wid.val + bd_right.val &&
     y + height >= bd_outl.val &&
     y <= bd_outl.val + bd_ins_hei.val - bd_lost.val
  then
    for k = 0 to 1 do {
      for n = 1 to cnt.(k) do {
        draw_piece_with_col wid xinf.piece_col.(k) xinf.piece_bd_col.(k)
          (xlost k n) (ylost k n);
      };
    }
  else ();
  draw_state wid xinf state.val;
  match
    match state.val with
    [ Tracing p [move :: _] -> Some (p, move)
    | Playing p move -> Some (p, move)
    | _ -> None ]
  with
  [ Some (p, move) -> display_move wid xinf p move
  | None -> () ]
};

type catch =
  { marked_pieces : mutable list (int * int);
    moving_piece : mutable (int * int);
    moving_to : mutable (int * int) }
;
value catch = {marked_pieces = []; moving_piece = (0, 0); moving_to = (0, 0)};

value init_catch () = catch.marked_pieces := [];

value in_board i j =
  i >= 0 && i < Array.length board && j >= 0 && j < Array.length board.(i)
;

value rec check_dir di dj =
  fun
  [ [(i, j) :: ([(i', j') :: _] as l)] ->
      if i = i' + di && j = j' + dj then check_dir di dj l else False
  | _ -> True ]
;

value are_neighbours (i, j) (i', j') =
  i == i' && (j == j' + 1 || j == j' - 1) ||
  i == i' + 1 && (j == j' || j == j' + 1) ||
  i == i' - 1 && (j == j' || j == j' - 1)
;

value are_aligned (i, j) =
  fun
  [ [(i', j') :: _] as l ->
      if are_neighbours (i, j) (i', j') then check_dir (i - i') (j - j') l
      else False
  | [] -> True ]
;

value set_state wid xinf v = do {
  if state_player_of_state state.val <> state_player_of_state v then
    draw_state wid xinf v
  else ();
  state.val := v
};

value cancel_fun xinf wid =
  match state.val with
  [ Tracing p [move :: _] -> do {
      undisplay_move wid xinf p move;
      rt_unmap_widget (widget_named xinf.xd "next_cancel");
      rt_unfreeze_widget (widget_named xinf.xd "next");
      set_state wid xinf (Running p)
    }
  | _ -> () ]
;

value stop_intermediate_command wid xinf = do {
  rt_cancel_timeout xinf.xa "";
  match state.val with
  [ Tracing p [move :: _] -> do {
      cancel_fun xinf wid;
      prerr_endline "abort trace"
    }
  | Playing p move -> do {
      undisplay_move wid xinf p move;
      set_state wid xinf (Running p);
      prerr_endline "abort computer move"
    }
  | SelectedMoves p _ -> do {
      set_state wid xinf (Running p);
      prerr_endline "abort selected moves"
    }
  | _ -> () ]
};

value button_pressed xinf wid (x, y, _, _, {item = but}) = do {
  stop_intermediate_command wid xinf;
  match state.val with
  [ Running p ->
      let (i, j) = pos_of_xy x y in
      let valid = in_board i j && board.(i).(j) == bd_piece_of p in
      if valid then do {
        catch.marked_pieces := [(i, j)];
        mark wid xinf.mark_col (i, j)
      }
      else ()
  | PiecesMarked p ->
      let (i, j) = pos_of_xy x y in
      let valid = in_board i j && board.(i).(j) == bd_piece_of p in
      if valid then
        if List.mem (i, j) catch.marked_pieces then do {
          catch.moving_piece := (i, j);
          catch.moving_to := (i, j)
        }
        else do {
          List.iter (draw_piece wid xinf) catch.marked_pieces;
          catch.marked_pieces := [(i, j)];
          mark wid xinf.mark_col (i, j);
          set_state wid xinf (Running p)
        }
      else do {
        List.iter (draw_piece wid xinf) catch.marked_pieces;
        catch.marked_pieces := [];
        set_state wid xinf (Running p)
      }
  | Editing _ ->
      let (i, j) = pos_of_xy x y in
      if in_board i j && board.(i).(j) <> Bd_garb then
        match but with
        [ 1 -> do { board.(i).(j) := bd_black; draw_piece wid xinf (i, j) }
        | 2 -> do { board.(i).(j) := Bd_empty; erase_piece wid xinf (i, j) }
        | 3 -> do { board.(i).(j) := bd_white; draw_piece wid xinf (i, j) }
        | _ -> () ]
      else
        for k = 0 to 1 do {
          if x >= lost_xmin k && x < lost_xmax k && y >= lost_ymin k &&
             y < lost_ymax k
          then
            match but with
            [ 1 ->
                if cnt.(k) > 0 then do {
                  unloose_piece wid xinf (index_player k);
                  cnt.(k) := cnt.(k) - 1
                }
                else ()
            | 3 ->
                if cnt.(k) < to_kill - 1 then do {
                  cnt.(k) := cnt.(k) + 1;
                  loose_piece wid xinf (index_player k)
                }
                else ()
            | _ -> () ]
          else ();
        }
  | _ -> () ]
};

value same_player (i, j) =
  dorec where rec dorec =
    fun
    [ [] -> True
    | [(i', j') :: l] -> board.(i).(j) == board.(i').(j') && dorec l ]
;

value button_motion xinf wid (x, y) =
  let _ = Random.int 2 in
  match state.val with
  [ Running p ->
      let (i, j) = pos_of_xy x y in
      let valid =
        in_board i j && board.(i).(j) == bd_piece_of p &&
        List.length catch.marked_pieces < 3
      in
      if valid && not (List.mem (i, j) catch.marked_pieces) &&
         are_aligned (i, j) catch.marked_pieces &&
         same_player (i, j) catch.marked_pieces
      then do {
        catch.marked_pieces := [(i, j) :: catch.marked_pieces];
        mark wid xinf.mark_col (i, j)
      }
      else ()
  | PiecesMarked p ->
      let (i, j) = pos_of_xy x y in
      if are_neighbours (i, j) catch.moving_piece then
        if (i, j) <> catch.moving_to then do {
          draw_dir wid xinf catch.moving_piece (i, j);
          catch.moving_to := (i, j)
        }
        else ()
      else if catch.moving_to <> catch.moving_piece then do {
        draw_piece wid xinf catch.moving_piece;
        mark wid xinf.mark_col catch.moving_piece;
        catch.moving_to := catch.moving_piece
      }
      else ()
  | _ -> () ]
;

value computer_player = ref (None : option player);

value computer_play xinf wid p = do {
  rt_select_mouse wid xinf.watch_mouse;
  rt_flush xinf.xd;
  match alpha_beta 0 p (possible_moves p) with
  [ [move :: _] as moves ->
      let len = List.length moves in
      let move = nth (Random.int len) moves in
      xinf.gf.move_fun p move
        (draw_piece wid xinf, erase_piece wid xinf, loose_piece wid xinf,
         set_state wid xinf)
  | _ -> () ];
  rt_unselect_mouse wid
};

value computer_play_if_required xinf wid =
  match (computer_player.val, state.val) with
  [ (Some p, Running p') -> if p == p' then computer_play xinf wid p else ()
  | _ -> () ]
;

value button_released xinf wid =
  match state.val with
  [ Running p ->
      if catch.marked_pieces <> [] then set_state wid xinf (PiecesMarked p)
      else ()
  | PiecesMarked p -> do {
      set_state wid xinf (Running p);
      List.iter (draw_piece wid xinf) catch.marked_pieces;
      if catch.moving_piece <> catch.moving_to then do {
        let (i, j) = catch.moving_piece
        and (i', j') = catch.moving_to in
        let di = i' - i
        and dj = j' - j in
        match moved_pieces_and_count catch.marked_pieces di dj with
        [ Some (pieces, nb_out) -> do {
            xinf.gf.move_fun p (pieces, di, dj, nb_out)
              (draw_piece wid xinf, erase_piece wid xinf,
               loose_piece wid xinf, set_state wid xinf);
            computer_play_if_required xinf wid
          }
        | None -> () ];
        catch.marked_pieces := []
      }
      else ()
    }
  | _ -> () ]
;

value restore_fun xinf wid = do {
  stop_intermediate_command wid xinf;
  if match state.val with
     [ Terminated | Running _ ->
         move_history.val == [] && undo_history.val == []
     | _ -> False ]
  then do {
    try do {
      let ic = open_in "abalone.save" in
      try do {
        let b = input_value ic in
        for i = 0 to 2 * side do {
          for j = 0 to 2 * side do {
            match b.(i).(j) with
            [ Bd_piece p -> board.(i).(j) := bd_piece_of p
            | x -> board.(i).(j) := x ];
          };
        };
        let c = input_value ic in
        for i = 0 to 1 do { cnt.(i) := c.(i) };
        state.val := input_value ic;
        move_history.val := input_value ic;
        undo_history.val := input_value ic
      }
      with ex -> do { close_in ic; raise ex };
      close_in ic;
      prerr_endline "<game restored>"
    }
    with
    [ Sys_error t -> do { prerr_string "Sys_error: "; prerr_endline t }
    | End_of_file -> prerr_endline "Error: end of file"
    | Failure s -> do { prerr_string "Failed: "; prerr_endline s } ];
    rt_clear_widget wid;
    expose_bd xinf wid (0, 0, bd_wid.val, bd_hei.val)
  }
  else prerr_endline "restore only if no game running"
};

value save_fun xinf wid = do {
  stop_intermediate_command wid xinf;
  let oc = open_out "abalone.save" in
  output_value oc board;
  output_value oc cnt;
  output_value oc state.val;
  output_value oc move_history.val;
  output_value oc undo_history.val;
  close_out oc;
  prerr_endline "<game saved>"
};

value new_game_fun xinf wid = do {
  let new_game () = do {
    xinf.gf.init_fun ();
    rt_clear_widget wid;
    expose_bd xinf wid (0, 0, bd_wid.val, bd_hei.val)
  }
  in
  stop_intermediate_command wid xinf;
  match state.val with
  [ Terminated -> new_game ()
  | Running _ ->
      if move_history.val == [] && undo_history.val == [] then new_game ()
      else do {
        rt_map_widget (unfreeze xinf.new_game_ask);
        set_state wid xinf (Wait_confirm new_game state.val)
      }
  | _ -> () ]
};

value quit_fun xinf wid = do {
  let quit () = rt_stop_main_loop xinf.xa in
  stop_intermediate_command wid xinf;
  match state.val with
  [ Terminated -> quit ()
  | Running _ ->
      if move_history.val == [] && undo_history.val == [] then quit ()
      else do {
        rt_map_widget (unfreeze xinf.quit_ask);
        set_state wid xinf (Wait_confirm quit state.val)
      }
  | _ -> () ]
};

value undo_fun xinf wid = do {
  stop_intermediate_command wid xinf;
  match state.val with
  [ Running _ | Terminated ->
      xinf.gf.undo_fun
        (draw_piece wid xinf, erase_piece wid xinf, unloose_piece wid xinf,
         set_state wid xinf)
  | _ -> () ]
};

value redo_fun xinf wid = do {
  stop_intermediate_command wid xinf;
  match state.val with
  [ Running _ ->
      xinf.gf.redo_fun
        (draw_piece wid xinf, erase_piece wid xinf, loose_piece wid xinf,
         set_state wid xinf)
  | _ -> () ]
};

value show_all_moves_fun xinf wid = do {
  stop_intermediate_command wid xinf;
  match state.val with
  [ Running p ->
      match possible_moves p with
      [ [move :: _] as moves -> do {
          let len = List.length moves in
          Printf.printf "%d possible moves\n" len;
          flush stdout;
          display_move wid xinf p move;
          rt_map_widget (widget_named xinf.xd "next_cancel");
          set_state wid xinf (Tracing p moves)
        }
      | _ -> () ]
  | _ -> () ]
};

value show_best_moves_fun xinf wid = do {
  stop_intermediate_command wid xinf;
  match state.val with
  [ Running p ->
      match alpha_beta 0 p (possible_moves p) with
      [ [move :: _] as moves -> do {
          let len = List.length moves in
          Printf.printf "%d possible moves\n" len;
          flush stdout;
          display_move wid xinf p move;
          rt_map_widget (widget_named xinf.xd "next_cancel");
          set_state wid xinf (Tracing p moves)
        }
      | _ -> () ]
  | _ -> () ]
};

value invert_board_fun xinf wid = do {
  inverted.val := not inverted.val;
  rt_clear_widget wid;
  expose_bd xinf wid (0, 0, widget_width wid, widget_height wid)
};

value next_fun xinf wid =
  match state.val with
  [ Tracing p [move :: ([move' :: _] as moves)] -> do {
      undisplay_move wid xinf p move;
      display_move wid xinf p move';
      set_state wid xinf (Tracing p moves);
      match moves with
      [ [_] -> rt_freeze_widget (widget_named xinf.xd "next")
      | _ -> () ]
    }
  | Tracing p [move] -> do {
      cancel_fun xinf wid;
      prerr_endline "no more move"
    }
  | _ -> () ]
;

value computer_plays xinf wid = do {
  stop_intermediate_command wid xinf;
  match state.val with
  [ Running p -> do {
      computer_player.val := Some p;
      computer_play xinf wid p
    }
  | _ -> () ]
};

value next_move xinf wid =
  match state.val with
  [ Playing p move -> do {
      undisplay_move wid xinf p move;
      xinf.gf.move_fun p move
        (draw_piece wid xinf, erase_piece wid xinf, loose_piece wid xinf,
         set_state wid xinf)
    }
  | _ -> next_fun xinf wid ]
;

value key_pressed xinf wid km =
  let str = string_of_keysym km in
  match str with
  [ "c" -> computer_plays xinf wid
  | "q" -> quit_fun xinf wid
  | "n" ->
      match state.val with
      [ Wait_confirm _ s -> do {
          set_state wid xinf s;
          rt_unmap_widget (unfreeze xinf.new_game_ask);
          rt_unmap_widget (unfreeze xinf.quit_ask)
        }
      | _ -> new_game_fun xinf wid ]
  | "S" -> save_fun xinf wid
  | "R" -> restore_fun xinf wid
  | "y" -> do {
      stop_intermediate_command wid xinf;
      match state.val with
      [ Wait_confirm f s -> do {
          set_state wid xinf s;
          rt_unmap_widget (unfreeze xinf.new_game_ask);
          rt_unmap_widget (unfreeze xinf.quit_ask);
          f ()
        }
      | _ -> () ]
    }
  | "m" -> show_all_moves_fun xinf wid
  | "z" -> do {
      stop_intermediate_command wid xinf;
      match state.val with
      [ Editing _ ->
          for i = 0 to 2 * side do {
            for j = 0 to 2 * side do {
              match board.(i).(j) with
              [ Bd_piece _ -> do {
                  board.(i).(j) := Bd_empty;
                  erase_piece wid xinf (i, j)
                }
              | _ -> () ];
            };
          }
      | _ -> () ]
    }
  | "e" -> do {
      stop_intermediate_command wid xinf;
      match state.val with
      [ Running p -> do {
          move_history.val := [];
          undo_history.val := [];
          set_state wid xinf (Editing p)
        }
      | Editing p -> set_state wid xinf (Running p)
      | _ -> () ]
    }
  | "0" | "1" | "2" | "3" | "4" as lev -> do {
      stop_intermediate_command wid xinf;
      match state.val with
      [ Running p -> do {
          rt_select_mouse wid xinf.watch_mouse;
          rt_flush xinf.xd;
          try
            match alpha_beta (int_of_string lev) p (possible_moves p) with
            [ [move :: _] as moves -> do {
                let len = List.length moves in
                Printf.printf "%d possible moves\n" len;
                flush stdout;
                set_state wid xinf (SelectedMoves p moves)
              }
            | _ -> () ]
          with x -> do { rt_unselect_mouse wid; raise x };
          rt_unselect_mouse wid
        }
      | _ -> () ]
    }
  | "t" ->
      match state.val with
      [ SelectedMoves p moves ->
          match moves with
          [ [move :: _] -> do {
              display_move wid xinf p move;
              set_state wid xinf (Tracing p moves)
            }
          | _ -> set_state wid xinf (Running p) ]
      | _ -> stop_intermediate_command wid xinf ]
  | "p" ->
      match state.val with
      [ SelectedMoves p moves ->
          match moves with
          [ [move :: _] -> do {
              let move = nth (Random.int (List.length moves)) moves in
              display_move wid xinf p move;
              set_state wid xinf (Playing p move)
            }
          | _ -> set_state wid xinf (Running p) ]
      | _ -> stop_intermediate_command wid xinf ]
  | "a" -> do {
      stop_intermediate_command wid xinf;
      let rec tmout_fun () =
        match state.val with
        [ Running p -> do {
            match alpha_beta 0 p (possible_moves p) with
            [ [_ :: _] as moves ->
                let move = nth (Random.int (List.length moves)) moves in
                xinf.gf.move_fun p move
                  (draw_piece wid xinf, erase_piece wid xinf,
                   loose_piece wid xinf, set_state wid xinf)
            | _ -> set_state wid xinf (Running (opponent p)) ];
            rt_set_timeout xinf.xa "" (rt_current_time xinf.xa) tmout_fun
          }
        | _ -> () ]
      in
      rt_set_timeout xinf.xa "" (rt_current_time xinf.xa) tmout_fun
    }
  | "u" -> undo_fun xinf wid
  | "r" -> redo_fun xinf wid
  | " " -> next_move xinf wid
  | _ -> stop_intermediate_command wid xinf ]
;

value display_help wid txt = do {
  term_send wid "\027[?35h";
  term_send wid "\027[20h";
  term_send wid txt
};

value date_in_sec =
  let num =
    num 0 where rec num n (strm__ : Istream.t _) =
      match Istream.peek strm__ with
      [ Some ('0'..'9' as c) -> do {
          Istream.junk strm__;
          try num (10 * n + Char.code c - Char.code '0') strm__ with
          [ Istream.Failure -> raise (Istream.Error "") ]
        }
      | Some _ -> do { Istream.junk strm__; n }
      | _ -> raise Istream.Failure ]
  in
  let rec word (strm__ : Istream.t _) =
    match Istream.peek strm__ with
    [ Some ' ' -> do { Istream.junk strm__; "" }
    | Some x -> do {
        Istream.junk strm__;
        let w =
          try word strm__ with [ Istream.Failure -> raise (Istream.Error "") ]
        in
        String.make 1 x ^ w
      }
    | _ -> raise Istream.Failure ]
  in
  fun (strm__ : Istream.t _) ->
    let _ = word strm__ in
    let _ =
      try word strm__ with [ Istream.Failure -> raise (Istream.Error "") ]
    in
    let day =
      try num strm__ with [ Istream.Failure -> raise (Istream.Error "") ]
    in
    let hour =
      try num strm__ with [ Istream.Failure -> raise (Istream.Error "") ]
    in
    let min =
      try num strm__ with [ Istream.Failure -> raise (Istream.Error "") ]
    in
    let sec =
      try num strm__ with [ Istream.Failure -> raise (Istream.Error "") ]
    in
    ((day * 24 + hour) * 60 + min) * 60 + sec
;

value set_random_seed date =
  try Random.init (date_in_sec (Istream.of_string date)) with
  [ Istream.Failure | Istream.Error _ -> () ]
;

value usage () = do {
  prerr_endline "\
Usage:
\tabalone [-bw] [-display dname] [-date \"`date`\"]

   -bw\t\t\tblack & white
   -display dname\tX server to contact
   -date \"`date`\"\tto init the random seed
";
  failwith "error in option"
};

value get_args argv = do {
  let i = ref 1 in
  let next_arg () = do {
    incr i;
    if i.val < Array.length argv then argv.(i.val) else usage ()
  }
  and color = ref True
  and dname = ref "" in
  while i.val < Array.length argv do {
    match argv.(i.val) with
    [ "-bw" -> color.val := False
    | "-display" -> dname.val := next_arg ()
    | "-date" -> set_random_seed (next_arg ())
    | "-d" ->
        let _ = next_arg () in
        ()
    | _ -> usage () ];
    incr i
  };
  (color.val, dname.val)
};

value pack1_desc attr args = pack_desc attr args (fun _ _ -> ());
value button1_desc attr txt = button_desc attr (txt, None);

value popup_desc attr (xd, b, sfl) =
  let popup_wid =
    rt_create_popup_widget xd
      (pack1_desc []
         (Vertical,
          List.map
            (fun (s, f) ->
               button1_desc [LeftJustifAtt; BorderAtt 0] s
                 (fun _ ->
                    fun
                    [ ButtonEvRelease _ _ _ -> f ()
                    | _ -> () ]))
            sfl))
  in
  button1_desc attr b
    (fun _ ->
       fun
       [ ButtonEvPress xll yll _ -> rt_map_popup_widget popup_wid xll yll 0
       | ButtonEvEnter xll yll -> rt_map_popup_widget popup_wid xll yll 0
       | _ -> () ])
;

value menu_desc attr menu_items =
  pack1_desc attr
    (Horizontal,
     menu_items @
     [pack1_desc (attr @ [BorderAtt 0; FillerAtt]) (Vertical, [])])
;

value button_action act _ =
  fun
  [ ButtonEvRelease _ _ _ -> act ()
  | _ -> () ]
;

value help_text =
  "The aim is to push " ^ string_of_int to_kill ^ " \
pieces of the opponent out of the board.

You can move 1, 2 or 3 contiguous pieces a any direction, if it is
free or it pushes less opponent's pieces.

To play one move:

  1) select 1 2 or 3 pieces to move by dragging the mouse button;
     selected pieces receive a red point in their center.
  2) select a direction by dragging the mouse button from one of the
     pieces selected to the desired direction.

  A move supposes to push and release the mouse button twice.
  To cancel the phase 1), click once elsewhere.

Commands:
\t0..4\tfind all moves at this level
                  (interrupt with ^c in xterm)
        p\tcomputer shows a random move selected by 0..4
                  (space to make it play this move)
        t\ttrace all moves selected by 0..4
        space   show next move

\tc\tcomputer becomes the current player
\ta\tcomputer plays alone (stop with return)

\te\tedit board

When editing:
\tz\tclear board
        e\texit editing
        mouse button\tin board\tin lost pieces regions
            1\t\tset a black\t     remove piece
            2\t\tempty case
            3\t\tset a white\t     add piece"
;

value help_nlines =
  loop 1 0 where rec loop n i =
    if i == String.length help_text then n
    else
      let n =
        match help_text.[i] with
        [ '\n' -> n + 1
        | _ -> n ]
      in
      loop n (i + 1)
;

value window_outl = 68;

value adjust_const xd =
  let hs =
    2 * (screen_height xd - window_outl - 2 * bd_outl.val - bd_low.val) /
    ((2 * side + 1) * 3 + 5)
  in
  let hw = side2widt hs in
  let pd = hw - 2 * hex_outl.val - hex_bord.val in
  if pd < piec_diam.val then do { piec_diam.val := pd; init_const () } else ()
;

value xabalone gf argv = do {
  init_catch ();
  let (color, dname) = get_args argv in
  let xd = rt_initialize dname in
  adjust_const xd;
  try do {
    let xa = rt_args [xd]
    and colored = is_colored xd && color
    and ask_wid txt =
      delay
        (fun () -> do {
           let parent = widget_named xd "abalone" in
           let wid =
             rt_create_subwidget parent 0 0
               (title_desc [NameAtt txt] txt (fun _ _ -> ()))
           in
           rt_move_widget wid ((widget_width parent - widget_width wid) / 2)
             ((widget_height parent - widget_height wid) / 2);
           wid
         })
    in
    let select_white =
      let white = rt_white_color xd in
      fun _ -> rt_select_color white
    and select_black =
      let black = rt_black_color xd in
      fun _ -> rt_select_color black
    and backg =
      if colored then rt_create_color xd (34, 139, 34) else rt_white_color xd
    in
    let select_backg _ = rt_select_color backg
    and skyblue =
      if colored then
        let col = rt_create_color xd (216, 249, 255) in
        fun _ -> rt_select_color col
      else fun () -> failwith "skyblue"
    and select_sand =
      if colored then fun () -> failwith "select_sand"
      else
        let patt =
          rt_create_pattern xd "\
\001\001\008\008@@\004\004\128\128\016\016\002\002  \001\001\008\008@@\004\004\128\128\016\016\002\002  "
            16 16
        in
        fun _ -> do { select_black (); rt_select_pattern patt (0, 0) }
    in
    let xinf =
      {xd = xd; xa = xa; watch_mouse = rt_create_font_mouse xd xC_watch;
       backg_col = select_backg;
       line_col =
         if colored then rt_create_color xd (255, 180, 0)
         else rt_black_color xd;
       garb_col =
         if colored then rt_create_color xd (165, 250, 255)
         else rt_white_color xd;
       piece_col =
         if colored then [| select_black; skyblue |]
         else [| select_black; select_sand |];
       piece_bd_col =
         if colored then [| select_black; skyblue |]
         else [| select_black; select_black |];
       mark_col =
         if colored then
           let col = rt_create_color xd (255, 0, 0) in
           let f _ = rt_select_color col in
           [| f; f |]
         else [| select_white; select_black |];
       quit_ask = ask_wid " really quit (y/n) ? ";
       new_game_ask = ask_wid " abort current game (y/n) ? "; gf = gf}
    in
    let rec with_wid f () = f xinf (widget_named xd "abalone") in
    let wid =
      rt_create_widget xd "abalone" "abalone" AutoPosition
        (Some (quit_fun xinf))
        (pack1_desc []
           (InDepth,
            [pack1_desc
               [NameAtt "board"; BandAtt 0; InterAtt 1;
                BackgroundAtt (ColorPn (rt_black_color xd))]
               (Vertical,
                [menu_desc [BorderAtt 0]
                   [popup_desc []
                      (xd, "Game",
                       [("New Game (n)", with_wid new_game_fun);
                        ("Computer plays (c)", with_wid computer_plays);
                        ("Undo (u)", with_wid undo_fun);
                        ("Redo (r)", with_wid redo_fun);
                        ("Invert Board", with_wid invert_board_fun);
                        ("Quit Abalone (q)", with_wid quit_fun)]);
                    popup_desc []
                      (xd, "File",
                       [("Restore Game (R)", with_wid restore_fun);
                        ("Save Game (S)", with_wid save_fun)]);
                    popup_desc []
                      (xd, "Moves",
                       [("Show All Moves (m)", with_wid show_all_moves_fun);
                        ("Show Computer Moves", with_wid show_best_moves_fun);
                        ("Next Move (space)", with_wid next_move)]);
                    button1_desc [] "Help"
                      (button_action
                         (fun () ->
                            rt_raise_widget (widget_named xd "help")))];
                 raw_desc
                   [BackgroundAtt (ColorPn backg); NameAtt "abalone";
                    FillerAtt]
                   (bd_wid.val, bd_hei.val, 0,
                    [SelExposure; SelButtonPress; SelButtonMotion;
                     SelButtonRelease; SelKeyPress])
                   (fun wid ->
                      fun
                      [ RawEvExpose x0 x1 x2 x3 ->
                          expose_bd xinf wid (x0, x1, x2, x3)
                      | RawEvButtonPress x0 x1 x2 x3 x4 ->
                          button_pressed xinf wid (x0, x1, x2, x3, x4)
                      | RawEvMotionNotify x0 x1 ->
                          button_motion xinf wid (x0, x1)
                      | RawEvButtonRelease x0 x1 x2 x3 x4 ->
                          button_released xinf wid
                      | RawEvKeyPress a -> key_pressed xinf wid a
                      | _ -> () ])]);
             pack1_desc [NameAtt "help"]
               (Vertical,
                [menu_desc [BorderAtt 0]
                   [button1_desc [] "Board"
                      (button_action
                         (fun () ->
                            rt_raise_widget (widget_named xd "board")))];
                 term_desc [NameAtt "help_term"; FillerAtt]
                   (help_nlines, 60, 0) (fun _ _ -> ())])]))
    in
    let _ =
      rt_create_subwidget wid (bd_outl.val + bd_ins_wid.val + bd_right.val)
        (bd_outl.val + bd_ins_hei.val + bd_low.val)
        (pack1_desc
           [BackgroundAtt (ColorPn backg); BorderAtt 0; NameAtt "next_cancel"]
           (Vertical,
            [button1_desc [NameAtt "next"] "next"
               (button_action (with_wid next_fun));
             button1_desc [] "cancel" (button_action (with_wid cancel_fun))]))
    in
    display_help (widget_named xd "help_term") help_text;
    rt_map_widget wid;
    rt_main_loop xa
  }
  with x -> do { rt_end xd; raise x };
  rt_end xd
};
