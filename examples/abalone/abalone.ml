(* $Id: abalone.ml,v 1.5 2007/07/05 17:52:42 deraugla Exp $ *)

open Common;

value no_more_pieces p =
  loop 0 0 where rec loop i j =
    if i = Array.length board then True
    else if j = Array.length board.(i) then loop (i + 1) 0
    else if board.(i).(j) = p then False
    else loop i (j + 1)
;

value move_piece di dj (draw, erase, lost, set_state) (i, j) = do {
  let who = board.(i).(j) in
  board.(i).(j) := Bd_empty;
  erase (i, j);
  let (i, j) = (i + di, j + dj) in
  if board.(i).(j) == Bd_garb then do {
    let pwho = player_of who in
    let k = player_index pwho in
    cnt.(k) := cnt.(k) + 1;
    if cnt.(k) >= to_kill then set_state Terminated
    else if no_more_pieces who then set_state Terminated
    else ();
    lost pwho
  }
  else do { board.(i).(j) := who; draw (i, j) }
};

value unmove_piece di dj (draw, erase, unlost, set_state) play nb_out
    (oi, oj) = do {
  let (i, j) = (oi + di, oj + dj) in
  let play =
    match board.(i).(j) with
    [ Bd_garb -> do {
        let looser = if nb_out > 0 then opponent play else play in
        let k = player_index looser in
        unlost looser;
        cnt.(k) := cnt.(k) - 1;
        looser
      }
    | _ -> do {
        let play = player_of board.(i).(j) in
        board.(i).(j) := Bd_empty;
        erase (i, j);
        play
      } ]
  in
  board.(oi).(oj) := bd_piece_of play;
  draw (oi, oj)
};

value move_fun play (pieces, di, dj, nb_out)
    ((_, _, _, set_state) as funs) = do {
  set_state (Running (opponent play));
  List.iter (move_piece di dj funs) pieces;
  move_history.val := [(play, pieces, di, dj, nb_out) :: move_history.val];
  undo_history.val := []
};

value undo_fun ((_, _, _, set_state) as funs) =
  match move_history.val with
  [ [((play, pieces, di, dj, nb_out) as move) :: h] -> do {
      List.iter (unmove_piece di dj funs play nb_out) (List.rev pieces);
      move_history.val := h;
      undo_history.val := [move :: undo_history.val];
      set_state (Running play)
    }
  | [] -> () ]
;

value redo_fun ((_, _, _, set_state) as funs) =
  match undo_history.val with
  [ [((play, pieces, di, dj, nb_out) as move) :: h] -> do {
      set_state (Running (opponent play));
      List.iter (move_piece di dj funs) pieces;
      undo_history.val := h;
      move_history.val := [move :: move_history.val]
    }
  | [] -> () ]
;

value init_game_fun () = do {
  for i = 0 to 2 * side do {
    for j = 0 to 2 * side do {
      match board.(i).(j) with
      [ Bd_piece _ -> board.(i).(j) := Bd_empty
      | _ -> () ];
    };
  };
  for j = 1 to side do { board.(1).(j) := bd_black };
  for j = 1 to side + 1 do { board.(2).(j) := bd_black };
  for j = 3 to side do { board.(3).(j) := bd_black };
  for j = 1 to side do { board.(2 * side - 1).(j + side - 1) := bd_white };
  for j = 1 to side + 1 do {
    board.(2 * side - 2).(j + side - 2) := bd_white;
  };
  for j = 3 to side do { board.(2 * side - 3).(j + side - 3) := bd_white };
  cnt.(0) := 0;
  cnt.(1) := 0;
  move_history.val := [];
  undo_history.val := [];
  state.val := Running Pl_white
};

value abalone graphic_interface argv = do {
  init_game_fun ();
  graphic_interface
    {init_fun = init_game_fun; move_fun = move_fun; undo_fun = undo_fun;
     redo_fun = redo_fun}
    argv
};
