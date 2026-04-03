(* $Id: computer.ml,v 1.5 2007/07/05 17:52:42 deraugla Exp $ *)

open Common;

value dir = [| (-1, -1); (-1, 0); (0, -1); (0, 1); (1, 0); (1, 1) |];

value possible_moves fri = do {
  let bd_fri = bd_piece_of fri in
  let moves = ref [] in
  let try_add_move pieces (di, dj) =
    match moved_pieces_and_count pieces di dj with
    [ Some (pieces, nb_out) ->
        moves.val := [(pieces, di, dj, nb_out) :: moves.val]
    | None -> () ]
  in
  let pieces = ref [] in
  for i = 0 to 2 * side do {
    for j = 0 to 2 * side do {
      if board.(i).(j) == bd_fri then do {
        pieces.val := [(i, j)];
        for k = 0 to 5 do { try_add_move pieces.val dir.(k) };
        for k = 3 to 5 do {
          let (di, dj) = dir.(k) in
          let (i', j') = (i + di, j + dj) in
          let (i'', j'') = (i' + di, j' + dj) in
          if board.(i').(j') == bd_fri then do {
            pieces.val := [(i, j); (i', j')];
            for d = 0 to 5 do {
              if d <> k && d <> 5 - k then try_add_move pieces.val dir.(d)
              else ();
            };
            if board.(i'').(j'') == bd_fri then do {
              pieces.val := [(i, j); (i', j'); (i'', j'')];
              for d = 0 to 5 do {
                if d <> k && d <> 5 - k then try_add_move pieces.val dir.(d)
                else ();
              }
            }
            else ()
          }
          else ();
        };
        for k = 0 to 5 do {
          let (di, dj) = dir.(k) in
          let (i', j') = (i + di, j + dj) in
          let (i'', j'') = (i' + di, j' + dj) in
          if board.(i').(j') == bd_fri then do {
            pieces.val := [(i, j); (i', j')];
            try_add_move pieces.val (di, dj);
            if board.(i'').(j'') == bd_fri then do {
              pieces.val := [(i, j); (i', j'); (i'', j'')];
              try_add_move pieces.val (di, dj)
            }
            else ()
          }
          else ();
        }
      }
      else ();
    };
  };
  List.rev moves.val
};

value vdir = Array.to_list dir;

value nb_adj_border (i, j) =
  if board.(i).(j) == Bd_garb then 6
  else
    List.fold_left
      (fun n (di, dj) ->
         if board.(i + di).(j + dj) == Bd_garb then n + 1 else n)
      0 vdir
;

value fast_move_piece di dj (i, j) = do {
  let who = board.(i).(j) in
  board.(i).(j) := Bd_empty;
  let (i, j) = (i + di, j + dj) in
  if board.(i).(j) == Bd_garb then
    let pwho = player_of who in
    let k = player_index pwho in
    cnt.(k) := cnt.(k) + 1
  else board.(i).(j) := who
};

value fast_unmove_piece di dj play nb_out (oi, oj) =
  let (i, j) = (oi + di, oj + dj) in
  let play =
    match board.(i).(j) with
    [ Bd_garb -> do {
        let looser = if nb_out > 0 then opponent play else play in
        let k = player_index looser in
        cnt.(k) := cnt.(k) - 1;
        looser
      }
    | _ -> do {
        let play = player_of board.(i).(j) in
        board.(i).(j) := Bd_empty;
        play
      } ]
  in
  board.(oi).(oj) := bd_piece_of play
;

value fast_move_fun play (pieces, di, dj, nb_out) =
  List.iter (fast_move_piece di dj) pieces
;

value fast_undo_fun play (pieces, di, dj, nb_out) =
  List.iter (fast_unmove_piece di dj play nb_out) (List.rev pieces)
;

value infinity = 10000;
value lev_max = 100;
value win_value = infinity - lev_max;

value union l1 l2 =
  union_rec l1 where rec union_rec =
    fun
    [ [] -> l2
    | [a :: l] -> if List.mem a l2 then union_rec l else [a :: union_rec l] ]
;

value get_conc p moves =
  let bd_me = bd_piece_of p in
  List.fold_left
    (fun p0 p1 ->
       match (p0, p1) with
       [ (concp, (pieces, _, _, 1)) ->
           List.fold_left
             (fun (fri_conc, opp_conc) ((i, j) as ij) ->
                if board.(i).(j) == bd_me then (union [ij] fri_conc, opp_conc)
                else (fri_conc, union [ij] opp_conc))
             concp pieces
       | (concp, _) -> concp ])
    ([], []) moves
;

type defense =
  [ Dval of int
  | Dborder_attack
  | Dborder ]
;

value defensive_value p ((pieces, _, _, nb_out) as move) concp =
  if nb_out < 0 then Dborder
  else
    let (fri_conc, opp_conc) = unfreeze concp in
    let len = List.length fri_conc in
    if len == 0 then Dborder
    else if
      List.exists (fun ij -> List.mem ij fri_conc || List.mem ij opp_conc)
        pieces
    then do {
      let move_opp =
        let (i1, j1) = List.hd pieces in
        board.(i1).(j1) != bd_piece_of p
      in
      fast_move_fun p move;
      let opp = opponent p in
      let (fri_conc', _) = get_conc p (possible_moves opp) in
      fast_undo_fun p move;
      let len' = List.length fri_conc' in
      let val0 = len - len' in
      if val0 > 0 then Dval (2 * val0 + (if move_opp then 1 else 0))
      else Dborder_attack
    }
    else Dborder
;

value distance_from_border (i, j) =
  List.fold_left min (2 * side)
    [i; 2 * side - i; if i <= side then j else j - i + side;
     if i <= side then side + i - j else 2 * side - j]
;

value dist_value =
  let v = [| 15; 5; 1 |] in
  fun i -> if i < Array.length v then v.(i) else 0
;

value border_value play (pieces, di, dj, _) =
  let bd_play = bd_piece_of play in
  let n1 =
    List.fold_left
      (fun n (i, j) ->
         let v = dist_value (distance_from_border (i, j)) in
         if board.(i).(j) == bd_play then n - v else n + v)
      0 pieces
  and n2 =
    List.fold_left
      (fun n (i, j) ->
         let v = dist_value (distance_from_border (i + di, j + dj)) in
         if board.(i).(j) == bd_play then n - v else n + v)
      0 pieces
  in
  n2 - n1
;

value border_attack_value play (pieces, di, dj, _) =
  let bd_play = bd_piece_of play in
  let n1 =
    List.fold_left
      (fun n (i, j) ->
         if board.(i).(j) == bd_play then n else n + nb_adj_border (i, j))
      0 pieces
  and n2 =
    List.fold_left
      (fun n (i, j) ->
         if board.(i).(j) == bd_play then n
         else n + nb_adj_border (i + di, j + dj))
      0 pieces
  in
  let val0 = n2 - n1 in
  if val0 == 0 then -1 else val0
;

value eval_pos main_p lev val_before_move play ((_, _, _, nb_out) as move)
    concp =
  let (val0, dval) =
    let play_k = player_index play in
    if cnt.(1 - play_k) >= to_kill then (0, infinity)
    else if cnt.(play_k) >= to_kill then (0, -infinity)
    else if nb_out > 0 && cnt.(1 - play_k) + nb_out >= to_kill then
      (0, win_value + lev)
    else if nb_out < 0 && cnt.(play_k) - nb_out >= to_kill then
      (0, -win_value - lev)
    else
      (val_before_move,
       match defensive_value play move concp with
       [ Dval dval -> 100 * dval
       | Dborder_attack -> 10 * border_attack_value play move
       | Dborder -> border_value play move ])
  in
  val0 + (if play == main_p then dval else -dval)
;

value suicide (pieces, di, dj, nb_out) = nb_out < 0;

value saved_board = Array.make_matrix (2 * side + 1) (2 * side + 1) Bd_ext;
value saved_cnt = Array.make 2 0;

value alpha_beta lev main_p moves = do {
  let rec maximize p lev move threshold val_before_move concp =
    let val0 = eval_pos main_p lev val_before_move p move concp in
    if lev == 0 || val0 >= win_value || val0 <= -win_value then val0
    else do {
      fast_move_fun p move;
      let opp = opponent p in
      let moves = possible_moves opp in
      let concp = delay (fun _ -> get_conc p moves) in
      let (max_val, _) =
        List.fold_left
          (fun ((max_val, go_on) as r) move ->
             if go_on then
               let val0 = minimize opp (lev - 1) move max_val val0 concp in
               if val0 > max_val then
                 if val0 >= threshold then (val0, False) else (val0, True)
               else (max_val, True)
             else r)
          (-infinity, True) moves
      in
      fast_undo_fun p move;
      max_val
    }
  and minimize p lev move threshold val_before_move concp =
    let val0 = eval_pos main_p lev val_before_move p move concp in
    if lev == 0 || val0 >= win_value || val0 <= -win_value then val0
    else do {
      fast_move_fun p move;
      let opp = opponent p in
      let moves = possible_moves opp in
      let concp = delay (fun _ -> get_conc p moves) in
      let (min_val, _) =
        List.fold_left
          (fun ((min_val, go_on) as r) move ->
             if go_on then
               let val0 = maximize opp (lev - 1) move min_val val0 concp in
               if val0 < min_val then
                 if val0 <= threshold then (val0, False) else (val0, True)
               else (min_val, True)
             else r)
          (infinity, True) moves
      in
      fast_undo_fun p move;
      min_val
    }
  in
  for i = 0 to 2 * side do {
    for j = 0 to 2 * side do { saved_board.(i).(j) := board.(i).(j) };
  };
  for i = 0 to 1 do { saved_cnt.(i) := cnt.(i) };
  try
    let concp =
      delay (fun _ -> get_conc main_p (possible_moves (opponent main_p)))
    in
    let (mval, emoves) =
      List.fold_left
        (fun (mval, emoves) move ->
           let val0 = minimize main_p lev move (-infinity) 0 concp in
           (max mval val0, [(val0, move) :: emoves]))
        (-infinity, []) moves
    in
    let moves =
      List.fold_left
        (fun moves (val0, move) ->
           if val0 == mval then [move :: moves] else moves)
        [] emoves
    in
    let moves =
      let nmoves =
        List.fold_right
          (fun move moves -> if suicide move then moves else [move :: moves])
          moves []
      in
      if nmoves = [] then moves else nmoves
    in
    moves
  with
  [ Sys.Break -> do {
      prerr_endline "interrupted";
      for i = 0 to 2 * side do {
        for j = 0 to 2 * side do { board.(i).(j) := saved_board.(i).(j) };
      };
      for i = 0 to 1 do { cnt.(i) := saved_cnt.(i) };
      []
    } ]
};
