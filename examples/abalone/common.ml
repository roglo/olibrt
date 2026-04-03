(* $Id: common.ml,v 1.6 2007/07/05 17:52:42 deraugla Exp $ *)

value side = 5;
value to_kill = 6;

type player = [ Pl_black | Pl_white ];

type bd_hold =
  [ Bd_ext
  | Bd_garb
  | Bd_empty
  | Bd_piece of player ]
;

type move = (list (int * int) * int * int * int);

type state =
  [ Running of player
  | SelectedMoves of player and list move
  | Terminated
  | Tracing of player and list move
  | Playing of player and move
  | PiecesMarked of player
  | Wait_confirm of unit -> unit and state
  | Editing of player ]
;

type display_fun =
  ((int * int) -> unit * (int * int) -> unit * player -> unit * state -> unit)
;

type game_fun =
  { init_fun : unit -> unit;
    move_fun : player -> move -> display_fun -> unit;
    undo_fun : display_fun -> unit;
    redo_fun : display_fun -> unit }
;

type frozen 'a =
  [ Fval of 'a
  | Ffunc of unit -> 'a ]
;

value unfreeze r =
  match r.val with
  [ Fval x -> x
  | Ffunc f -> do {
      let x = f () in
      r.val := Fval x;
      x
    } ]
;

value delay f = ref (Ffunc f);

value bd_white = Bd_piece Pl_white and bd_black = Bd_piece Pl_black;

value cnt = [| 0; 0 |];

value move_history =
  ref ([] : list (player * list (int * int) * int * int * int))
;

value undo_history =
  ref ([] : list (player * list (int * int) * int * int * int))
;

value board = do {
  let board = Array.make_matrix (2 * side + 1) (2 * side + 1) Bd_ext
  and in_board i j =
    if i <= 0 then False
    else if i <= side then j >= 1 && j <= side + i - 1
    else if i <= 2 * side - 1 then j >= i - side + 1 && j <= 2 * side - 1
    else False
  and in_garbage i j =
    if i < 0 then False
    else if i == 0 then j >= 0 && j <= side
    else if i <= side then j == 0 || j == side + i
    else if i <= 2 * side - 1 then j == i - side || j == 2 * side
    else if i == 2 * side then j >= side && j <= 2 * side
    else False
  in
  for i = 0 to 2 * side do {
    for j = 0 to 2 * side do {
      if in_board i j then board.(i).(j) := Bd_empty
      else if in_garbage i j then board.(i).(j) := Bd_garb
      else ();
    };
  };
  board
};

value bd_piece_of =
  fun
  [ Pl_black -> bd_black
  | Pl_white -> bd_white ]
;

value player_of =
  fun
  [ Bd_piece x -> x
  | _ -> invalid_arg "player_of" ]
;

value opponent =
  fun
  [ Pl_black -> Pl_white
  | Pl_white -> Pl_black ]
;

value player_index =
  fun
  [ Pl_black -> 0
  | Pl_white -> 1 ]
;

value index_player =
  fun
  [ 0 -> Pl_black
  | 1 -> Pl_white
  | _ -> invalid_arg "index_player" ]
;

value state = ref Terminated;
value is_side_move pieces di dj =
  match pieces with
  [ [_] -> True
  | [(i, j); (i', j') :: _] ->
      not (i == i' + di && j == j' + dj || i == i' - di && j == j' - dj)
  | [] -> failwith "is_side_move" ]
;

value last_and_invert_list =
  do_rec [] where rec do_rec il =
    fun
    [ [last] -> (last, [last :: il])
    | [x :: l] -> do_rec [x :: il] l
    | [] -> invalid_arg "last_and_invert_list" ]
;

value sort_pieces_in_dir pieces di dj =
  match pieces with
  [ [ij] -> (ij, pieces)
  | [((i, j) as ij); (i', j') :: _] ->
      if i' == i + di && j' = j + dj then last_and_invert_list pieces
      else (ij, pieces)
  | _ -> invalid_arg "sort_pieces_in_dir" ]
;

value moved_pieces_and_count pieces di dj =
  let len = List.length pieces in
  if len > 3 then None
  else if is_side_move pieces di dj then
    let (valid, nb_out) =
      List.fold_left
        (fun (valid, nb_out) (i, j) ->
           if valid then
             let b = board.(i + di).(j + dj) in
             (b == Bd_empty || b == Bd_garb,
              nb_out - (if b == Bd_garb then 1 else 0))
           else (False, 0))
        (True, 0) pieces
    in
    if valid then Some (pieces, nb_out) else None
  else
    let ((i, j), spieces) = sort_pieces_in_dir pieces di dj in
    let opp = bd_piece_of (opponent (player_of board.(i).(j))) in
    let (pieces, len_opp, bd_end) =
      count_rec spieces 0 (i + di) (j + dj) where rec count_rec pieces n i j =
        if board.(i).(j) == opp then
          count_rec [(i, j) :: pieces] (n + 1) (i + di) (j + dj)
        else (pieces, n, board.(i).(j))
    in
    if len_opp < len then
      match bd_end with
      [ Bd_empty -> Some (pieces, 0)
      | Bd_garb -> Some (pieces, if len_opp > 0 then 1 else -1)
      | _ -> None ]
    else None
;

value rec nth p0 p1 =
  match (p0, p1) with
  [ (0, [x :: _]) -> x
  | (n, [_ :: l]) -> nth (n - 1) l
  | (_, _) -> invalid_arg "nth" ]
;
