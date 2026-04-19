(* $Id: welltris.ml,v 1.4 2007/07/05 18:18:26 deraugla Exp $ *)

open Rt;
open Sys;

value tee (f, g) x = (f x, g x);
value max x y = if x > y then x else y
and min x y = if x < y then x else y
and add x y = x + y
and b f g x = f (g x);
value iterate f =
  iterate_f where rec iterate_f n x =
    if n > 0 then iterate_f (pred n) (f x) else ()
;
value it_vect f i v = List.fold_left f i (Array.to_list v)
and \*** x y = x mod y + (if x < 0 then y else 0);
value sigma = List.fold_left add 0
and random n = Random.int n
and init_random () = Random.self_init ()
and implode_ascii l = do {
  let len = List.length l in
  let s = Bytes.create len in
  iterate
    (fun
     [ (i, [hd :: tl]) -> do { Bytes.set s i (Char.chr hd); (i + 1, tl) }
     | _ -> failwith "implode_ascii" ])
    len (0, l);
  Bytes.to_string s
};

value gloop f p =
  looprec where rec looprec x = if p x then x else looprec (f x)
;

type game_data =
  { xd : xdata;
    xargs : xargs;
    pix : pixmap;
    woops : drawable -> game_data -> unit -> unit;
    state : mutable game_state;
    level : mutable int;
    score : mutable int;
    nlines : mutable int;
    explosions_to_do : mutable bool;
    expl_list : mutable array (list int);
    condemn : mutable array int;
    patt : glop;
    runner : runner;
    board : array (array (ref square)) }
and game_state = [ NotStarted | Running | Pausing | Ended | Quit ]
and glop =
  [ C'Pattern of array pattern
  | C'Color of array color ]
and runner =
  { i : mutable int;
    j : mutable int;
    p : mutable int;
    s : mutable int;
    r : mutable int;
    dl : mutable list (int * int) }
and square = [ Square | Empty | Moribund ]
and pieces = { frq : int; dsc : list (int * int) }
and my_patt =
  [ C'WhiteP
  | C'BlackP
  | C'DiagonalP
  | C'SandP
  | C'ChBoardP
  | C'VerticalP
  | C'GrayP ]
;

value welltris_wizard = ref False;

value upperB = 70
and leftB = 10
and lowerB = 10
and rightB = 10;

value xM = 8;
value yM = 8;
value zM = 12;

value eCH = ref 80;

value (speed_tab, nlevels) =
  tee (Array.get, Array.length) [| 1000; 800; 600; 500; 400; 300 |]
;

value change_level = 500;

value iM = 2 * (xM + yM)
and jM = zM + max xM yM
and xZM = zM + xM
and yZM = zM + yM
and l1 = xM
and l2 = xM + yM
and l3 = xM + yM + xM
and l4 = xM + yM + xM + yM;

value w = ref (eCH.val * xM);
value h = ref (eCH.val * yM);

value cXE = ref (eCH.val * (xM / 2));
value cYE = ref (eCH.val * (yM / 2));
value z1E = 520;

value gW = ref (leftB + w.val + 1 + rightB);
value gH = ref (upperB + h.val + 1 + lowerB);
value cONDEMN_COUNT = 60;

value sigma = List.fold_left add 0;

value (pieces, sUMFRQ) =
  tee (Array.get, b (List.fold_left (fun v p -> v + p.frq) 0) Array.to_list)
    [| {frq = 8; dsc = [(0, 0); (1, 0)]};
       {frq = 8; dsc = [(0, 0); (1, 0); (-1, 0)]};
       {frq = 8; dsc = [(0, 0); (1, 0); (0, 1)]};
       {frq = 8; dsc = [(0, 0); (1, 0); (2, 0); (-1, 0)]};
       {frq = 8; dsc = [(0, 0); (1, 0); (1, 1); (-1, 0)]};
       {frq = 8; dsc = [(0, 0); (1, 0); (1, 1); (0, -1)]};
       {frq = 8; dsc = [(0, 0); (1, 0); (1, 1); (0, 1)]};
       {frq = 8; dsc = [(0, 0); (1, 0); (0, 1); (-1, 0)]};
       {frq = 5; dsc = [(0, 0); (0, 1); (1, 1); (-1, 0); (0, -1)]};
       {frq = 5; dsc = [(0, 0); (0, 1); (0, 2); (0, -1); (0, -1)]};
       {frq = 5; dsc = [(0, 0); (1, 0); (2, 0); (0, 1); (0, 2)]};
       {frq = 5; dsc = [(0, 0); (1, 0); (1, 1); (0, -1); (-1, -1)]};
       {frq = 5; dsc = [(0, 0); (1, 0); (-1, 0); (0, 1); (0, -1)]};
       {frq = 5; dsc = [(0, 0); (1, 0); (0, -1); (-1, 0); (-2, 0)]};
       {frq = 5; dsc = [(0, 0); (1, 0); (1, 1); (-1, 0); (-1, -1)]} |]
and rotmat =
  Array.get
    [| Array.get
         [| (1, 0, 0, 1); (0, 1, -1, 0); (-1, 0, 0, -1); (0, -1, 1, 0) |];
       Array.get
         [| (1, 0, 0, -1); (0, 1, 1, 0); (-1, 0, 0, 1); (0, -1, -1, 0) |] |]
and plan_center =
  Array.get [| l1 / 2; (l1 + l2) / 2; (l2 + l3) / 2; (l3 + l4) / 2 |]
and dijm_of_p =
  Array.get
    [| (0, l1, zM + yM / 2); (l1, l2, zM + xM / 2); (l2, l3, zM + yM / 2);
       (l3, l4, zM + xM / 2) |]
;

value proj (x, y, z) =
  (leftB + cXE.val + (x * eCH.val - cXE.val) * z1E / (z * eCH.val + z1E),
   upperB + cYE.val + (y * eCH.val - cYE.val) * z1E / (z * eCH.val + z1E))
and p_of_i i =
  if i < l1 then 0 else if i < l2 then 1 else if i < l3 then 2 else 3
;

value proj_sh (x, y, z, dx, dy) =
  let (x0, y0) = proj (x, y, z) in
  (x0 + dx, y0 + dy)
;

value condemn_polyg =
  Array.get
    [| [proj_sh (0, 0, 0, 1, 0); proj_sh (0, 0, zM, 1, -1);
        proj_sh (xM, 0, zM, -1, -1); proj_sh (xM, 0, 0, -1, 0);
        proj_sh (0, 0, 0, 1, 0)];
       [proj_sh (xM, 0, 0, 0, 1); proj_sh (xM, 0, zM, 1, 1);
        proj_sh (xM, yM, zM, 1, -1); proj_sh (xM, yM, 0, 0, -1);
        proj_sh (xM, 0, 0, 0, 1)];
       [proj_sh (0, yM, 0, 1, 0); proj_sh (0, yM, zM, 1, 1);
        proj_sh (xM, yM, zM, -1, 1); proj_sh (xM, yM, 0, -1, 0);
        proj_sh (0, yM, 0, 1, 0)];
       [proj_sh (0, 0, 0, 0, 1); proj_sh (0, 0, zM, -1, 1);
        proj_sh (0, yM, zM, -1, -1); proj_sh (0, yM, 0, 0, -1);
        proj_sh (0, 0, 0, 0, 1)] |]
;

value select_pattern (gm, n) =
  let i =
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
  [ C'Pattern patt -> rt_select_pattern patt.(i) (0, 0)
  | C'Color col -> rt_select_color col.(i) ]
;

value disp_score (wid, gm) = do {
  select_pattern (gm, C'BlackP);
  rt_erase_draw_string wid (leftB + 10, 25)
    (if welltris_wizard.val then "score <" ^ string_of_int gm.score ^ ">   "
     else "score " ^ string_of_int gm.score ^ "   ")
}
and disp_nlines (wid, gm) =
  rt_erase_draw_string wid (leftB + 10, upperB - 15)
    ("nb lines " ^ string_of_int gm.nlines)
and disp_state (wid, gm) =
  rt_erase_draw_string wid (gW.val - rightB - 80, upperB - 15)
    (match gm.state with
     [ Pausing -> "    <pause>"
     | Ended -> "<game over>"
     | _ -> "           " ])
;

value possible (board, i, j, c) =
  if i < 0 || j < 0 then True
  else if j = jM || board.(i).(j).val = Square then False
  else if i < l1 then j < yZM && c.(0) = 0
  else if i < l2 then j < xZM && c.(1) = 0
  else if i < l3 then j < yZM && c.(2) = 0
  else j < xZM && c.(3) = 0
and incr_score (wid, gm, v) =
  for i = 1 to v do {
    gm.score := gm.score + 1;
    if \*** gm.score change_level = 0 then
      if gm.level < nlevels - 1 then do {
        gm.level := gm.level + 1;
        rt_cancel_timeout gm.xargs "";
        rt_set_timeout gm.xargs ""
          (rt_current_time gm.xargs + speed_tab gm.level) (gm.woops wid gm);
        print_string "level ";
        print_int gm.level;
        print_newline ()
      }
      else ()
    else ();
  }
;

value possible_move (gm, i, j, r) =
  let (a, b0, c, d) = rotmat gm.runner.s r
  and s = (pieces gm.runner.p).dsc in
  List.for_all
    (fun (di, dj) ->
       possible
         (gm.board, \*** (i + a * di + c * dj) iM, j + b0 * di + d * dj,
          gm.condemn))
    s
;

value new_runner (wid, gm) = do {
  let a = it_vect (fun n c -> if c = 0 then n + 1 else n) 0 gm.condemn in
  if a = 0 then do {
    gm.state := Ended;
    rt_cancel_timeout gm.xargs "";
    disp_state (wid, gm)
  }
  else do {
    let (p, _) =
      gloop (fun (p, a) -> (p + 1, if gm.condemn.(p) > 0 then a else a - 1))
        (fun (p, a) -> a = 0 && gm.condemn.(p) = 0) (0, random a)
    in
    gm.runner.i := plan_center p;
    gm.runner.j := -1;
    let (p, _) =
      gloop (fun (p, a) -> (p + 1, a - (pieces p).frq)) (fun (_, a) -> a < 0)
        (0, random sUMFRQ)
    in
    gm.runner.p := p - 1;
    gm.runner.s := random 2;
    gm.runner.r := random 4;
    gm.runner.dl := [];
    if not (possible_move (gm, gm.runner.i, gm.runner.j, gm.runner.r))
    then do {
      gm.state := Ended;
      rt_cancel_timeout gm.xargs "";
      disp_state (wid, gm)
    }
    else ()
  };
  ()
}
and pause (wid, gm) = do {
  gm.state := Pausing;
  rt_cancel_timeout gm.xargs "";
  disp_state (wid, gm)
}
and restart (wid, gm) = do {
  gm.state := Running;
  rt_cancel_timeout gm.xargs "";
  rt_set_timeout gm.xargs "" (rt_current_time gm.xargs + speed_tab gm.level)
    (gm.woops wid gm);
  disp_state (wid, gm)
};

value (disp_square, clear_square, mark_square) =
  let disp_or_clear_square (wid, gm, i, j, redraw_lines) =
    if j >= 0 then
      let u = max (j - zM) 0
      and v = if j < zM then 0 else 1 in
      let (x, y, dx, dy) =
        if i < l1 then (i, u, 1, v)
        else if i < l2 then (xM - u, i - l1, -v, 1)
        else if i < l3 then (l3 - i, yM - u, -1, -v)
        else (u, l4 - i, v, -1)
      and (z, dz) = (min j zM, 1 - v) in
      let p1 = proj (x, y, z)
      and p2 =
        proj
          (x + (if dx = dy then dx else 0), y + (if dx = -dy then dy else 0),
           z + dz)
      and p3 = proj (x + dx, y + dy, z + dz)
      and p4 =
        proj
          (x + (if dx = dy then 0 else dx), y + (if dx = -dy then 0 else dy),
           z)
      in
      rt_fill_polygon wid [p1; p2; p3; p4]
    else ()
  in
  (fun ((wid, gm, i, j, redraw_lines) as a) -> do {
     select_pattern (gm, C'GrayP);
     disp_or_clear_square a
   },
   fun ((wid, gm, i, j, redraw_lines) as a) -> do {
     rt_select_pixmap gm.pix;
     disp_or_clear_square a
   },
   fun ((wid, gm, i, j, redraw_lines) as a) -> do {
     select_pattern (gm, C'DiagonalP);
     disp_or_clear_square a
   })
;

value subtract f =
  fun
  [ [] -> f
  | e ->
      subtract_e f where rec subtract_e =
        fun
        [ [] -> []
        | [elem :: l] ->
            if List.mem elem e then subtract_e l
            else [elem :: subtract_e l] ] ]
;

value disp_condemn (wid, gm, p) = do {
  select_pattern (gm, C'SandP);
  rt_fill_polygon wid (condemn_polyg p)
};

value disp_board (wid, gm) = do {
  disp_score (wid, gm);
  disp_nlines (wid, gm);
  disp_state (wid, gm);
  for i = 0 to Array.length gm.board - 1 do {
    let v = gm.board.(i) in
    for j = 0 to Array.length v - 1 do {
      let s = v.(j) in
      match s.val with
      [ Square -> disp_square (wid, gm, i, j, False)
      | Moribund -> mark_square (wid, gm, i, j, False)
      | Empty -> () ];
    };
  };
  List.iter (fun (i, j) -> disp_square (wid, gm, i, j, False)) gm.runner.dl;
  for p = 0 to Array.length gm.condemn - 1 do {
    let c = gm.condemn.(p) in
    if c > 0 then disp_condemn (wid, gm, p) else ();
  }
};

value disp_uncondemn (wid, gm, p) = do {
  rt_select_pixmap gm.pix;
  rt_fill_polygon wid (condemn_polyg p);
  disp_board (wid, gm)
};

value disp_runner (wid, gm) = do {
  let i = gm.runner.i
  and j = gm.runner.j
  and p = gm.runner.p
  and s = gm.runner.s
  and r = gm.runner.r in
  let dl2 =
    let (a, b0, c, d) = rotmat s r in
    List.map
      (fun (di, dj) -> (\*** (i + a * di + c * dj) iM, j + b0 * di + d * dj))
      (pieces p).dsc
  in
(*
  let dl = gm.runner.dl in
  List.iter (fun (i, j) -> clear_square (wid, gm, i, j, True))
    (subtract dl dl2);
  List.iter (fun (i, j) -> disp_square (wid, gm, i, j, True))
    (subtract dl2 dl);
*)
  gm.runner.dl := dl2;
match wid with
| WidgetDr wid -> rt_clear_widget wid
| _ -> ()
end;
disp_board (wid, gm);
};

value mark_explosions (wid, gm) = do {
  let mark_plan (i1, i2, jm) =
    let empty_line j =
      gloop succ (fun i -> i = i2 || gm.board.(i).(j).val <> Empty) i1 = i2
    and full_line j =
      gloop succ (fun i -> i = i2 || gm.board.(i).(j).val = Empty) i1 = i2
    in
    mark_loop (jm - 1) where rec mark_loop jm =
      let j =
        gloop pred (fun j -> j < 0 || full_line j || j < zM && empty_line j)
          jm
      in
      if j >= 0 && gm.board.(i1).(j).val <> Empty then do {
        gm.explosions_to_do := True;
        incr_score (wid, gm, 50);
        gm.nlines := gm.nlines + 1;
        iterate
          (fun i -> do {
             if gm.board.(i).(j).val = Square then do {
               gm.board.(i).(j).val := Moribund;
               mark_square (wid, gm, i, j, True)
             }
             else ();
             i + 1
           })
          (i2 - i1) i1;
        [j :: mark_loop (j - 1)]
      }
      else []
  in
  gm.explosions_to_do := False;
  for p = 0 to Array.length gm.expl_list - 1 do {
    gm.expl_list.(p) :=
      if gm.condemn.(p) > 0 then [] else List.rev (mark_plan (dijm_of_p p));
  };
  disp_score (wid, gm);
  disp_nlines (wid, gm);
  if gm.explosions_to_do then
    if welltris_wizard.val then pause (wid, gm)
    else do {
      rt_cancel_timeout gm.xargs "";
      rt_set_timeout gm.xargs "" (rt_current_time gm.xargs + 300)
        (gm.woops wid gm)
    }
  else do {
    rt_cancel_timeout gm.xargs "";
    rt_set_timeout gm.xargs "" (rt_current_time gm.xargs + speed_tab gm.level)
      (gm.woops wid gm);
    new_runner (wid, gm)
  }
}
and explose (wid, gm) =
  for p = 0 to Array.length gm.expl_list - 1 do {
    let el = gm.expl_list.(p) in
    let (i1, i2, _) = dijm_of_p p in
    List.iter
      (fun j -> do {
         let rec copy_loop j =
           if j > 0 then do {
             iterate
               (fun i -> do {
                  let s = gm.board.(i).(j - 1).val
                  and d = gm.board.(i).(j).val in
                  if s <> d then do {
                    gm.board.(i).(j).val := s;
                    (match s with
                     [ Empty -> clear_square
                     | Square -> disp_square
                     | Moribund -> mark_square ])
                      (wid, gm, i, j, True)
                  }
                  else ();
                  i + 1
                })
               (i2 - i1) i1;
             copy_loop (j - 1)
           }
           else ()
         in
         copy_loop j;
         iterate
           (fun i -> do {
              if gm.board.(i).(0).val <> Empty then do {
                gm.board.(i).(0).val := Empty;
                clear_square (wid, gm, i, j, True)
              }
              else ();
              i + 1
            })
           (i2 - i1) i1
       })
      el;
  }
;

value woops_fun wid gm () = do {
  if gm.state <> Running then failwith "erreur dans woops_fun" else ();
  rt_set_timeout gm.xargs "" (rt_current_time gm.xargs + speed_tab gm.level)
    (gm.woops wid gm);
  if gm.explosions_to_do then do {
    explose (wid, gm);
    mark_explosions (wid, gm)
  }
  else do {
    for p = 0 to Array.length gm.condemn - 1 do {
      let c = gm.condemn.(p) in
      if c > 0 then
        if (do { gm.condemn.(p) := c - 1; gm.condemn.(p) }) = 0 then
          disp_uncondemn (wid, gm, p)
        else ()
      else ();
    };
    if List.exists
         (fun (i, j) -> not (possible (gm.board, i, j + 1, gm.condemn)))
         gm.runner.dl
    then do {
      List.iter
        (fun (i, j) -> do {
           if i >= 0 && j >= 0 then do {
             gm.board.(i).(j).val := Square;
             incr_score (wid, gm, 1)
           }
           else ();
           if j < zM then do {
             let p = p_of_i i in
             if gm.condemn.(p) = 0 then disp_condemn (wid, gm, p) else ();
             gm.condemn.(p) := cONDEMN_COUNT;
             ()
           }
           else ()
         })
        gm.runner.dl;
      mark_explosions (wid, gm)
    }
    else do { gm.runner.j := gm.runner.j + 1; disp_runner (wid, gm) }
  }
};

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
;

value colors =
  [| (255, 255, 255); (0, 0, 0); (203, 46, 201); (135, 206, 255);
     (255, 255, 135); (34, 139, 24); (255, 143, 52) |]
;

value patt_init xd =
  let patt = Array.map (fun p -> rt_create_pattern xd p 16 16) patterns in
  C'Pattern patt
;

value col_init xd =
  let col =
    Array.map
      (fun col ->
         let (r, g, b0) = col in
         rt_create_color xd (r, g, b0))
      colors
  in
  C'Color col
;

value pix_of_mm xd x =
  let (wmm, hmm) = screen_size_mm xd in
  let w = screen_width xd in
  truncate (x *. float w /. float wmm +. 0.5)
;

value welltris dname = do {
  let xd = rt_initialize dname in
  eCH.val := pix_of_mm xd 18.0;
  w.val := eCH.val * xM;
  h.val := eCH.val * yM;
  cXE.val := eCH.val * (xM / 2);
  cYE.val := eCH.val * (yM / 2);
  gW.val := leftB + w.val + 1 + rightB;
  gH.val := upperB + h.val + 1 + lowerB;
  let xargs = rt_args [xd] in
  let gm =
    {xd = xd; xargs = xargs; woops = woops_fun;
     pix = rt_create_pixmap xd gW.val gH.val; state = NotStarted; level = 0;
     score = 0; nlines = 0; explosions_to_do = False;
     expl_list = [| []; []; []; [] |]; condemn = [| 0; 0; 0; 0 |];
     patt = if is_colored xd then col_init xd else patt_init xd;
     runner = {i = 0; j = 0; p = 0; s = 0; r = 0; dl = []};
     board = Array.make iM [| |]}
  in
  rt_select_color (rt_black_color xd);
  rt_fill_rectangle (PixmapDr gm.pix) (0, 0, gW.val, gH.val);
  let x = ref Empty in
  for p = 0 to Array.length gm.board - 1 do {
    gm.board.(p) := Array.make jM x;
  };
  for i = 0 to Array.length gm.board - 1 do {
    let v = gm.board.(i) in
    for j = 0 to Array.length v - 1 do {
      let s = v.(j) in
      v.(j) :=
        if j < zM then ref Empty
        else if i < l1 then if j < yZM then ref Empty else s
        else if i < l2 then
          if j < xZM then gm.board.(xZM - 1 - j).(zM + i - l1) else s
        else if i < l3 then
          if j < yZM then gm.board.(l3 - 1 - i).(zM + yZM - 1 - j) else s
        else if j < xZM then gm.board.(j - zM).(zM + l4 - 1 - i)
        else s;
    };
  };
  init_random ();
  let expose (wid, _, _, _, _) = do {
    let drw = WidgetDr wid in
    if gm.state = NotStarted then do {
      gm.state := Running;
      rt_cancel_timeout gm.xargs "";
      rt_set_timeout gm.xargs ""
        (rt_current_time gm.xargs + speed_tab gm.level)
        (gm.woops (WidgetDr wid) gm);
      new_runner (drw, gm)
    }
    else ();
    disp_board (drw, gm);
    ()
  }
  and keyp (wid, k) = do {
    let drw = WidgetDr wid in
    if k = "q" then rt_stop_main_loop gm.xargs
    else if gm.state = Pausing then restart (drw, gm)
    else
      match k with
      [ "Up" | "R8" | "i" | "KP_8" ->
          if gm.state = Running then
            let i = gm.runner.i in
            let di = if i >= l1 / 2 && i < (l2 + l3) / 2 then -1 else 1 in
            let ni = \*** (i + di) iM in
            if possible_move (gm, ni, gm.runner.j, gm.runner.r) then do {
              gm.runner.i := ni;
              disp_runner (drw, gm)
            }
            else ()
          else ()
      | "Down" | "R14" | "k" | "KP_2" ->
          if gm.state = Running then
            let i = gm.runner.i in
            let di = if i >= l1 / 2 && i < (l2 + l3) / 2 then 1 else -1 in
            let ni = \*** (i + di) iM in
            if possible_move (gm, ni, gm.runner.j, gm.runner.r) then do {
              gm.runner.i := ni;
              disp_runner (drw, gm)
            }
            else ()
          else ()
      | "Left" | "R10" | "j" | "KP_4" ->
          if gm.state = Running then
            let i = gm.runner.i in
            let di =
              if i >= (l1 + l2) / 2 && i < (l3 + l4) / 2 then 1 else -1
            in
            let ni = \*** (i + di) iM in
            if possible_move (gm, ni, gm.runner.j, gm.runner.r) then do {
              gm.runner.i := ni;
              disp_runner (drw, gm)
            }
            else ()
          else ()
      | "Right" | "R12" | "l" | "KP_6" ->
          if gm.state = Running then
            let i = gm.runner.i in
            let di =
              if i >= (l1 + l2) / 2 && i < (l3 + l4) / 2 then -1 else 1
            in
            let ni = \*** (i + di) iM in
            if possible_move (gm, ni, gm.runner.j, gm.runner.r) then do {
              gm.runner.i := ni;
              disp_runner (drw, gm)
            }
            else ()
          else ()
      | "R11" | "u" | "KP_5" | "Begin" ->
          if gm.state = Running then
            let nr = \*** (gm.runner.r - 1) 4 in
            if possible_move (gm, gm.runner.i, gm.runner.j, nr) then do {
              gm.runner.r := nr;
              disp_runner (drw, gm)
            }
            else ()
          else ()
      | "n" ->
          if gm.state = Ended then do {
            gm.state := Running;
            gm.level := 0;
            gm.score := 0;
            gm.nlines := 0;
            gm.explosions_to_do := False;
            gm.condemn.(0) := 0;
            gm.condemn.(1) := 0;
            gm.condemn.(2) := 0;
            gm.condemn.(3) := 0;
            gm.expl_list.(0) := [];
            gm.expl_list.(1) := [];
            gm.expl_list.(2) := [];
            gm.expl_list.(3) := [];
            Array.iter (fun v -> Array.iter (fun s -> s.val := Empty) v)
              gm.board;
            rt_set_timeout gm.xargs ""
              (rt_current_time gm.xargs + speed_tab 0) (gm.woops drw gm);
            rt_clear_widget wid;
            new_runner (drw, gm);
            disp_board (drw, gm)
          }
          else ()
      | "c" -> do {
          select_pattern (gm, C'WhiteP);
          rt_fill_rectangle drw (0, 0, gW.val, gH.val)
        }
      | "e" -> do { rt_clear_widget wid; disp_board (drw, gm) }
      | " " | "Ins" | "K0" | "Insert" ->
          if gm.state = Running && gm.runner.j >= 1 then do {
            let (a, b0, c, d) = rotmat gm.runner.s gm.runner.r
            and s = (pieces gm.runner.p).dsc in
            while
              List.for_all
                (fun (di, dj) ->
                   let i = \*** (gm.runner.i + a * di + c * dj) iM
                   and j = gm.runner.j + b0 * di + d * dj in
                   possible (gm.board, i, j + 1, gm.condemn))
                s
            do {
              for p = 0 to Array.length gm.condemn - 1 do {
                let c = gm.condemn.(p) in
                if c > 0 then
                  if (do { gm.condemn.(p) := c - 1; gm.condemn.(p) }) = 0 then
                    disp_uncondemn (drw, gm, p)
                  else ()
                else ();
              };
              gm.runner.j := gm.runner.j + 1;
              incr_score (drw, gm, 1)
            };
            disp_runner (drw, gm);
            List.iter
              (fun (i, j) -> do {
                 if i >= 0 && j >= 0 then do {
                   gm.board.(i).(j).val := Square;
                   incr_score (drw, gm, 1)
                 }
                 else ();
                 if j < zM then do {
                   let p = p_of_i i in
                   if gm.condemn.(p) = 0 then disp_condemn (drw, gm, p)
                   else ();
                   gm.condemn.(p) := cONDEMN_COUNT;
                   ()
                 }
                 else ()
               })
              gm.runner.dl;
            mark_explosions (drw, gm)
          }
          else ()
      | "p" -> if gm.state = Running then pause (drw, gm) else ()
      | _ -> do { print_string k; print_string "-"; flush stdout } ];
    ()
  }
  in
  try do {
    let draw = PixmapDr gm.pix in
    select_pattern (gm, C'WhiteP);
    rt_fill_rectangle draw (0, 0, gW.val, gH.val);
    select_pattern (gm, C'BlackP);
    iterate
      (fun z -> do {
         let (x1, y1) = proj (0, 0, z)
         and (x2, y2) = proj (xM, yM, z) in
         rt_draw_rectangle draw (x1, y1, x2 - x1, y2 - y1);
         z + 1
       })
      (zM + 1) 0;
    iterate
      (fun x -> do {
         let p1 = proj (x, 0, 0)
         and p2 = proj (x, 0, zM)
         and p3 = proj (x, yM, zM)
         and p4 = proj (x, yM, 0) in
         rt_draw_lines draw [p1; p2; p3; p4];
         x + 1
       })
      (xM + 1) 0;
    iterate
      (fun y -> do {
         let p1 = proj (0, y, 0)
         and p2 = proj (0, y, zM)
         and p3 = proj (xM, y, zM)
         and p4 = proj (xM, y, 0) in
         rt_draw_lines draw [p1; p2; p3; p4];
         y + 1
       })
      (yM + 1) 0;
    let wid =
      rt_create_widget xd "welltris" "welltris" AutoPosition
        (Some (fun _ -> rt_stop_main_loop gm.xargs))
        (raw_desc [BackgroundAtt (PixmapPn gm.pix)]
           (gW.val, gH.val, 0, [SelExposure; SelKeyPress])
           (fun wid ->
              fun
              [ RawEvExpose x y width height ->
                  expose (wid, x, y, width, height)
              | RawEvKeyPress k -> keyp (wid, string_of_keysym k)
              | _ -> () ]))
    in
    rt_map_widget wid;
    rt_main_loop gm.xargs
  }
  with x -> do { rt_end xd; raise x };
  rt_end xd
};

value dname = ref "";

let i = ref 1 in
while i.val < Array.length argv do {
  match argv.(i.val) with
  [ "-d" -> incr i
  | s -> dname.val := s ];
  incr i
};

try welltris dname.val with
[ Failure s -> do {
    print_string "Failure: ";
    print_string s;
    print_newline ()
  }
| Invalid_argument s -> do {
    print_string "Invalid_argument: ";
    print_string s;
    print_newline ()
  }
| x -> do {
    print_string "Unknown exception in welltris";
    print_newline ();
    raise x
  } ];
