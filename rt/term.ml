(* $Id: term.ml,v 1.18 2017/12/28 10:38:58 deraugla Exp $ *)

open C_term;
open Rt_color;
open Rtdecl;
open Termdef;
open Util;
open Xlib;

value term_expose_vt100_row wid li row bcol ecol =
  if row + li.shift < li.nrow then
    term_expose_row wid li (row + li.shift) bcol ecol
  else ()
;

value lshift_vect vect bpos epos nb = do {
  let len = epos - bpos in
  let rec shift i0 cnt =
    if cnt > 0 then
      let line_0 = vect.(i0) in
      shift_turn i0 cnt where rec shift_turn i cnt =
        if cnt > 0 then
          let j = bpos + (i + nb - bpos) mod len in
          if j == i0 then do { vect.(i) := line_0; shift (i0 + 1) (cnt - 1) }
          else do { vect.(i) := vect.(j); shift_turn j (cnt - 1) }
        else ()
    else ()
  in
  if nb < 0 || nb > len then invalid_arg "lshift_vect" else ();
  shift bpos len
};

value term_insert_lines wid li row nb =
  if nb != 0 && row >= li.sreg1 && row < li.sreg2 then do {
    let nrow = li.sreg2 - row in
    let nb = min nrow nb in term_scroll_down wid li nb (row + li.shift) nrow;
    lshift_vect li.lines (li.nhrow + row) (li.nhrow + li.sreg2) (nrow - nb);
    for i = 0 to nb - 1 do {
      let line = li.lines.(li.nhrow + row + i) in
      Gstring.fill line.str 0 li.ncol " ";
      Bytes.fill line.vid 0 li.ncol '\000';
      Array.fill line.backg 0 li.ncol 0;
      Array.fill line.foreg 0 li.ncol 0;
    }
  }
  else ()
;

value term_delete_lines wid li row nb =
  if nb != 0 && row >= li.sreg1 && row < li.sreg2 then do {
    let nrow = li.sreg2 - row in
    let nb = min nrow nb in
    term_scroll_up wid li nb (row + li.shift) nrow;
    lshift_vect li.lines (li.nhrow + row) (li.nhrow + li.sreg2) nb;
    for i = 0 to nb - 1 do {
      let line = li.lines.(li.nhrow + li.sreg2 - i - 1) in
      Gstring.fill line.str 0 li.ncol " ";
      Bytes.fill line.vid 0 li.ncol '\000';
      Array.fill line.backg 0 li.ncol 0;
      Array.fill line.foreg 0 li.ncol 0;
    }
  }
  else ()
;

value get1_args def n _ = max def n;

value get2_args def n =
  fun
  [ [] -> (def, max def n)
  | [x :: _] -> (max def x, max def n) ]
;

value get3_args def n =
  fun
  [ [] -> (def, def, max def n)
  | [x] -> (def, max def x, max def n)
  | [x; y :: _] -> (max def y, max def x, max def n) ]
;

value get4_args def n =
  fun
  [ [] -> (def, def, def, max def n)
  | [x] -> (def, def, max def x, max def n)
  | [x; y] -> (def, max def y, max def x, max def n)
  | [x; y; z :: _] -> (max def z, max def y, max def x, max def n) ]
;

value not_impl txt = do {
  prerr_string "pas implemente': ";
  prerr_endline txt
};

value ignored txt = ();

value not_unders txt c = do {
  prerr_string "pas comprendre: ";
  prerr_string txt;
  prerr_string " ";
  prerr_char c;
  prerr_string " (\\";
  prerr_int (Char.code c);
  prerr_string ")\n";
  flush stderr
};

value cursor_erased = ref False;
value redraw_all = ref False;
value bcol = ref 0 and ecol = ref 0;

value save_ctx () = (cursor_erased.val, redraw_all.val, bcol.val, ecol.val);

value restore_ctx (cursor_erased_v, redraw_all_v, bcol_v, ecol_v) = do {
  cursor_erased.val := cursor_erased_v;
  redraw_all.val := redraw_all_v;
  bcol.val := bcol_v;
  ecol.val := ecol_v
};

value check_emph wid li =
  let erow1 = li.erow1 - li.nhrow + li.shift in
  let erow2 = li.erow2 - li.nhrow + li.shift in
  if if li.crow > max erow1 erow2 then False
     else if erow1 < erow2 then
       li.crow < erow2 || li.crow == erow2 && bcol.val < li.ecol2
     else if erow2 < erow1 then
       li.crow < erow1 || li.crow == erow1 && bcol.val < li.ecol1
     else if li.ecol1 < li.ecol2 then bcol.val < li.ecol2
     else if li.ecol2 < li.ecol1 then bcol.val < li.ecol1
     else False
  then
    term_emphasize_from wid 0 0
  else ()
;

value term_flush wid li = do {
  check_emph wid li;
  if not redraw_all.val then
    if ecol.val > bcol.val then do {
      term_expose_vt100_row wid li li.crow bcol.val ecol.val;
      cursor_erased.val := True;
      bcol.val := li.ccol;
      ecol.val := li.ccol
    }
    else if not cursor_erased.val then do {
      if li.ccol < li.ncol then
        term_expose_vt100_row wid li li.crow li.ccol (li.ccol + 1)
      else ();
      cursor_erased.val := True
    }
    else ()
  else ()
};

value t_check_cursor_position li = do {
  let row1 = if flg_set li flg_orig_mode then li.sreg1 else 0 in
  let row2 = if flg_set li flg_orig_mode then li.sreg2 else li.nrow in
  li.crow := max row1 (min (row2 - 1) li.crow);
  li.ccol := max 0 (min (li.ncol - 1) li.ccol)
};

value t_move_cursor wid li (row, col) = do {
  term_flush wid li;
  li.crow := row - 1 + (if flg_set li flg_orig_mode then li.sreg1 else 0);
  li.ccol := col - 1;
  t_check_cursor_position li;
  bcol.val := li.ccol;
  ecol.val := li.ccol
};

value t_move_cursor_up wid li n =
  if li.crow > 0 then do {
    term_flush wid li;
    let n = if n == 0 then 1 else n in li.crow := li.crow - n;
    t_check_cursor_position li;
    bcol.val := li.ccol;
    ecol.val := li.ccol
  }
  else ()
;

value t_move_cursor_down wid li n =
  if li.crow < li.nrow - 1 then do {
    term_flush wid li;
    let n = if n == 0 then 1 else n in li.crow := li.crow + n;
    t_check_cursor_position li;
    bcol.val := li.ccol;
    ecol.val := li.ccol
  }
  else ()
;

value t_move_cursor_right wid li n =
  if li.ccol < li.ncol - 1 then do {
    term_flush wid li;
    let n = if n == 0 then 1 else n in li.ccol := li.ccol + n;
    t_check_cursor_position li;
    bcol.val := li.ccol;
    ecol.val := li.ccol
  }
  else ()
;

value t_move_cursor_left wid li n =
  if li.ccol > 0 then do {
    term_flush wid li;
    let n = if n == 0 then 1 else n in li.ccol := li.ccol - n;
    t_check_cursor_position li;
    bcol.val := li.ccol;
    ecol.val := li.ccol
  }
  else ()
;

value t_linefeed wid li =
  if li.crow == li.sreg2 - 1 then do {
    term_flush wid li;
    if li.sreg1 == 0 && li.sreg2 == li.nrow && li.max_history_size != 0
    then do {
      let len = li.nhrow + li.nrow in
      if li.nhrow < li.max_history_size then do {
        let line =
          {str = Gstring.make li.ncol " ";
           vid = Bytes.make li.ncol '\000'; foreg = Array.make li.ncol 0;
           backg = Array.make li.ncol 0}
        in
        let v =
          make_array (len + 1)
            (fun i -> if i < len then li.lines.(i) else line)
        in
        li.lines := v;
        li.nhrow := li.nhrow + 1;
        li.callb wid TermEvExtendHistory
      }
      else do {
        lshift_vect li.lines 0 len 1;
        if li.erow1 > 0 then li.erow1 := li.erow1 - 1 else li.ecol1 := 0;
        if li.erow2 > 0 then li.erow2 := li.erow2 - 1 else li.ecol2 := 0;
        let line = li.lines.(len-1) in Gstring.fill line.str 0 li.ncol " ";
        Bytes.fill line.vid 0 li.ncol '\000';
        Array.fill line.backg 0 li.ncol 0;
        Array.fill line.foreg 0 li.ncol 0
      };
      term_scroll_up wid li 1 0 li.nrow;
      if li.shift != 0 then term_expose_row wid li (li.nrow - 1) 0 li.ncol
      else ()
    }
    else term_delete_lines wid li li.sreg1 1
  }
  else t_move_cursor_down wid li 1
;

value t_reverse_linefeed wid li =
  if li.crow == li.sreg1 then do {
    term_flush wid li;
    term_insert_lines wid li li.sreg1 1
  }
  else t_move_cursor_up wid li 1
;

value t_return wid li =
  if li.ccol != 0 then do {
    term_flush wid li;
    li.ccol := 0;
    bcol.val := li.ccol;
    ecol.val := li.ccol
  }
  else ()
;

value t_erase_in_display wid li =
  fun
  [ 0 -> do {
      term_flush wid li;
      let xd = wid.wid_xd in
      for row = li.crow to li.nrow - 1 do {
        let line = li.lines.(li.nhrow + row) in
        let bcol = if row == li.crow then li.ccol else 0 in
        Gstring.fill line.str bcol (li.ncol - bcol) " ";
        Bytes.fill line.vid bcol (li.ncol - bcol) '\000';
        Array.fill line.backg bcol (li.ncol - bcol) 0;
        Array.fill line.foreg bcol (li.ncol - bcol) 0;
      };
      let tband = opt_val term_band.val li.att_val.band_att in
      if li.ccol < li.ncol then
        xClearArea
          (xd.dpy, wid.win, tband + li.ccol * li.twidth,
           tband + (li.crow + li.shift) * li.theight,
           (li.ncol - li.ccol) * li.twidth, li.theight, 0)
      else ();
      if li.crow + 1 < li.nrow then
        xClearArea
          (xd.dpy, wid.win, tband,
           tband + (li.crow + 1 + li.shift) * li.theight, wid.width,
           (li.nrow - (li.crow + 1)) * li.theight, 0)
      else ();
      cursor_erased.val := True;
      bcol.val := li.ccol;
      ecol.val := li.ccol
    }
  | 1 -> do {
      term_flush wid li;
      let xd = wid.wid_xd in
      for row = 0 to li.crow do {
        let line = li.lines.(li.nhrow + row) in
        let ecol = if row == li.crow then li.ccol + 1 else li.ncol in
        Gstring.fill line.str 0 ecol " ";
        Bytes.fill line.vid 0 ecol '\000';
        Array.fill line.backg 0 ecol 0;
        Array.fill line.foreg 0 ecol 0;
      };
      let tband = opt_val term_band.val li.att_val.band_att in
      if li.crow > 1 then
        xClearArea
          (xd.dpy, wid.win, tband, tband + li.shift * li.theight, wid.width,
           (li.crow - 1) * li.theight, 0)
      else ();
      xClearArea
        (xd.dpy, wid.win, tband, tband + (li.crow + li.shift) * li.theight,
         min li.ncol (li.ccol + 1) * li.twidth, wid.height, 0);
      cursor_erased.val := True;
      bcol.val := li.ccol;
      ecol.val := li.ccol
    }
  | 2 -> do {
      term_emphasize_from wid 0 0;
      let xd = wid.wid_xd in
      for row = 0 to li.nrow - 1 do {
        let line = li.lines.(li.nhrow + row) in
        Gstring.fill line.str 0 li.ncol " ";
        Bytes.fill line.vid 0 li.ncol '\000';
        Array.fill line.backg 0 li.ncol 0;
        Array.fill line.foreg 0 li.ncol 0;
      };
      if li.shift == 0 then xClearWindow (xd.dpy, wid.win)
      else
        let tband = opt_val term_band.val li.att_val.band_att in
        xClearArea
          (xd.dpy, wid.win, tband, tband + li.shift * li.theight, 0, 0, 0);
      cursor_erased.val := True;
      bcol.val := li.ccol;
      ecol.val := li.ccol
    }
  | 3 -> do {
      (* olibrt specific extension : erase history *)
      for row = 0 to li.nrow - 1 do {
        li.lines.(row) := li.lines.(li.nhrow + row);
      };
      li.nhrow := 0;
      li.shift := 0;
    }
  | n -> not_unders ("CSI " ^ string_of_int n) 'J' ]
;

value t_erase_in_line wid li =
  fun
  [ 0 -> do {
      term_flush wid li;
      let xd = wid.wid_xd in
      let line = li.lines.(li.nhrow + li.crow) in
      let len = li.ncol - li.ccol in Gstring.fill line.str li.ccol len " ";
      Bytes.fill line.vid li.ccol len '\000';
      Array.fill line.backg li.ccol len 0;
      Array.fill line.foreg li.ccol len 0;
      if li.ccol < li.ncol then
        let tband = opt_val term_band.val li.att_val.band_att in
        xClearArea
          (xd.dpy, wid.win, tband + li.ccol * li.twidth,
           tband + (li.crow + li.shift) * li.theight,
           (li.ncol - li.ccol) * li.twidth, li.theight, 0)
      else ();
      cursor_erased.val := True;
      bcol.val := li.ccol
    }
  | 1 -> do {
      term_flush wid li;
      let xd = wid.wid_xd in
      let line = li.lines.(li.nhrow + li.crow) in
      let len = li.ccol + 1 in Gstring.fill line.str 0 len " ";
      Bytes.fill line.vid 0 len '\000';
      Array.fill line.backg 0 len 0;
      Array.fill line.foreg 0 len 0;
      if li.ccol < li.ncol then
        let tband = opt_val term_band.val li.att_val.band_att in
        xClearArea
          (xd.dpy, wid.win, tband, tband + (li.crow + li.shift) * li.theight,
           (li.ccol + 1) * li.twidth, li.theight, 0)
      else ();
      cursor_erased.val := True
    }
  | 2 -> do {
      term_flush wid li;
      let xd = wid.wid_xd in
      let line = li.lines.(li.nhrow + li.crow) in
      let len = li.ncol in Gstring.fill line.str 0 len " ";
      Bytes.fill line.vid 0 len '\000';
      Array.fill line.backg 0 len 0;
      Array.fill line.foreg 0 len 0;
      let tband = opt_val term_band.val li.att_val.band_att in
      xClearArea
        (xd.dpy, wid.win, tband, tband + (li.crow + li.shift) * li.theight,
         li.ncol * li.twidth, li.theight, 0);
      cursor_erased.val := True
    }
  | n -> not_unders ("CSI " ^ string_of_int n) 'K' ]
;

value t_set_scrolling_region wid li (v1, v2) =
  let top = v1 - 1 in
  let bot = if v2 >= li.nrow || v2 == 1 then li.nrow else v2 in
  if bot >= top + 2 then do {
    term_flush wid li;
    li.sreg1 := top;
    li.sreg2 := bot;
    li.ccol := 0;
    li.crow := if flg_set li flg_orig_mode then li.sreg1 else 0;
    bcol.val := li.ccol;
    ecol.val := li.ccol
  }
  else ()
;

value t_set_reverse_video wid li rev = do {
  let xd = wid.wid_xd in
  let backg =
    if rev then
      match li.att_val.foreg_att with
      [ Some (ColorPn c) -> c.pixel
      | _ -> xd.black ]
    else
      match li.att_val.backg_att with
      [ Some (ColorPn c) -> c.pixel
      | _ -> xd.white ]
  in
  xSetWindowBackground (xd.dpy, wid.win, backg);
  xClearWindow (xd.dpy, wid.win);
  if li.shift > 0 then
    let tband = opt_val term_band.val li.att_val.band_att in
    term_expose wid tband tband (li.ncol * li.twidth) (li.shift * li.theight)
  else ()
};

value self_insert wid li c = do {
  if li.ccol == li.ncol then
    if flg_set li flg_auto_wrap then do { t_return wid li; t_linefeed wid li }
    else do {
      li.ccol := li.ccol - 1;
      if bcol.val == li.ncol then do {
        bcol.val := li.ccol;
        ecol.val := li.ccol
      }
      else ()
    }
  else ();
  check_emph wid li;
  let line = li.lines.(li.nhrow + li.crow) in
  if flg_set li flg_insert_mode && li.ccol + 1 < li.ncol then
    let first = li.ccol in
    let last = li.ncol - 1 in
    let lc = Gstring.get line.str last
    and lv = Bytes.get line.vid last
    and lf = Array.get line.foreg last
    and lb = Array.get line.backg last in
    let last =
      let rec get_last n =
        if n >= first && Gstring.get line.str n = lc &&
           Bytes.get line.vid n == lv && Array.get line.foreg n == lf &&
           Array.get line.backg n == lb
        then
          get_last (n - 1)
        else n
      in
      get_last (last - 1) + 1
    in
    if last > first then do {
      Gstring.blit line.str first line.str (first + 1) (last - first);
      Bytes.blit line.vid first line.vid (first + 1) (last - first);
      Array.blit line.foreg first line.foreg (first + 1) (last - first);
      Array.blit line.backg first line.backg (first + 1) (last - first);
      ecol.val := max ecol.val (last + 1)
    }
    else ()
  else ();
  Gstring.set line.str li.ccol c;
  Bytes.set line.vid li.ccol li.vmask;
  Array.set line.foreg li.ccol li.foregm;
  Array.set line.backg li.ccol li.backgm;
  li.ccol := li.ccol + 1;
  ecol.val := max ecol.val li.ccol
};

value delete_chars xd li n = do {
  let src = li.ccol + n in
  let line = li.lines.(li.nhrow + li.crow) in
  if src < li.ncol then do {
    Gstring.blit line.str src line.str li.ccol (li.ncol - src);
    Bytes.blit line.vid src line.vid li.ccol (li.ncol - src);
    Array.blit line.foreg src line.foreg li.ccol (li.ncol - src);
    Array.blit line.backg src line.backg li.ccol (li.ncol - src)
  }
  else ();
  let n = min n (li.ncol - li.ccol) in
  Gstring.fill line.str (li.ncol - n) n " ";
  Bytes.fill line.vid (li.ncol - n) n '\000';
  Array.fill line.foreg (li.ncol - n) n 0;
  Array.fill line.backg (li.ncol - n) n 0;
  ecol.val := li.ncol
};

value term_control_char wid li =
  fun
  [ '\b' -> do { t_move_cursor_left wid li 1; True }
  | '\n' -> do {
      if flg_set li flg_newline_mode then t_return wid li else ();
      t_linefeed wid li;
      True
    }
  | _ -> False ]
;

value is_utf8_cont_byte b =
  Char.code b land 0b11000000 = 0b10000000
;

value st_normal wid li =
  fun
  [ '\000' -> ()
  | '\007' -> xBell (wid.wid_xd.dpy, 100)
  | '\015' -> not_impl "select G0 character set"
  | '\027' -> li.state := IS_escape
  | '\r' -> t_return wid li
  | '\t' ->
      if li.ccol >= li.ncol - 1 then do { t_return wid li; t_linefeed wid li }
      else do {
        term_flush wid li;
        li.ccol := li.ccol + 1;
        while li.ccol < li.ncol && Bytes.get li.tabs li.ccol != 't' do {
          li.ccol := li.ccol + 1
        };
        if li.ccol == li.ncol then li.ccol := li.ncol - 1 else ();
        bcol.val := li.ccol;
        ecol.val := li.ccol
      }
  | c ->
      let xd = wid.wid_xd in
      match xd.char_set with
      [ Utf_8 ->
          if is_utf8_cont_byte c && li.ccol >= 1 then
            let line = li.lines.(li.nhrow + li.crow) in
            let cc = Gstring.get line.str (li.ccol - 1) in
            Gstring.set line.str (li.ccol - 1) (cc ^ String.make 1 c)
          else self_insert wid li (String.make 1 c)
      | Latin_1 -> self_insert wid li (String.make 1 c) ] ]
;

value st_escape wid li =
  fun
  [ '[' -> li.state := IS_csi 0 []
  | ']' -> li.state := IS_ext 0 []
  | 'c' -> do {
      term_soft_reset li;
      t_set_reverse_video wid li False;
      cursor_erased.val := True;
      bcol.val := li.ccol;
      ecol.val := li.ccol
    }
  | 'D' -> t_linefeed wid li
  | 'E' -> do { t_linefeed wid li; t_return wid li }
  | 'H' -> if li.ccol < li.ncol then Bytes.set li.tabs li.ccol 't' else ()
  | 'M' -> t_reverse_linefeed wid li
  | '7' -> do {
      li.vcrow := li.crow;
      li.vccol := li.ccol;
      li.vvmask := li.vmask;
      li.vforegm := li.foregm;
      li.vbackgm := li.backgm
    }
  | '8' -> do {
      term_flush wid li;
      li.crow := li.vcrow;
      li.ccol := li.vccol;
      li.vmask := li.vvmask;
      li.foregm := li.vforegm;
      li.backgm := li.vbackgm;
      t_check_cursor_position li;
      bcol.val := li.ccol;
      ecol.val := li.ccol
    }
  | '=' -> set_flg li flg_appl_num_kp
  | '<' -> ()
  | '>' -> reset_flg li flg_appl_num_kp
  | '#' -> li.state := IS_escape_sharp
  | '(' -> li.state := IS_escape_lparen
  | ')' -> li.state := IS_escape_rparen
  | c -> not_unders "ESC " c ]
;

value st_escape_sharp wid li =
  fun
  [ '6' -> not_impl "set dbl width"
  | '8' -> do {
      term_emphasize_from wid 0 0;
      for row = 0 to li.nrow - 1 do {
        let line = li.lines.(li.nhrow + row) in
        Gstring.fill line.str 0 li.ncol (String.make 1 'E');
        Bytes.fill line.vid 0 li.ncol '\000';
        Array.fill line.backg 0 li.ncol 0;
        Array.fill line.foreg 0 li.ncol 0;
      };
      redraw_all.val := True
    }
  | c -> not_unders "ESC #" c ]
;

value st_escape_lparen wid li =
  fun
  [ 'B' -> not_impl "select standard as G0 set"
  | '0' -> not_impl "select special graphics as G0 set"
  | c -> not_unders "ESC (" c ]
;

value st_escape_rparen wid li =
  fun
  [ 'B' -> not_impl "select standard as G1 set"
  | c -> not_unders "ESC )" c ]
;

value st_ext wid li (n, vl) =
  fun
  [ '0'..'9' as c ->
      let n = 10 * n + Char.code c - Char.code '0' in li.state := IS_ext n vl
  | ';' -> li.state := IS_ext 0 [n :: vl]
  | 'b' -> do {
      invert_flg li flg_bars;
      xClearWindow (wid.wid_xd.dpy, wid.win);
      term_expose wid 0 0 wid.width wid.height;
      cursor_erased.val := False;
      bcol.val := li.ccol;
      ecol.val := li.ccol
    }
  | 't' -> invert_flg li flg_trace
  | 'B' ->
      if vl = [] && n == 0 then do {
        li.backgm := 0;
        li.vmask := Char.chr (cland li.vmask (lnot f_backg))
      }
      else do {
        let (r, g, b) = get3_args 0 n vl in
        let col = rt_closest_color wid.wid_xd (r, g, b) in
        li.backgm := col.pixel;
        li.vmask := Char.chr (clor li.vmask f_backg)
      }
  | 'F' ->
      if vl = [] && n == 0 then do {
        li.foregm := 0;
        li.vmask := Char.chr (cland li.vmask (lnot f_foreg))
      }
      else do {
        let (r, g, b) = get3_args 0 n vl in
        let col = rt_closest_color wid.wid_xd (r, g, b) in
        li.foregm := col.pixel;
        li.vmask := Char.chr (clor li.vmask f_foreg)
      }
  | c -> not_unders "ESC ] " c ]
;

value st_csi wid li (n, vl) =
  fun
  [ '0'..'9' as c ->
      let n = 10 * n + Char.code c - Char.code '0' in li.state := IS_csi n vl
  | ';' -> li.state := IS_csi 0 [n :: vl]
  | 'A' -> t_move_cursor_up wid li n
  | 'B' -> t_move_cursor_down wid li n
  | 'C' -> t_move_cursor_right wid li n
  | 'D' -> t_move_cursor_left wid li n
  | 'g' ->
      match n with
      [ 0 -> if li.ccol < li.ncol then Bytes.set li.tabs li.ccol ' ' else ()
      | 3 -> Bytes.fill li.tabs 0 li.ncol ' '
      | _ -> ignored ("CSI " ^ string_of_int n ^ " g") ]
  | 'H' | 'f' -> t_move_cursor wid li (get2_args 1 n vl)
  | 'h' ->
      match n with
      [ 4 -> set_flg li flg_insert_mode
      | 20 -> set_flg li flg_newline_mode
      | _ -> not_unders ("CSI " ^ string_of_int n) 'h' ]
  | 'J' -> t_erase_in_display wid li n
  | 'K' -> t_erase_in_line wid li n
  | 'L' -> do {
      term_flush wid li;
      term_insert_lines wid li li.crow (max n 1)
    }
  | 'l' ->
      match n with
      [ 4 -> reset_flg li flg_insert_mode
      | 20 -> reset_flg li flg_newline_mode
      | _ -> not_unders ("CSI " ^ string_of_int n) 'l' ]
  | 'M' -> do {
      term_flush wid li;
      term_delete_lines wid li li.crow (max n 1)
    }
  | 'm' ->
      let vmask =
        List.fold_right
          (fun x vmask ->
             match x with
             [ 0 ->
                 vmask land
                 lnot (f_bld lor f_blk lor f_rev lor f_und lor f_ita)
             | 1 -> vmask lor f_bld
             | 4 -> vmask lor f_und
             | 5 -> vmask lor f_ita
             | 7 -> vmask lor f_rev
             | x -> do { not_unders ("CSI " ^ string_of_int x) 'm'; 0 } ])
          [n :: vl] (Char.code li.vmask)
      in
      li.vmask := Char.chr vmask
  | 'n' ->
      match n with
      [ 6 ->
          let answ =
            "\027[" ^ string_of_int (li.crow + 1) ^ ";" ^
            string_of_int (li.ccol + 1) ^ "R"
          in
          li.callb wid (TermEvAnswer answ)
      | n -> not_unders ("CSI " ^ string_of_int n ^ " ") 'n' ]
  | 'P' -> delete_chars wid.wid_xd li n
  | 'q' -> not_impl "leds"
  | 'r' -> t_set_scrolling_region wid li (get2_args 1 n vl)
  | '?' -> li.state := IS_csi_question 0 []
  | '@' ->
      if flg_reset li flg_insert_mode then do {
        set_flg li flg_insert_mode;
        for i = 1 to get1_args 0 n vl do {
          self_insert wid li (String.make 1 ' ');
          li.ccol := li.ccol - 1;
        };
        reset_flg li flg_insert_mode
      }
      else ()
  | c -> not_unders "CSI ... " c ]
;

value st_csi_question wid li (n, vl) =
  fun
  [ '0'..'9' as c ->
      let n = 10 * n + Char.code c - Char.code '0' in
      li.state := IS_csi_question n vl
  | 'h' ->
      match get1_args 0 n vl with
      [ 1 -> set_flg li flg_appl_curs_key
      | 3 -> do {
          not_impl "set 132 columns erase screen";
          t_erase_in_display wid li 2
        }
      | 4 -> not_impl "smooth scrolling"
      | 5 -> do {
          set_flg li flg_reverse_video;
          t_set_reverse_video wid li True;
          redraw_all.val := True
        }
      | 6 -> do {
          term_flush wid li;
          set_flg li flg_orig_mode;
          li.crow := li.sreg1;
          li.ccol := 0;
          bcol.val := li.ccol;
          ecol.val := li.ccol
        }
      | 7 -> set_flg li flg_auto_wrap
      | 8 -> not_impl "set auto repeat"
      | 35 -> do {
          term_flush wid li;
          set_flg li flg_cursor_off;
          hide_cursor wid li
        }
      | 47 -> not_impl "interface monitor on"
      | _ -> not_unders ("CSI ? ... " ^ string_of_int n) 'h' ]
  | 'l' ->
      match get1_args 0 n vl with
      [ 1 -> reset_flg li flg_appl_curs_key
      | 2 -> not_impl "unlock keyboard"
      | 3 -> do {
          not_impl "set 80 columns erase screen";
          t_erase_in_display wid li 2
        }
      | 4 -> not_impl "jump scrolling"
      | 5 -> do {
          reset_flg li flg_reverse_video;
          t_set_reverse_video wid li False;
          redraw_all.val := True
        }
      | 6 -> do {
          term_flush wid li;
          reset_flg li flg_orig_mode;
          li.crow := 0;
          li.ccol := 0;
          bcol.val := li.ccol;
          ecol.val := li.ccol
        }
      | 7 -> reset_flg li flg_auto_wrap
      | 8 -> not_impl "reset auto repeat"
      | 35 -> do {
          term_flush wid li;
          reset_flg li flg_cursor_off;
(**)
          show_cursor wid li
(**)
        }
      | 47 -> not_impl "interface monitor off"
      | _ -> not_unders ("CSI ? ... " ^ string_of_int n) 'l' ]
  | c -> not_unders "CSI ? ... " c ]
;

value term_send wid str =
  let li = get_term_local_info wid.info in
  let send_loop () =
    for i = 0 to String.length str - 1 do {
      let c = String.get str i in
      if not (term_control_char wid li c) then
        match li.state with
        [ IS_csi_question n vl -> do {
            li.state := IS_normal;
            st_csi_question wid li (n, vl) c
          }
        | IS_csi n vl -> do { li.state := IS_normal; st_csi wid li (n, vl) c }
        | IS_ext n vl -> do { li.state := IS_normal; st_ext wid li (n, vl) c }
        | IS_escape -> do { li.state := IS_normal; st_escape wid li c }
        | IS_escape_sharp -> do {
            li.state := IS_normal;
            st_escape_sharp wid li c
          }
        | IS_escape_lparen -> do {
            li.state := IS_normal;
            st_escape_lparen wid li c
          }
        | IS_escape_rparen -> do {
            li.state := IS_normal;
            st_escape_rparen wid li c
          }
        | IS_normal -> st_normal wid li c ]
      else ();
    }
  in
  let ctx = save_ctx () in
  try do {
    redraw_all.val := False;
    cursor_erased.val := False;
    bcol.val := li.ccol;
    ecol.val := li.ccol;
    send_loop ();
    check_emph wid li;
    if not redraw_all.val then do {
      if ecol.val > bcol.val then do {
        term_expose_vt100_row wid li li.crow bcol.val ecol.val;
        cursor_erased.val := True
      }
      else ();
      if cursor_erased.val then term_expose_cursor wid li else ()
    }
    else term_expose wid 0 0 wid.width wid.height;
    restore_ctx ctx
  }
  with x -> do { restore_ctx ctx; raise x }
;
