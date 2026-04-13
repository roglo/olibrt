(* $Id: termdef.ml,v 1.23 2017/12/28 10:38:58 deraugla Exp $ *)

open Rtdecl;
open Util;
open Xlib;
open Xft;
open Keysym;

type term_event =
  [ TermEvButtonPress of int and int and int and modified int
  | TermEvButtonRelease of int and int and int and modified int
  | TermEvButtonMotion of int and int and int
  | TermEvKeyPress of modified keysym
  | TermEvAnswer of string
  | TermEvExtendHistory
  | TermEvSizeChange of int and int ]
;

type term_event_handler = widget -> term_event -> unit;

value exceptq e =
  exceptq_e where rec exceptq_e =
    fun
    [ [] -> []
    | [elem :: l] -> if e == elem then l else [elem :: exceptq_e l] ]
;

value union l1 l2 =
  union_rec l1 where rec union_rec =
    fun
    [ [] -> l2
    | [a :: l] -> if List.mem a l2 then union_rec l else [a :: union_rec l] ]
;

type frozen 'a =
  [ Ffun of unit -> 'a
  | Fval of 'a ]
;
type mlazy 'a = ref (frozen 'a);

type input_state =
  [ IS_normal
  | IS_escape
  | IS_ext of int and list int
  | IS_csi of int and list int
  | IS_csi_question of int and list int
  | IS_escape_sharp
  | IS_escape_lparen
  | IS_escape_rparen ]
;

module Gstring :
  sig
    type t = 'abstract;
    type gchar = string;
    value empty : t;
    value length : t -> int;
    value get : t -> int -> gchar;
    value set : t -> int -> gchar -> unit;
    value create : int -> t;
    value make : int -> gchar -> t;
    value sub : t -> int -> int -> t;
    value fill : t -> int -> int -> gchar -> unit;
    value blit : t -> int -> t -> int -> int -> unit;
    value concat : t -> t -> t;
    value of_char : gchar -> t;
    value to_string : t -> string;
  end =
  struct
    type t = array gchar
    and gchar = string;
    value empty = [| |];
    value length s = Array.length s;
    value get s i = Array.get s i;
    value set s i c = Array.set s i c;
    value create n = Array.make n "";
    value make n c = Array.make n c;
    value sub s i j = Array.sub s i j;
    value concat s1 s2 = Array.append s1 s2;
    value fill s i j c = Array.fill s i j c;
    value blit s o1 d o2 len = Array.blit s o1 d o2 len;
    value of_char c = [| c |];
    value to_string s =
      let len = Array.fold_left (fun len c -> len + String.length c) 0 s in
      loop (Bytes.create len) 0 0 where rec loop t i j =
        if i = Array.length s then Bytes.to_string t
        else do {
          String.blit s.(i) 0 t j (String.length s.(i));
          loop t (i + 1) (j + String.length s.(i));
        }
    ;
  end
;

type term_global_info =
  { tgc : gC;
    ftfont : xftfont;
    attrs : xWindowAttributes;
    color : xftcolor;
    c_backg : mutable int;
    c_foreg : mutable int }
and term_local_info =
  { term_gi : term_global_info;
    draw : xftdraw;
    att_val : attribute_values;
    callb : term_event_handler;
    tfs : array (mlazy font);
    twidth : int;
    theight : int;
    max_history_size : mutable int;
    lines : mutable array line;
    nhrow : mutable int;
    nrow : mutable int;
    ncol : mutable int;
    shift : mutable int;
    sreg1 : mutable int;
    sreg2 : mutable int;
    crow : mutable int;
    ccol : mutable int;
    vmask : mutable char;
    foregm : mutable int;
    backgm : mutable int;
    vcrow : mutable int;
    vccol : mutable int;
    vvmask : mutable char;
    vforegm : mutable int;
    vbackgm : mutable int;
    erow1 : mutable int;
    ecol1 : mutable int;
    erow2 : mutable int;
    ecol2 : mutable int;
    scroll_cnt : mutable int;
    scroll_rect : mutable list (int * int * int * int);
    state : mutable input_state;
    tabs : mutable bytes;
    flags : mutable int }
and line =
  { str : Gstring.t; vid : bytes; foreg : array int; backg : array int }
and copy_area =
  { src_row : int;
    src_col : int;
    dst_row : int;
    dst_col : int;
    cp_nrow : int;
    cp_ncol : int }
;

value xor a b = if a then not b else b;
value cland c i = Char.code c land i;
value clor c i = Char.code c lor i;
value clxor c i = Char.code c lxor i;

value f_bld = 1 lsl 0
and f_ita = 1 lsl 1
and f_rev = 1 lsl 2
and f_blk = 1 lsl 3
and f_und = 1 lsl 4
and f_foreg = 1 lsl 5
and f_backg = 1 lsl 6;

value f_gc = f_bld lor f_ita lor f_rev;

value cnt = ref 0;
value new_bit () = do {
  let v = cnt.val in incr cnt;
  1 lsl v
};

value flg_auto_wrap = new_bit ()
and flg_newline_mode = new_bit ()
and flg_insert_mode = new_bit ()
and flg_appl_num_kp = new_bit ()
and flg_appl_curs_key = new_bit ()
and flg_orig_mode = new_bit ()
and flg_cursor_off = new_bit ()
and flg_reverse_video = new_bit ()
and flg_blink = new_bit ()
and flg_blink_string = new_bit ()
and flg_timeout_set = new_bit ()
and flg_bars = new_bit ()
and flg_trace = new_bit ();

value flg_set li flg = li.flags land flg != 0
and flg_reset li flg = li.flags land flg == 0
and set_flg li flg = li.flags := li.flags lor flg
and reset_flg li flg = li.flags := li.flags land lnot flg
and invert_flg li flg = li.flags := li.flags lxor flg;

value freeze f = ref (Ffun f);
value rec unfreeze v =
  match v.val with
  [ Ffun f -> do { v.val := Fval (f ()); unfreeze v }
  | Fval x -> x ]
;

value (term_global_info, get_term_global_info) =
  dynamo_global_info "term_global_info"
    (ref None : ref (option term_global_info))
and (term_local_info, get_term_local_info) =
  dynamo_local_info "term_local_info"
    (ref None : ref (option term_local_info))
;

value term_font =
  [| "*-courier-medium-r-*-14-*"; "*-courier-bold-r-*-14-*";
     "*-courier-medium-o-*-14-*"; "*-courier-bold-o-*-14-*" |]
and term_inter = ref 0
and term_band = ref 2
and term_blink = ref (500, 100);

value set_backg_foreg (xd, li, rev, vid, foreg, backg, font) = do {
  let w_foreg =
    if cland vid f_foreg != 0 then foreg
    else
      match li.att_val.foreg_att with
      [ Some (ColorPn c) -> c.pixel
      | _ -> xd.black ]
  in
  let w_backg =
    if cland vid f_backg != 0 then backg
    else
      match li.att_val.backg_att with
      [ Some (ColorPn c) -> c.pixel
      | _ -> xd.white ]
  in
  let (w_foreg, w_backg) =
    if rev then (w_backg, w_foreg) else (w_foreg, w_backg)
  in
  let gi = li.term_gi in
  if gi.c_foreg != w_foreg then do {
    xSetForeground (xd.dpy, gi.tgc, w_foreg);
    gi.c_foreg := w_foreg
  }
  else ();
  if gi.c_backg != w_backg then do {
    xSetBackground (xd.dpy, gi.tgc, w_backg);
    gi.c_backg := w_backg
  }
  else ();
};

value expose_row wid li cursor optim_spaces only_blink row bcol ecol =
  let xd = wid.wid_xd in
  let gi = li.term_gi in
  let line = li.lines.(li.nhrow - li.shift + row) in
  expose_loop bcol where rec expose_loop col1 =
    if col1 < ecol then do {
      let vid = Bytes.get line.vid col1 in
      let foreg = Array.get line.foreg col1 in
      let backg = Array.get line.backg col1 in
      let col2 =
        find_end (col1 + 1) where rec find_end col =
          if col == ecol || Bytes.get line.vid col != vid ||
             Array.get line.foreg col != foreg ||
             Array.get line.backg col != backg
          then
            col
          else find_end (col + 1)
      in
      if cland vid f_blk != 0 then set_flg li flg_blink_string else ();
      if cland vid f_blk == 0 && only_blink then ()
      else
        let ecol =
          let rec find_last_no_space col =
            if col == col1 || Gstring.get line.str (col - 1) <> " " then
              col
            else
              find_last_no_space (col - 1)
          in
          if not optim_spaces || cland vid (f_rev lor f_backg) != 0 then col2
          else find_last_no_space col2
        in
        let bcol =
          let rec find_first_no_space col =
            if col == col1 || Gstring.get line.str col <> " " then col
            else find_first_no_space (col + 1)
          in
          if not optim_spaces || cland vid (f_rev lor f_backg) != 0 then col1
          else find_first_no_space col1
        in
        if ecol > bcol then do {
          let str = Gstring.sub line.str bcol (ecol - bcol) in
          if flg_set li flg_trace then do {
            prerr_string ">";
            prerr_int (Char.code vid);
            prerr_string ">";
            prerr_string (if cursor then "|" else Gstring.to_string str);
            prerr_endline "<<";
            flush stderr
          }
          else ();
          let tband = opt_val term_band.val li.att_val.band_att in
          let tinter = opt_val term_inter.val li.att_val.inter_att in
          let x = tband + bcol * li.twidth in
          let y =
            let tband = (wid.height - (li.nrow - 1) * li.theight) / 2 in
            tband + row * li.theight + pix_of_mm xd 1.5
          in
          let font = unfreeze li.tfs.(cland vid (f_bld lor f_ita)) in
          let rev =
            xor (flg_set li flg_reverse_video) (cland vid f_rev != 0)
          in
          let params = (xd, li, rev, vid, foreg, backg, font) in
          if cursor then do {
            set_backg_foreg params;
            xDrawLine
              (xd.dpy, wid.win, gi.tgc, x, y, x, y + li.theight - tinter - 1)
          }
          else if cland vid f_blk != 0 && flg_reset li flg_blink then
            xClearArea
              (xd.dpy, wid.win, x, y,
               li.twidth * Gstring.length str, li.theight, 0)
          else do {
            set_backg_foreg params;
            let txt =
              let txt = Gstring.to_string str in
              match xd.char_set with
              [ Latin_1 -> txt
              | Utf_8 -> latin_1_of_utf_8 txt ]
            in
	    xftDrawString8
	      (li.draw, gi.color, gi.ftfont, x, y, txt, ecol - bcol);
            if cland vid f_und != 0 then
              xDrawLine
                (xd.dpy, wid.win, gi.tgc, x, y,
                 x + li.twidth * Gstring.length str, y)
            else ()
          }
        }
        else ();
      expose_loop col2
    }
    else ()
;

value term_expose_row wid li row bcol ecol =
  expose_row wid li False False False row bcol ecol
;

value term_expose_cursor wid li =
  if li.crow + li.shift < li.nrow && li.ccol < li.ncol then
    expose_row wid li (flg_set li flg_blink && flg_reset li flg_cursor_off)
      False False (li.crow + li.shift) li.ccol (li.ccol + 1)
  else ()
;

value term_expose_blink wid li = do {
  term_expose_cursor wid li;
  if flg_set li flg_blink_string then do {
    reset_flg li flg_blink_string;
    for row = 0 to li.nrow - 1 do {
      expose_row wid li False True True row 0 li.ncol;
    }
  }
  else ()
};


value row_col_of_xy li x y =
  let tband = opt_val term_band.val li.att_val.band_att in
  let row = min li.nrow (max 0 ((y - tband) / li.theight)) in
  let col = min li.ncol (max 0 ((x - tband) / li.twidth)) in (row, col)
;

value term_expose wid x y width height = do {
  let li = get_term_local_info wid.info in
  let (brow, bcol) = row_col_of_xy li x y in
  let (erow, ecol) =
    row_col_of_xy li (x + width + li.twidth - 1) (y + height + li.theight - 1)
  in
  if flg_set li flg_bars then do {
    let xd = wid.wid_xd in
    let tb = opt_val term_band.val li.att_val.band_att in
    let bx = tb + bcol * li.twidth in
    let by = tb + brow * li.theight in
    let ex = tb + ecol * li.twidth in
    let ey = tb + erow * li.theight in
    let rec draw_vertical_lines x =
      if x < ex then do {
        xDrawLine (xd.dpy, wid.win, xd.gc, x, by, x, ey);
        draw_vertical_lines (x + li.twidth)
      }
      else ()
    and draw_horizontal_lines y = do {
      let tinter = opt_val term_inter.val li.att_val.inter_att in
      xDrawLine (xd.dpy, wid.win, xd.gc, bx, y, ex, y);
      let y = y + li.theight - tinter in
      if y < ey then do {
        xDrawLine (xd.dpy, wid.win, xd.gc, bx, y, ex, y);
        draw_horizontal_lines (y + tinter)
      }
      else ()
    }
    in
    draw_vertical_lines bx;
    draw_horizontal_lines by
  }
  else ();
  for row = brow to erow - 1 do {
    expose_row wid li False True False row bcol ecol;
  };
  if li.crow >= brow && li.crow < erow && li.ccol >= bcol && li.ccol < ecol
  then
    term_expose_cursor wid li
  else ()
};

value term_soft_reset li = do {
  li.state := IS_normal;
  li.vmask := '\000';
  li.foregm := 0;
  li.backgm := 0;
  li.crow := 0;
  li.ccol := 0;
  li.sreg1 := 0;
  li.sreg2 := li.nrow;
  li.erow1 := 0;
  li.ecol1 := 0;
  li.erow2 := 0;
  li.ecol2 := 0;
  li.scroll_cnt := 0;
  li.scroll_rect := [];
  li.flags :=
    flg_auto_wrap lor
    (li.flags land
     (flg_blink lor flg_timeout_set lor flg_bars lor flg_trace));
  for row = li.nhrow to Array.length li.lines - 1 do {
    Gstring.fill li.lines.(row).str 0 li.ncol " ";
    Bytes.fill li.lines.(row).vid 0 li.ncol '\000';
    Array.fill li.lines.(row).backg 0 li.ncol 0;
    Array.fill li.lines.(row).foreg 0 li.ncol 0;
  };
  for i = 0 to li.ncol - 1 do {
    Bytes.set li.tabs i (if i mod 8 == 0 then 't' else ' ');
  }
};

value term_scroll_down wid li nb row nrow = do {
  let row = max 0 (min li.nrow row) in
  let nrow = min nrow (li.nrow - row) in
  let nb = min nb nrow in
  let xd = wid.wid_xd in
  let tband = opt_val term_band.val li.att_val.band_att in
  if row + nb < li.nrow then do {
    xCopyArea
      (xd.dpy, wid.win, wid.win, xd.gc, tband, tband + row * li.theight,
       li.ncol * li.twidth, (nrow - nb) * li.theight, tband,
       tband + (row + nb) * li.theight);
    li.scroll_cnt := li.scroll_cnt + 1
  }
  else ();
  if row < li.nrow then
    xClearArea
      (xd.dpy, wid.win, tband, tband + row * li.theight, li.ncol * li.twidth,
       nb * li.theight, 0)
  else ()
};

value term_scroll_up wid li nb row nrow = do {
  let row = max 0 (min li.nrow row) in
  let nrow = min nrow (li.nrow - row) in
  let nb = min nb nrow in
  let xd = wid.wid_xd in
  let tband = opt_val term_band.val li.att_val.band_att in
  let yband = (wid.height - li.nrow * li.theight) in
  if row + nb < li.nrow then do {
    xCopyArea
      (xd.dpy, wid.win, wid.win, xd.gc, tband,
       yband + (row + nb) * li.theight, li.ncol * li.twidth,
       (nrow - nb) * li.theight, tband, yband + row * li.theight);
    li.scroll_cnt := li.scroll_cnt + 1
  }
  else ();
  if row + nrow - nb < li.nrow then
    xClearArea
      (xd.dpy, wid.win, tband, yband + (row + nrow - nb) * li.theight,
       li.ncol * li.twidth, nb * li.theight, 0)
  else ()
};

value show_cursor wid li =
  let xd = wid.wid_xd in
  let rec blinking () = do {
    reset_flg li flg_timeout_set;
    if not wid.deleted && flg_reset li flg_cursor_off then do {
      invert_flg li flg_blink;
      term_expose_blink wid li;
      set_flg li flg_timeout_set;
      xd.sys_timeout :=
        [(wid,
          if flg_set li flg_blink then fst term_blink.val
          else snd term_blink.val,
          blinking) ::
         xd.sys_timeout]
    }
    else ()
  }
  in
  if flg_reset li flg_cursor_off then do {
    xd.cancel_timeout := exceptq wid xd.cancel_timeout;
    set_flg li flg_blink;
    term_expose_blink wid li;
    if flg_reset li flg_timeout_set then do {
      set_flg li flg_timeout_set;
      xd.sys_timeout :=
        [(wid, fst term_blink.val, blinking) :: xd.sys_timeout]
    }
    else ()
  }
  else ()
;

value hide_cursor wid li =
  if not wid.deleted then do {
    let xd = wid.wid_xd in reset_flg li flg_blink;
    xd.cancel_timeout := union [wid] xd.cancel_timeout;
    reset_flg li flg_timeout_set;
    term_expose_blink wid li
  }
  else ()
;

(* Emphasized text *)

value term_invert_zone wid li expose row col1 col2 = do {
  let row = max 0 (min (li.nrow + li.nhrow - li.shift - 1) row) in
  let col1 = max 0 (min li.ncol col1)
  and col2 = max 0 (min li.ncol col2) in
  let line = li.lines.(row) in
  for i = col1 to col2 - 1 do {
    Bytes.set line.vid i (Char.chr (clxor (Bytes.get line.vid i) f_rev));
  };
  let row = row - li.nhrow + li.shift in
  if row >= 0 && row < li.nrow && expose then
    term_expose_row wid li row col1 col2
  else ()
};

value display_emphasized_zone wid li expose row1 col1 =
  if row1 == li.erow2 then
    term_invert_zone wid li expose row1 (min col1 li.ecol2)
      (max col1 li.ecol2)
  else do {
    let (row1, col1) =
      if row1 < li.erow2 then (row1, col1) else (li.erow2, li.ecol2)
    and (row2, col2) =
      if row1 > li.erow2 then (row1, col1) else (li.erow2, li.ecol2)
    in
    term_invert_zone wid li expose row1 col1 li.ncol;
    for row = row1 + 1 to row2 - 1 do {
      term_invert_zone wid li expose row 0 li.ncol;
    };
    term_invert_zone wid li expose row2 0 col2
  }
;

value adjust_emph_row_col li row col =
  if col == 0 || row == li.nhrow + li.nrow then (row, 0)
  else
    let str = li.lines.(row).str in
    find (col - 1) where rec find i =
      if i == li.ncol then (row + 1, 0)
      else if Gstring.get str i = " " then find (i + 1)
      else (row, col)
;

value term_emphasize_from wid row col = do {
  let li = get_term_local_info wid.info in
  let row = li.nhrow - li.shift + max 0 (min (li.nrow - 1) row) in
  let col = max 0 (min col (li.ncol - 1)) in
  let (row, col) = adjust_emph_row_col li row col in
  display_emphasized_zone wid li True li.erow1 li.ecol1;
  li.erow1 := row;
  li.ecol1 := col;
  li.erow2 := row;
  li.ecol2 := col
};

value term_emphasize_to wid row col = do {
  let li = get_term_local_info wid.info in
  let row = li.nhrow - li.shift + max 0 (min (li.nrow - 1) row) in
  let col = max 0 (min col li.ncol) in
  let (row, col) = adjust_emph_row_col li row col in
  display_emphasized_zone wid li True row col;
  li.erow2 := row;
  li.ecol2 := col
};

value term_emphasized_location wid =
  let li = get_term_local_info wid.info in
  if li.erow1 < li.erow2 then
    ((li.erow1, li.ecol1), (li.erow2, li.ecol2))
  else if li.erow1 > li.erow2 then
    ((li.erow2, li.ecol2), (li.erow1, li.ecol1))
  else if li.ecol1 < li.ecol2 then
    ((li.erow1, li.ecol1), (li.erow2, li.ecol2))
  else
    ((li.erow2, li.ecol2), (li.erow1, li.ecol1))
;

value term_get_emphasized wid =
  let ((row1, col1), (row2, col2)) = term_emphasized_location wid in
  let li = get_term_local_info wid.info in
  sel_str Gstring.empty row1 col1 where rec sel_str s row col =
    (**)
    if row == row2 && col == col2 then Gstring.to_string s
    else
      let str = li.lines.(row).str in
      let last =
        find_last (if row < row2 then li.ncol else col2)
        where rec find_last i =
          if i == col then i
          else if Gstring.get str (i - 1) <> " " then i
          else find_last (i - 1)
      in
      let s = Gstring.concat s (Gstring.sub str col (last - col)) in
      if row == row2 then Gstring.to_string s
      else sel_str (Gstring.concat s (Gstring.of_char "\n")) (row + 1) 0
;

value term_resize wid li nrow ncol = do {
  let erow1 = li.erow1
  and ecol1 = li.ecol1 in
  let erow2 = li.erow2
  and ecol2 = li.ecol2 in
(*
  let nrow = max nrow li.nrow in
  let ncol = max ncol li.ncol in
*)
  display_emphasized_zone wid li False li.erow1 li.ecol1;
  let lines =
    make_array (li.nhrow + nrow)
      (fun _ ->
         {str = Gstring.make ncol " "; vid = Bytes.make ncol '\000';
          foreg = Array.make ncol 0; backg = Array.make ncol 0})
  in
  let tabs = Bytes.make ncol ' ' in
  let min_ncol = min ncol li.ncol in
  for row = 0 to li.nhrow + min nrow li.nrow - 1 do {
    Gstring.blit li.lines.(row).str 0 lines.(row).str 0 min_ncol;
    Bytes.blit li.lines.(row).vid 0 lines.(row).vid 0 min_ncol;
    Array.blit li.lines.(row).backg 0 lines.(row).backg 0 min_ncol;
    Array.blit li.lines.(row).foreg 0 lines.(row).foreg 0 min_ncol;
  };
  for i = 0 to ncol - 1 do {
    Bytes.set tabs i (if i mod 8 == 0 then 't' else ' ');
  };
  Bytes.blit li.tabs 0 tabs 0 min_ncol;
  li.lines := lines;
  li.nrow := nrow;
  li.ncol := ncol;
  li.sreg1 := 0;
  li.sreg2 := nrow;
  li.tabs := tabs;
  li.crow := min li.crow (max 0 (nrow - 1));
  li.ccol := min li.ccol (max 0 (ncol - 1));
  let (row, col) =
    adjust_emph_row_col li (min li.nrow erow1) (min li.ncol ecol1)
  in
  li.erow1 := row;
  li.ecol1 := col;
  li.erow2 := row;
  li.ecol2 := col;
  let (row, col) =
    adjust_emph_row_col li (min li.nrow erow2) (min li.ncol ecol2)
  in
  display_emphasized_zone wid li True row col;
  li.erow2 := row;
  li.ecol2 := col
};
