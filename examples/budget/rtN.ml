type xdata = Rt.xdata;
type widget = Rt.widget;
type delete_callback = Rt.delete_callback;

type attribute =
  [ BandAtt of int
  | HeightAtt of int
  | InterAtt of int
  | NameAtt of string
  | WidthAtt of int ]
;
type size_policy = [ FIXSZ | INCSZ ];
type direction = [ DIRx | DIRy | DIRz ];
type orientation = [ ORup | ORdown | ORleft | ORright ];
type keysym = Rt.keysym;

type termEvButton = { tEvRow : int; tEvCol : int; tEvButton : int };

type arrow_event = Rt.arrow_event == [ ArrowEvPress | ArrowEvRelease ];

type button_event =
  [ ButtonEvEnter of int and int
  | ButtonEvPress of int and int
  | ButtonEvRelease
  | ButtonEvShortcut ]
;
type line_event = unit;
type pack_event = [ PackEvKeyPress of int and keysym ];
type term_event =
  [ TermEvButtonMotion of termEvButton
  | TermEvButtonPress of termEvButton and int
  | TermEvButtonRelease of termEvButton
  | TermEvClearCutBuffer
  | TermEvCutBufferGot of string
  | TermEvKeyPress of int and keysym
  | TermEvSizeChanged ]
;

type widget_desc =
  [ ArrowDesc of
      list Rt.attribute and orientation and widget -> arrow_event -> unit
  | ButtonDesc of
      list Rt.attribute and Rt.button_args and widget -> button_event -> unit
  | LineDesc of list Rt.attribute and widget -> unit -> unit
  | PackDesc of
      list Rt.attribute and
        (direction * list (size_policy * widget_desc)) and
        widget -> pack_event -> unit
  | TermDesc of
      list Rt.attribute and (int * int) and widget -> term_event -> unit ]
;

value rt_xdata_of_xdata xd = xd;

value alert_wid = ref None;
value popup_wid = ref [];
value term_wid = ref [];
value xll = ref 0;
value yll = ref 0;

value rt_attribute_of_rtn =
  fun
  [ BandAtt n -> Rt.BandAtt n
  | HeightAtt n -> Rt.HeightAtt n
  | InterAtt n -> Rt.InterAtt n
  | NameAtt s -> Rt.NameAtt s
  | WidthAtt n -> Rt.WidthAtt n ]
;

value rt_orientation_of_rtn =
  fun
  [ DIRx -> Rt.Horizontal
  | DIRy -> Rt.Vertical
  | DIRz -> Rt.InDepth ]
;

value rt_direction_of_rtn =
  fun
  [ ORup -> Rt.D_up
  | ORdown -> Rt.D_down
  | ORleft -> Rt.D_left
  | ORright -> Rt.D_right ]
;

value rtn_of_rt_button_event =
  fun
  [ Rt.ButtonEvEnter x y ->
      do { xll.val := x; yll.val := y; ButtonEvEnter x y }
  | Rt.ButtonEvPress x y _ ->
      do { xll.val := x; yll.val := y; ButtonEvPress x y }
  | Rt.ButtonEvRelease _ _ _ -> ButtonEvRelease
  | Rt.ButtonEvSizeChange -> failwith "not impl ButtonEvSizeChange"
  | Rt.ButtonEvShortcut -> ButtonEvShortcut ]
;

value int_of_keysym = Rt.int_of_keysym;

value rtn_of_rt_pack_event =
  fun
  [ Rt.PackEvKeyPress mksym ->
      let kmod = if mksym.Rt.mod1 then 0x8 else 0 in
      let ksym = mksym.Rt.item in
      PackEvKeyPress kmod ksym ]
;

value rtn_of_rt_term_event =
  fun
  [ Rt.TermEvButtonMotion row char_col curs_col ->
      TermEvButtonMotion {tEvRow = row; tEvCol = char_col; tEvButton = 0}
  | Rt.TermEvButtonPress row char_col curs_col butt ->
      TermEvButtonPress
        {tEvRow = row; tEvCol = char_col; tEvButton = butt.Rt.item} 0
  | Rt.TermEvButtonRelease row char_col curs_col butt ->
      TermEvButtonRelease
        {tEvRow = row; tEvCol = char_col; tEvButton = butt.Rt.item}
  | Rt.TermEvKeyPress mksym ->
      let kmod = if mksym.Rt.mod1 then 0x8 else 0 in
      let ksym = mksym.Rt.item in
      TermEvKeyPress kmod ksym
  | Rt.TermEvSizeChange _ _ ->
      TermEvSizeChanged
  | Rt.TermEvAnswer _ ->
      failwith "not impl Rt.TermEvAnswer"
  | Rt.TermEvExtendHistory ->
      failwith "not impl Rt.TermEvExtendHistory" ]
;

value ca att = [Rt.BorderAtt 0 :: List.map rt_attribute_of_rtn att];

value arrow_desc att orient act = ArrowDesc (ca att) orient act;
value button_desc att (s, o) act =
  ButtonDesc [Rt.BandAtt 3 :: ca att] (s, o) act
;
value line_desc att () act = LineDesc [Rt.BandAtt 3 :: ca att] act;
value pack_desc att (dir, dl) act = PackDesc (ca att) (dir, dl) act;
value term_desc att (x, y) act =
  TermDesc [Rt.BandAtt 5 :: ca att] (x, y) act
;

value eval_arrow_desc att orient act =
  Rt.arrow_desc att (rt_direction_of_rtn orient) act
;

value eval_button_desc att (s, o) act =
  Rt.button_desc [Rt.NameAtt s :: att] (s, o)
    (fun w ev ->
       match
         try Some (rtn_of_rt_button_event ev) with [ Failure _ -> None ]
       with
       [ Some ev -> act w ev
       | None -> () ])
;

value eval_line_desc att () act = Rt.line_desc att () (fun w ev -> act w ());

value eval_pack_desc att (dir, dl) act =
  let dl = List.map snd dl in
  Rt.pack_desc att (rt_orientation_of_rtn dir, dl)
    (fun w ev -> act w (rtn_of_rt_pack_event ev))
;

value eval_term_desc att (x, y) act =
  Rt.term_desc att (x, y, 0) (fun w ev -> act w (rtn_of_rt_term_event ev))
;

value rec gen_eval_desc is_popup extra_att =
  fun
  [ ArrowDesc attl b c ->
      eval_arrow_desc (attl @ extra_att) b c
  | ButtonDesc attl b c ->
      let attl = if is_popup then [Rt.LeftJustifAtt :: attl] else attl in
      eval_button_desc (attl @ extra_att) b c
  | LineDesc attl b ->
      eval_line_desc (attl @ extra_att) () b
  | PackDesc attl (d, wdl) c ->
      let wdl =
        List.map
          (fun (sp, wd) ->
             let extra_att =
               match sp with
               [ FIXSZ -> []
               | INCSZ -> [Rt.FillerAtt] ]
             in
             let wd = gen_eval_desc is_popup extra_att wd in
             (sp, wd))
          wdl
      in
      eval_pack_desc (attl @ extra_att) (d, wdl) c
  | TermDesc attl b c ->
      eval_term_desc (attl @ extra_att) b c ]
;

value eval_desc is_popup wd = gen_eval_desc is_popup [Rt.BorderAtt 1] wd;

value term_emph_from wid row col = Rt.term_emphasize_from wid row col;

value term_emph_to wid row col = Rt.term_emphasize_to wid row col;

value term_line wid i = do {
  let (nrow, ncol) = Rt.term_size wid in
  Rt.term_emphasize_from wid i 0;
  Rt.term_emphasize_to wid i ncol;
  let str = Rt.term_get_emphasized wid in
  let str =
    let len = String.length str in
    if len > 0 && str.[len - 1] = '\n' then String.sub str 0 (len - 1)
    else str
  in
  if String.length str < ncol then
    str ^ String.make (ncol - String.length str) ' '
  else str
};

value term_send wid str = do {
  let pend_nl =
    try List.assq wid term_wid.val with
    [ Not_found -> do {
        let pend_nl = ref False in
        Rt.term_send wid "\027[20h";
        term_wid.val := [(wid, pend_nl) :: term_wid.val];
        pend_nl
      } ]
  in
  if pend_nl.val then Rt.term_send wid "\n" else ();
  let str =
    let len = String.length str in
    if len > 0 && str.[len - 1] = '\n' then do {
      pend_nl.val := True;
      String.sub str 0 (len - 1)
    }
    else do { pend_nl.val := False; str }
  in
  Rt.term_send wid str
};
value term_get_params = Rt.term_size;
value term_set_params = Rt.term_set_size;
value rt_adjust_widget = Rt.rt_adjust_widget;

value rt_close = Rt.rt_end;
value rt_create_popup_widget wid wdesc = do {
  let wid =
    Rt.rt_create_popup_widget (Rt.xdata_of_widget wid) (eval_desc True wdesc)
  in
  popup_wid.val := [wid :: popup_wid.val];
  wid
};
value rt_create_transient_widget wid name wdel wdesc =
  Rt.rt_create_transient_widget wid name wdel (eval_desc False wdesc)
;
value rt_create_widget xd wname iname x y wdel wd =
  Rt.rt_create_widget xd wname iname Rt.AutoPosition wdel
    (eval_desc False wd)
;
value rt_eq_widget w1 w2 = w1 == w2;
value rt_freeze_widget = Rt.rt_freeze_widget;
value rt_get_cut_buffer wid =
  let str = Rt.rt_get_cut_buffer (Rt.xdata_of_widget wid) in
  Rt.term_send wid str
;
value rt_map_alert wid = do {
  match alert_wid.val with
  [ Some wid -> Rt.rt_unmap_widget wid
  | None -> () ];
  Rt.rt_map_widget wid;
  Rt.rt_raise_widget wid;
  alert_wid.val := Some wid
};
value rt_map_widget wid =
  if List.memq wid popup_wid.val then
    Rt.rt_map_popup_widget wid xll.val yll.val 0
  else do {
    Rt.rt_map_widget wid;
    Rt.rt_raise_widget wid
  }
;
value rt_move_widget = Rt.rt_move_widget;
value rt_open = Rt.rt_initialize;
value rt_root_widget (xd : xdata) = do {
  Rt.rt_select_char_set xd Rt.Utf_8;
  xd
};
value rt_set_cut_buffer wid str =
  Rt.rt_set_cut_buffer (Rt.xdata_of_widget wid) str
;
value rt_treat_one_event xd = Rt.rt_treat_one_event (Rt.rt_args [xd]);
value rt_unfreeze_widget = Rt.rt_unfreeze_widget;
value rt_widget_height = Rt.widget_height;
value rt_widget_named = Rt.widget_named;
value rt_widget_width = Rt.widget_width;
value rt_widget_x = Rt.widget_x;
value rt_widget_y = Rt.widget_y;
value rt_xdata_of_widget = Rt.xdata_of_widget;
value rt_unmap_alert _ = do {
  match alert_wid.val with
  [ Some wid -> Rt.rt_unmap_widget wid
  | None -> () ];
  alert_wid.val := None
};
value rt_unmap_widget = Rt.rt_unmap_widget;

value is_frozen = Rt.is_frozen;
