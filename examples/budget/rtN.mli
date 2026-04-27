type xdata = 'abstract;
type widget = 'abstract;
type widget_desc = 'abstract;
type delete_callback = option (widget -> unit);
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
type arrow_event = [ ArrowEvPress | ArrowEvRelease ];
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

value rt_xdata_of_xdata : xdata -> Rt.xdata;

value int_of_keysym : keysym -> int;

value arrow_desc :
  list attribute -> orientation -> (widget -> arrow_event -> unit) ->
    widget_desc;
value button_desc :
  list attribute -> Rt.button_args -> (widget -> button_event -> unit) ->
    widget_desc;
value line_desc :
  list attribute -> unit -> (widget -> unit -> unit) -> widget_desc;
value pack_desc :
  list attribute -> (direction * list (size_policy * widget_desc)) ->
    (widget -> pack_event -> unit) -> widget_desc;
value term_desc :
  list attribute -> (int * int) -> (widget -> term_event -> unit) ->
    widget_desc;
value term_emph_from : widget -> int -> int -> unit;
value term_emph_to : widget -> int -> int -> unit;
value term_line : widget -> int -> string;
value term_send : widget -> string -> unit;
value term_get_params : widget -> (int * int);
value term_set_params : widget -> (int * int) -> unit;
value rt_adjust_widget : widget -> (int * int);
value rt_close : xdata -> unit;
value rt_create_popup_widget : widget -> widget_desc -> widget;
value rt_create_transient_widget :
  widget -> string -> delete_callback -> widget_desc -> widget;
value rt_create_widget :
  xdata -> string -> string -> 'a -> 'b -> delete_callback -> widget_desc ->
    widget;
value rt_eq_widget : widget -> widget -> bool;
value rt_freeze_widget : widget -> unit;
value rt_get_cut_buffer : widget -> unit;
value rt_map_alert : widget -> unit;
value rt_map_widget : widget -> unit;
value rt_move_widget : widget -> int -> int -> unit;
value rt_open : string -> xdata;
value rt_root_widget : xdata -> xdata;
value rt_set_cut_buffer : widget -> string -> unit;
value rt_treat_one_event : xdata -> unit;
value rt_unfreeze_widget : widget -> unit;
value rt_widget_height : widget -> int;
value rt_widget_named : xdata -> string -> widget;
value rt_widget_width : widget -> int;
value rt_widget_x : widget -> int;
value rt_widget_y : widget -> int;
value rt_xdata_of_widget : widget -> xdata;
value rt_unmap_alert : 'a -> unit;
value rt_unmap_widget : widget -> unit;

value is_frozen : widget -> bool;
