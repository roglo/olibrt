
(*** Main (abstract) types *)

type xdata = 'a;
type widget = 'a;
type widget_desc = 'a;
type xargs = 'a;
type color = 'a;
type pixmap = 'a;
type mouse = 'a;
type font = 'a;
type gc = 'a;
type user_info = 'a;
type delete_callback = option (widget -> unit);
type char_set = [ Latin_1 | Utf_8 ];

(*** Paint *)

type paint = [ NonePn | PixmapPn of pixmap | ColorPn of color ];

(*** Drawable *)

type drawable = [ WidgetDr of widget | PixmapDr of pixmap ];

(*** Orientation *)

type orientation = [ Horizontal | Vertical | InDepth ];

(*** Direction *)

type direction = [ D_up | D_down | D_left | D_right ];

(*** Widget positioning *)

type position = [ AutoPosition | UserPosition of int and int ];

(*** Widget attributes *)

type attribute =
  [ BackgroundAtt of paint
  | BandAtt of int
  | BoldAtt of int
  | BorderAtt of int
  | BorderBackgAtt of paint
  | FillerAtt
  | FontAtt of array font
  | ForegroundAtt of paint
  | HeightAtt of int
  | InterAtt of int
  | LeftJustifAtt
  | NameAtt of string
  | WidthAtt of int ]
;

(*** Modified keys or buttons *)

type modified 'a =
  { shiftMod : bool;
    shiftLock : bool;
    control : bool;
    mod1 : bool;
    item : 'a }
;

(* $Id: rt.ml,v 1.49 2015/06/22 09:35:36 deraugla Exp $ *)

(***** Keysym *)
(* $Id: rt.ml,v 1.49 2015/06/22 09:35:36 deraugla Exp $ *)



type keysym =
  [ K_Ascii of char
  | K_Alt_L
  | K_Alt_R
  | K_BackSpace
  | K_Begin
  | K_Break
  | K_Cancel
  | K_Caps_Lock
  | K_Clear
  | K_Control_L
  | K_Control_R
  | K_Delete
  | K_Down
  | K_End
  | K_Escape
  | K_Execute
  | K_F of int
  | K_Find
  | K_Help
  | K_Home
  | K_Hyper_L
  | K_Hyper_R
  | K_Insert
  | K_KP_Add
  | K_KP_Decimal
  | K_KP_Divide
  | K_KP_Enter
  | K_KP_Equal
  | K_KP_F of int
  | K_KP_Multiply
  | K_KP_N of int
  | K_KP_Separator
  | K_KP_Space
  | K_KP_Subtract
  | K_KP_Tab
  | K_Left
  | K_Linefeed
  | K_Menu
  | K_Meta_L
  | K_Meta_R
  | K_Mode_switch
  | K_Multi_key
  | K_Next
  | K_Num_Lock
  | K_Pause
  | K_Print
  | K_Prior
  | K_Redo
  | K_Return
  | K_Right
  | K_Scroll_Lock
  | K_Select
  | K_Shift_L
  | K_Shift_Lock
  | K_Shift_R
  | K_Super_L
  | K_Super_R
  | K_Tab
  | K_Undo
  | K_Up
  | K_Other of int ]
;

(*** Reported events *)

type arrow_event = [ ArrowEvPress | ArrowEvRelease ];

(*** Arrow widget builder *)

type arrow_args = direction;
type arrow_event_handler = widget -> arrow_event -> unit;


(***** Button widget *)
(* $Id: rt.ml,v 1.49 2015/06/22 09:35:36 deraugla Exp $ *)



(*
    Buttons are widgets holding a non mutable text, sensitive to mouse
    button press and release. When a mouse button is pressed in the widget,
    the button appears in inverse video. Useful to trigger a specific action,
    or pop up a menu.
 *)

(*** Reported events *)

type button_event =
  [ ButtonEvEnter of int and int
  | ButtonEvPress of int and int and modified int
  | ButtonEvRelease of int and int and modified int
  | ButtonEvSizeChange
  | ButtonEvShortcut ]
;

(*** Button widget builder *)

type button_args = (string * option char);
type button_event_handler = widget -> button_event -> unit;

(***** Check widget *)

type check_event = [ CheckEvPress | CheckEvRelease ];
type check_args = unit;
type check_event_handler = widget -> check_event -> unit;

(***** Line widget *)

(*** Reported events *)

type line_event = [ NoLineEvent ];

(*** Line widget builder *)

type line_args = unit;
type line_event_handler = widget -> line_event -> unit;


(***** Pack widget *)

type pack_event = [ PackEvKeyPress of modified keysym ];

(*** Pack widget builder *)

type pack_args = (orientation * list widget_desc);
type pack_event_handler = widget -> pack_event -> unit;


(***** Scroll widget *)

(*** Reported events *)

type scroll_event =
  [ ScrollEvArrowPress of int
  | ScrollEvArrowRelease of int
  | ScrollEvButtonPress of modified int and int
  | ScrollEvButtonRelease
  | ScrollEvButtonMotion of int
  | ScrollEvSizeChange ]
;

(*** Scroll widget builder *)

type scroll_args = (orientation * int * int * int);
type scroll_event_handler = widget -> scroll_event -> unit;

(*** Table widget builder *)

type table_event = [ NoTableEvent ];
type table_args = list (list table_data)
and table_data = [ TD of list table_data_attr and widget_desc ]
and table_data_attr = [ Colspan of int ];
type table_event_handler = widget -> table_event -> unit;

(***** Term widget *)

(*** Reported events *)

type term_event =
  [ TermEvButtonPress of int and int and int and modified int
  | TermEvButtonRelease of int and int and int and modified int
  | TermEvButtonMotion of int and int and int
  | TermEvKeyPress of modified keysym
  | TermEvAnswer of string
  | TermEvExtendHistory
  | TermEvSizeChange of int and int ]
;

(*** Term widget builder *)

type term_args = (int * int * int);
type term_event_handler = widget -> term_event -> unit;

(*** Title widget builder *)

type title_event = [ TitleEvButtonPress of modified int ];
type title_args = string;
type title_event_handler = widget -> title_event -> unit;

(*** Event selection *)

type event_selection =
  [ SelButtonMotion
  | SelButtonPress
  | SelButtonRelease
  | SelEnterWindow
  | SelExposure
  | SelFocusChange
  | SelKeyPress
  | SelLeaveWindow
  | SelPointerMotion
  | SelStructureNotify ]
;

(*** Reported event *)

type raw_event =
  [ RawEvButtonPress of int and int and int and int and modified int
  | RawEvButtonRelease of int and int and int and int and modified int
  | RawEvConfigureNotify of (int * int * int) and (int * int * int)
  | RawEvEnterNotify of int and int and int and int
  | RawEvExpose of int and int and int and int
  | RawEvFocusIn
  | RawEvFocusOut
  | RawEvGraphicsExpose of int and int and int and int and int
  | RawEvKeyPress of modified keysym
  | RawEvLeaveNotify
  | RawEvMotionNotify of int and int
  | RawEvNoExpose ]
;

(*** Raw widget builder *)

type raw_args = (int * int * int * list event_selection);
type raw_event_handler = widget -> raw_event -> unit;

(***** Color *)
(* $Id: rt.ml,v 1.49 2015/06/22 09:35:36 deraugla Exp $ *)

type pattern = 'a;

type image = 'a;

type uoption 'a = [ UNone | USome of 'a ];
type user_info_func 'a = ('a -> user_info * widget -> 'a);

(***** Time *)
(* $Id: rt.ml,v 1.49 2015/06/22 09:35:36 deraugla Exp $ *)

type timeb = { time : int; millitm : int };

value string_of_keysym = Obj.magic Keysym.string_of_keysym;
value is_modifier = Obj.magic Keysym.is_modifier;
value keysym_of_keycode = Obj.magic Keysym.keysym_of_keycode;
value keysym_of_keysym_int = Obj.magic Keysym.keysym_of_keysym_int;
value int_of_keysym = Obj.magic Keysym.int_of_keysym;
value rt_version = Obj.magic Rt_main.rt_version;
value rt_initialize = Obj.magic Rt_main.rt_initialize;
value rt_end = Obj.magic Rt_main.rt_end;
value rt_select_char_set = Obj.magic Rt_main.rt_select_char_set;
value rt_create_widget = Obj.magic Rt_main.rt_create_widget;
value rt_map_widget = Obj.magic Rt_main.rt_map_widget;
value rt_unmap_widget = Obj.magic Rt_main.rt_unmap_widget;
value rt_raise_widget = Obj.magic Rt_main.rt_raise_widget;
value rt_destroy_widget = Obj.magic Rt_main.rt_destroy_widget;
value rt_freeze_widget = Obj.magic Rt_main.rt_freeze_widget;
value rt_unfreeze_widget = Obj.magic Rt_main.rt_unfreeze_widget;
value rt_adjust_widget = Obj.magic Rt_main.rt_adjust_widget;
value rt_change_background = Obj.magic Rt_main.rt_change_background;
value rt_change_widget_name = Obj.magic Rt_main.rt_change_widget_name;
value rt_args = Obj.magic Rt_main.rt_args;
value rt_select_xdata = Obj.magic Rt_main.rt_select_xdata;
value rt_unselect_xdata = Obj.magic Rt_main.rt_unselect_xdata;
value rt_select_file_descr = Obj.magic Rt_main.rt_select_file_descr;
value rt_select_file = Obj.magic Rt_main.rt_select_file;
value rt_unselect_file = Obj.magic Rt_main.rt_unselect_file;
value rt_set_timeout = Obj.magic Rt_main.rt_set_timeout;
value rt_cancel_timeout = Obj.magic Rt_main.rt_cancel_timeout;
value rt_current_time = Obj.magic Rt_main.rt_current_time;
value rt_immediate_time = Obj.magic Rt_main.rt_immediate_time;
value rt_main_loop = Obj.magic Rt_main.rt_main_loop;
value rt_stop_main_loop = Obj.magic Rt_main.rt_stop_main_loop;
value rt_treat_one_event = Obj.magic Rt_main.rt_treat_one_event;
value rt_pending_events = Obj.magic Rt_main.rt_pending_events;
value rt_treat_pending_events = Obj.magic Rt_main.rt_treat_pending_events;
value rt_display_name = Obj.magic Rt_main.rt_display_name;
value is_colored = Obj.magic Rt_main.is_colored;
value screen_width = Obj.magic Rt_main.screen_width;
value screen_height = Obj.magic Rt_main.screen_height;
value screen_depth = Obj.magic Rt_main.screen_depth;
value xdata_of_widget = Obj.magic Rt_main.xdata_of_widget;
value xdata_of_pixmap = Obj.magic Rt_main.xdata_of_pixmap;
value widget_x = Obj.magic Rt_main.widget_x;
value widget_y = Obj.magic Rt_main.widget_y;
value widget_width = Obj.magic Rt_main.widget_width;
value widget_height = Obj.magic Rt_main.widget_height;
value widget_border = Obj.magic Rt_main.widget_border;
value widget_children = Obj.magic Rt_main.widget_children;
value widget_named = Obj.magic Rt_main.widget_named;
value is_mapped = Obj.magic Rt_main.is_mapped;
value is_frozen = Obj.magic Rt_main.is_frozen;
value widget_size = Obj.magic Rt_main.widget_size;
value arrow_desc = Obj.magic C_arrow.arrow_desc;
value button_desc = Obj.magic C_button.button_desc;
value button_border = Obj.magic C_button.button_border;
value button_band = Obj.magic C_button.button_band;
value button_bold = Obj.magic C_button.button_bold;
value button_bold_band = Obj.magic C_button.button_bold_band;
value button_font = Obj.magic C_button.button_font;
value check_desc = Obj.magic C_check.check_desc;
value check_set = Obj.magic C_check.check_set;
value check_val = Obj.magic C_check.check_val;
value line_desc = Obj.magic C_line.line_desc;
value menu_button = Obj.magic C_button.menu_button;
value pack_desc = Obj.magic C_pack.pack_desc;
value pack_extend = Obj.magic C_pack.pack_extend;
value pack_remove_nth = Obj.magic C_pack.pack_remove_nth;
value pack_border = Obj.magic C_pack.pack_border;
value pack_band = Obj.magic C_pack.pack_band;
value pack_inter = Obj.magic C_pack.pack_inter;
value scroll_set_bar_size = Obj.magic C_scroll.scroll_set_bar_size;
value scroll_desc = Obj.magic C_scroll.scroll_desc;
value scroll_set = Obj.magic C_scroll.scroll_set;
value scroll_val = Obj.magic C_scroll.scroll_val;
value table_desc = Obj.magic C_table.table_desc;
value table_add_row = Obj.magic C_table.table_add_row;
value table_insert_row = Obj.magic C_table.table_insert_row;
value table_remove_nth_row = Obj.magic C_table.table_remove_nth_row;
value term_desc = Obj.magic C_term.term_desc;
value term_size = Obj.magic C_term.term_size;
value term_set_size = Obj.magic C_term.term_set_size;
value term_set_max_history_size = Obj.magic C_term.term_set_max_history_size;
value term_current_position = Obj.magic C_term.term_current_position;
value term_string_of_keysym = Obj.magic C_term.term_string_of_keysym;
value term_history_size = Obj.magic C_term.term_history_size;
value term_shift = Obj.magic C_term.term_shift;
value term_shift_value = Obj.magic C_term.term_shift_value;
value term_emphasize_from = Obj.magic C_term.term_emphasize_from;
value term_emphasize_to = Obj.magic C_term.term_emphasize_to;
value term_emphasized_location = Obj.magic C_term.term_emphasized_location;
value term_get_emphasized = Obj.magic C_term.term_get_emphasized;
value term_font = Obj.magic C_term.term_font;
value term_border = Obj.magic C_term.term_border;
value term_inter = Obj.magic C_term.term_inter;
value term_band = Obj.magic C_term.term_band;
value term_blink = Obj.magic C_term.term_blink;
value term_send = Obj.magic Term.term_send;
value title_change = Obj.magic C_title.title_change;
value title_desc = Obj.magic C_title.title_desc;
value title_border = Obj.magic C_title.title_border;
value title_band = Obj.magic C_title.title_band;
value title_font = Obj.magic C_title.title_font;
value raw_desc = Obj.magic C_raw.raw_desc;
value raw_set_size = Obj.magic C_raw.raw_set_size;
value rt_create_pixmap = Obj.magic Pixmap.rt_create_pixmap;
value rt_select_pixmap = Obj.magic Pixmap.rt_select_pixmap;
value color_pixel = Obj.magic Rt_color.color_pixel;
value rt_change_color = Obj.magic Rt_color.rt_change_color;
value rt_white_color = Obj.magic Rt_color.rt_white_color;
value rt_black_color = Obj.magic Rt_color.rt_black_color;
value rt_create_color = Obj.magic Rt_color.rt_create_color;
value rt_create_gc_with_color = Obj.magic Rt_color.rt_create_gc_with_color;
value rt_closest_color = Obj.magic Rt_color.rt_closest_color;
value rt_select_color = Obj.magic Rt_color.rt_select_color;
value rt_select_background_color =
  Obj.magic Rt_color.rt_select_background_color
;
value rt_query_color = Obj.magic Rt_color.rt_query_color;
value rt_free_color = Obj.magic Rt_color.rt_free_color;
value rgb_of_hsv = Obj.magic Rt_color.rgb_of_hsv;
value rt_create_pattern = Obj.magic Pattern.rt_create_pattern;
value rt_select_pattern = Obj.magic Pattern.rt_select_pattern;
value rt_select_pattern_mask = Obj.magic Pattern.rt_select_pattern_mask;
value rt_select_pattern_clip = Obj.magic Pattern.rt_select_pattern_clip;
value rt_unselect_pattern_clip = Obj.magic Pattern.rt_unselect_pattern_clip;
value rt_draw_point = Obj.magic Draw.rt_draw_point;
value rt_draw_point_with_gc = Obj.magic Draw.rt_draw_point_with_gc;
value rt_draw_line = Obj.magic Draw.rt_draw_line;
value rt_draw_lines = Obj.magic Draw.rt_draw_lines;
value rt_fill_polygon = Obj.magic Draw.rt_fill_polygon;
value rt_fill_rectangle = Obj.magic Draw.rt_fill_rectangle;
value rt_draw_rectangle = Obj.magic Draw.rt_draw_rectangle;
value rt_fill_arc = Obj.magic Draw.rt_fill_arc;
value rt_draw_arc = Obj.magic Draw.rt_draw_arc;
value rt_clear_widget = Obj.magic Draw.rt_clear_widget;
value rt_clear_area = Obj.magic Draw.rt_clear_area;
value rt_draw_string = Obj.magic Draw.rt_draw_string;
value rt_erase_draw_string = Obj.magic Draw.rt_erase_draw_string;
value rt_copy_area = Obj.magic Draw.rt_copy_area;
value rt_set_line_width = Obj.magic Draw.rt_set_line_width;
value rt_set_dashes = Obj.magic Draw.rt_set_dashes;
value rt_set_backing_store = Obj.magic Draw.rt_set_backing_store;
value rt_create_image = Obj.magic Rt_image.rt_create_image;
value rt_put_image = Obj.magic Rt_image.rt_put_image;
value rt_get_image = Obj.magic Rt_image.rt_get_image;
value rt_get_pixel = Obj.magic Rt_image.rt_get_pixel;
value rt_put_pixel = Obj.magic Rt_image.rt_put_pixel;
value rt_image_data_len = Obj.magic Rt_image.rt_image_data_len;
value rt_create_bitmap_mouse = Obj.magic Mouse.rt_create_bitmap_mouse;
value rt_create_font_mouse = Obj.magic Mouse.rt_create_font_mouse;
value rt_select_mouse = Obj.magic Mouse.rt_select_mouse;
value rt_unselect_mouse = Obj.magic Mouse.rt_unselect_mouse;
value rt_load_query_font = Obj.magic Font.rt_load_query_font;
value rt_select_font = Obj.magic Font.rt_select_font;
value rt_text_width = Obj.magic Font.rt_text_width;
value rt_font_size = Obj.magic Font.rt_font_size;
value rt_get_default = Obj.magic Resource.rt_get_default;
value rt_create_subwidget = Obj.magic Misc.rt_create_subwidget;
value rt_move_widget = Obj.magic Misc.rt_move_widget;
value rt_resize_widget = Obj.magic Misc.rt_resize_widget;
value rt_move_resize_widget = Obj.magic Misc.rt_move_resize_widget;
value rt_reparent_widget = Obj.magic Misc.rt_reparent_widget;
value rt_create_transient_widget = Obj.magic Misc.rt_create_transient_widget;
value rt_map_transient_widget = Obj.magic Misc.rt_map_transient_widget;
value rt_create_popup_widget = Obj.magic Misc.rt_create_popup_widget;
value rt_map_popup_widget = Obj.magic Misc.rt_map_popup_widget;
value rt_sync = Obj.magic Misc.rt_sync;
value rt_flush = Obj.magic Misc.rt_flush;
value rt_query_pointer = Obj.magic Misc.rt_query_pointer;
value rt_get_bell_params = Obj.magic Misc.rt_get_bell_params;
value rt_set_bell_params = Obj.magic Misc.rt_set_bell_params;
value rt_bell = Obj.magic Misc.rt_bell;
value rt_get_cut_buffer = Obj.magic Misc.rt_get_cut_buffer;
value rt_set_cut_buffer = Obj.magic Misc.rt_set_cut_buffer;
value rt_set_win_size = Obj.magic Misc.rt_set_win_size;
value rt_redirect_key_press_to = Obj.magic Misc.rt_redirect_key_press_to;
value rt_dont_redirect_key_press = Obj.magic Misc.rt_dont_redirect_key_press;
value rt_dont_redirect_key_press_from =
  Obj.magic Misc.rt_dont_redirect_key_press_from
;
value user_info x = Obj.magic Uinfo.user_info x;
value rt_set_user_info = Obj.magic Uinfo.rt_set_user_info;
value ftime = Obj.magic Time.ftime;
value timeb_sub = Obj.magic Time.timeb_sub;
value xC_num_glyphs = Obj.magic Mousefont.xC_num_glyphs;
value xC_X_cursor = Obj.magic Mousefont.xC_X_cursor;
value xC_arrow = Obj.magic Mousefont.xC_arrow;
value xC_based_arrow_down = Obj.magic Mousefont.xC_based_arrow_down;
value xC_based_arrow_up = Obj.magic Mousefont.xC_based_arrow_up;
value xC_boat = Obj.magic Mousefont.xC_boat;
value xC_bogosity = Obj.magic Mousefont.xC_bogosity;
value xC_bottom_left_corner = Obj.magic Mousefont.xC_bottom_left_corner;
value xC_bottom_right_corner = Obj.magic Mousefont.xC_bottom_right_corner;
value xC_bottom_side = Obj.magic Mousefont.xC_bottom_side;
value xC_bottom_tee = Obj.magic Mousefont.xC_bottom_tee;
value xC_box_spiral = Obj.magic Mousefont.xC_box_spiral;
value xC_center_ptr = Obj.magic Mousefont.xC_center_ptr;
value xC_circle = Obj.magic Mousefont.xC_circle;
value xC_clock = Obj.magic Mousefont.xC_clock;
value xC_coffee_mug = Obj.magic Mousefont.xC_coffee_mug;
value xC_cross = Obj.magic Mousefont.xC_cross;
value xC_cross_reverse = Obj.magic Mousefont.xC_cross_reverse;
value xC_crosshair = Obj.magic Mousefont.xC_crosshair;
value xC_diamond_cross = Obj.magic Mousefont.xC_diamond_cross;
value xC_dot = Obj.magic Mousefont.xC_dot;
value xC_dotbox = Obj.magic Mousefont.xC_dotbox;
value xC_double_arrow = Obj.magic Mousefont.xC_double_arrow;
value xC_draft_large = Obj.magic Mousefont.xC_draft_large;
value xC_draft_small = Obj.magic Mousefont.xC_draft_small;
value xC_draped_box = Obj.magic Mousefont.xC_draped_box;
value xC_exchange = Obj.magic Mousefont.xC_exchange;
value xC_fleur = Obj.magic Mousefont.xC_fleur;
value xC_gobbler = Obj.magic Mousefont.xC_gobbler;
value xC_gumby = Obj.magic Mousefont.xC_gumby;
value xC_hand1 = Obj.magic Mousefont.xC_hand1;
value xC_hand2 = Obj.magic Mousefont.xC_hand2;
value xC_heart = Obj.magic Mousefont.xC_heart;
value xC_icon = Obj.magic Mousefont.xC_icon;
value xC_iron_cross = Obj.magic Mousefont.xC_iron_cross;
value xC_left_ptr = Obj.magic Mousefont.xC_left_ptr;
value xC_left_side = Obj.magic Mousefont.xC_left_side;
value xC_left_tee = Obj.magic Mousefont.xC_left_tee;
value xC_leftbutton = Obj.magic Mousefont.xC_leftbutton;
value xC_ll_angle = Obj.magic Mousefont.xC_ll_angle;
value xC_lr_angle = Obj.magic Mousefont.xC_lr_angle;
value xC_man = Obj.magic Mousefont.xC_man;
value xC_middlebutton = Obj.magic Mousefont.xC_middlebutton;
value xC_mouse = Obj.magic Mousefont.xC_mouse;
value xC_pencil = Obj.magic Mousefont.xC_pencil;
value xC_pirate = Obj.magic Mousefont.xC_pirate;
value xC_plus = Obj.magic Mousefont.xC_plus;
value xC_question_arrow = Obj.magic Mousefont.xC_question_arrow;
value xC_right_ptr = Obj.magic Mousefont.xC_right_ptr;
value xC_right_side = Obj.magic Mousefont.xC_right_side;
value xC_right_tee = Obj.magic Mousefont.xC_right_tee;
value xC_rightbutton = Obj.magic Mousefont.xC_rightbutton;
value xC_rtl_logo = Obj.magic Mousefont.xC_rtl_logo;
value xC_sailboat = Obj.magic Mousefont.xC_sailboat;
value xC_sb_down_arrow = Obj.magic Mousefont.xC_sb_down_arrow;
value xC_sb_h_double_arrow = Obj.magic Mousefont.xC_sb_h_double_arrow;
value xC_sb_left_arrow = Obj.magic Mousefont.xC_sb_left_arrow;
value xC_sb_right_arrow = Obj.magic Mousefont.xC_sb_right_arrow;
value xC_sb_up_arrow = Obj.magic Mousefont.xC_sb_up_arrow;
value xC_sb_v_double_arrow = Obj.magic Mousefont.xC_sb_v_double_arrow;
value xC_shuttle = Obj.magic Mousefont.xC_shuttle;
value xC_sizing = Obj.magic Mousefont.xC_sizing;
value xC_spider = Obj.magic Mousefont.xC_spider;
value xC_spraycan = Obj.magic Mousefont.xC_spraycan;
value xC_star = Obj.magic Mousefont.xC_star;
value xC_target = Obj.magic Mousefont.xC_target;
value xC_tcross = Obj.magic Mousefont.xC_tcross;
value xC_top_left_arrow = Obj.magic Mousefont.xC_top_left_arrow;
value xC_top_left_corner = Obj.magic Mousefont.xC_top_left_corner;
value xC_top_right_corner = Obj.magic Mousefont.xC_top_right_corner;
value xC_top_side = Obj.magic Mousefont.xC_top_side;
value xC_top_tee = Obj.magic Mousefont.xC_top_tee;
value xC_trek = Obj.magic Mousefont.xC_trek;
value xC_ul_angle = Obj.magic Mousefont.xC_ul_angle;
value xC_umbrella = Obj.magic Mousefont.xC_umbrella;
value xC_ur_angle = Obj.magic Mousefont.xC_ur_angle;
value xC_watch = Obj.magic Mousefont.xC_watch;
value xC_xterm = Obj.magic Mousefont.xC_xterm;
