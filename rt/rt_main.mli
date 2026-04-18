(* $Id: rt_main.mli,v 1.9 2009/02/19 10:19:17 deraugla Exp $ *)

open Rtdecl;

(*** rt version *)

value rt_version : string;

(*** Connection to the display *)

value rt_initialize : string -> xdata;
        (* [rt_initialize dname] initializes the connection to the
           display [dname]; if [dname] is the empty string, the
           function gets the value of the environment variable
           [DISPLAY]; see [rt_end] *)
value rt_end : xdata -> unit;

value rt_select_char_set : xdata -> char_set -> unit;

(*** Widgets *)

value rt_create_widget :
  xdata -> string -> string -> position -> delete_callback -> widget_desc ->
    widget;
      (* [rt_create_widget xd title icon pos wdel wdesc]
         creates a widget on the display [xd], with [title] as
         window title, [icon] as icon text, [pos] as initial position, 
         [wdel] as optional delete handler and [wdesc] as widget
         description. *)
value rt_map_widget : widget -> unit;
      (* [rt_map_widget wid]
         maps the widget [wid] (i.e. make it appear) on the screen;
         see [rt_unmap_widget] *)
value rt_unmap_widget : widget -> unit;
      (* [rt_unmap_widget wid]
         unmaps the widget [wid] (i.e. make it disappear) from the
         screen; see [rt_map_widget] *)
value rt_raise_widget : widget -> unit;
      (* [rt_raise_widget wid]
         moves the widget [wid] on the top of the stacking order among
         its siblings (usefull for pack subwidget, when the orientation
         is InDepth) *)
value rt_destroy_widget : widget -> unit;
      (* [rt_destroy_widget wid]
         kills definively the widget [wid] and its subwidgets, if any *)
value rt_freeze_widget : widget -> unit;
      (* [rt_freeze_widget wid]
         freezes the widget [wid] and all its subwidgets, if any;
         some events are not yet reported until the widget is unfrozen;
         depending on the widget kind, the look of the widget may change:
         e.g. in a button widget, the text becomes gray; see
         [rt_unfreeze_widget] *)
value rt_unfreeze_widget : widget -> unit;
      (* [rt_unfreeze_widget wid]
         unfreezes the widget [wid] and all its subwidgets, if any;
         see [rt_freeze_widget] *)
value rt_adjust_widget : widget -> (int * int);
value rt_change_background : widget -> paint -> unit;
value rt_change_widget_name : widget -> string -> string -> unit;

(*** Events handling *)

value rt_args : list xdata -> xargs;
      (* [rt_args xdl]
         returns an [xargs] object, representing the [xdata] objects
         selected, without timeout nor file descriptor selected;
         used by the following functions *)
value rt_select_xdata : xargs -> xdata -> unit;
      (* [rt_select_xdata xa xd]
         adds [xd] among [xdatas] selected by [xa]; see
         [rt_unselect_xdata] *)
value rt_unselect_xdata : xargs -> xdata -> unit;
      (* [rt_unselect_xdata xa xd]
         removes [xd] from the list of [xdatas] selected by [xa];
         see [rt_select_xdata] *)
value rt_select_file_descr : xargs -> int -> (int -> unit) -> unit;
      (* [rt_select_file_descr xa fd f] Adds the file descriptor [fd] among
         the file descriptors selected by [xa]; when the file descriptor
         is ready to be read without being blocked, the fuction [f] is
         called with the file descriptor [fd]. Useful for reading in
         standard input of in a socket. *)
value rt_select_file : xargs -> int -> (string -> int -> unit) -> unit;
      (* Old version of the function [rt_select_file_descr] for file
         descriptors corresponding to a file, standard input or a
         connected socket. The user function is called with a string
         buffer containing the read characters and their number. *)
value rt_unselect_file : xargs -> int -> unit;
      (* [rt_unselect_file xa fd]
         removes [fd] from the list of file descriptors handled by
         [xa]; see [rt_select_file] *)
value rt_set_timeout : xargs -> string -> int -> (unit -> unit) -> unit;
      (* [rt_set_timeout xa name tm f] adds a timeout action; at time [tm],
         the function [f] is called; the current time is obtained using the
         function [rt_current_time] (see below); the unit is the millisecond;
         for example to have a timeout after one second, write:
         [rt_set_timeout xa name (rt_current_time xa + 1000) f]; the timeouts
         are named to offer the possibility to cancel them. See
         [rt_cancel_timeout] *)
value rt_cancel_timeout : xargs -> string -> unit;
      (* [rt_cancel_timeout name xa] cancels all timeouts timeout named name,
         if any. *)
value rt_current_time : xargs -> int;
      (* [rt_current_time xa]
         returns the current time, in milliseconds; what is called
         ``current time'' is the time of the beginning of the current
         callback function; calling [rt_current_time] many times
         returns always the same value until the end of the current
         callback, even if this callback is long; this time is
         relative to some beginning of the toolkit event handling;
         so, you cannot use it to adjust your watch! *)
value rt_immediate_time : xargs -> int;
value rt_main_loop : xargs -> unit;
      (* [rt_main_loop xa]
         is an infinite loop waiting for events on selected displays,
         file descriptors and timeouts in [xa] *)
value rt_stop_main_loop : xargs -> unit;
      (* [rt_stop_main_loop xa]
         stops the main loop at the end of the current callback
         (the programs will continue after the call to [rt_main_loop] *)
value rt_treat_one_event : xargs -> unit;
      (* [rt_treat_one_event xa]
         treats just one event, calling or not a user function *)
value rt_pending_events : xargs -> bool;
      (* [rt_pending_events xa] tells whether events are still pending
         (not yet treated); this includes user events (calling user functions)
         or system events (remaining in the toolkit internal functions);
         if the answer is [true], it is guarantied that [rt_treat_one_event]
         does not block. *)
value rt_treat_pending_events : xargs -> unit;

(*** Informations about displays *)

value rt_display_name : string -> string;
      (* [rt_display_name dname]
         returns the actual display connection name that would be
         used by [rt_initialize] or [rt_run]; that means that, if
         [dname] is the empty string, returns the value of the
         environnement variable [DISPLAY] *)
value is_colored : xdata -> bool;
      (* [is_colored xd]
         tells whether [xd] is a colored screen *)
value screen_width : xdata -> int;
      (* [screen_width xd]
         returns the width of the screen [xd] *)
value screen_height : xdata -> int;
      (* [screen_height xd]
         returns the height of the screen [xd] *)
value screen_depth : xdata -> int;
      (* [screen_depth xd]
         returns the depth of the screen [xd]; a black and white screen
         has a depth 1, a colored screen with 256 colors has a depth 8;
         the depth is the number of bits by pixels in the screen *)
value screen_size_mm : xdata -> (int * int);
      (* [screen_size_mm xd]
         returns (width, height) of the screen [xd] in millimeters *)
value xdata_of_widget : widget -> xdata;
      (* [xdata_of_widget wid]
         returns the [xdata] where the widget [wid] is *)
value xdata_of_pixmap : pixmap -> xdata;

(*** Informations about widgets *)

value widget_x : widget -> int;
      (* [widget_x wid]
         returns the [x] position of the widget [wid] relative to its
         parent *)
value widget_y : widget -> int;
      (* [widget_x wid]
         returns the [y] position of the widget [wid] relative to its
         parent *)
value widget_width : widget -> int;
      (* [widget_width wid]
         returns the width of the widget (this is the internal width,
         not including the border) *)
value widget_height : widget -> int;
      (* [widget_height wid]
         returns the height of the widget (this is the internal height,
         not including the border) *)
value widget_border : widget -> int;
      (* [widget_border wid]
         returns the border width of the widget *)
value widget_children : widget -> list widget;
      (* Returns the children widgets of the widget. *)
value widget_named : xdata -> string -> widget;
      (* [widget_named xd name]
         returns the widget named [name] in the xdata [xd]; see the
         attribute [NameAtt] *)
value pixmap_width : pixmap -> int;
value pixmap_height : pixmap -> int;
value is_mapped : widget -> bool;
      (* [is_mapped wid]
         tells whether the widget is mapped (visible on the screen)
         or not *)
value is_frozen : widget -> bool;
      (* [is_frozen wid]
         tells whether the widget is frozen or not *)
value widget_size : xdata -> widget_desc -> (int * int);
