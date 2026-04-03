(* $Id: c_table.mli,v 1.7 2015/06/22 09:35:36 deraugla Exp $ *)

open Rtdecl;

(*** Reported events *)

type table_event = [ NoTableEvent ];

(*** Table widget builder *)

type table_args = list (list table_data)
and table_data = [ TD of list table_data_attr and widget_desc ]
and table_data_attr = [ Colspan of int ];

type table_event_handler = widget -> table_event -> unit;

value table_desc :
  list attribute -> table_args -> table_event_handler -> widget_desc;

value table_insert_row : widget -> int -> list table_data -> unit;
   (* Add a row at given position in the table with the list of table_data.
      Need a call to [rt_adjust_widget] on the top window it belongs to to
      terminate the job. *)
value table_remove_nth_row : widget -> int -> unit;
   (* Remove the n-th row of the table (first is zero). Need a call to
      [rt_adjust_widget] on the top window it belongs to to terminate the
      job. *)

value table_add_row : widget -> list table_data -> unit;
   (* Add a row at table bottom with the list of table_data. Equivalent to
      [table_insert_row] with its current number of rows. Need a call
      to [rt_adjust_widget] on the top window it belongs to to terminate
      the job. *)
