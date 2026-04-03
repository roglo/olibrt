(* $Id: c_table.ml,v 1.27 2017/05/14 01:11:49 deraugla Exp $ *)

open Rtdecl;
open Xlib;

type table_event = [ NoTableEvent ];
type table_args = list (list table_data)
and table_data = [ TD of list table_data_attr and widget_desc ]
and table_data_attr = [ Colspan of int ];
type table_event_handler = widget -> table_event -> unit;

type table_local_info = { widll : mutable list (list (widget * int)) };

value (table_local_info, get_table_local_info) =
  Util.dynamo_local_info "table_local_info" (ref None)
;

value (table_args, get_table_args) =
  Util.dynamo_args_info "table_args_info" (ref None)
;

value max_size_of_dim xd wshaa = do {
  let nrow = Array.length wshaa in
  let ncol =
    Array.fold_left (fun m wsha -> max m (Array.length wsha)) 0 wshaa
  in
  let max_height_of_row = Array.make nrow 1 in
  let max_width_of_col = Array.make ncol 1 in
  for i = 0 to nrow - 1 do {
    for j = 0 to ncol - 1 do {
      if j < Array.length wshaa.(i) then do {
        match wshaa.(i).(j) with
        [ Some (wsh, colspan) -> do {
            max_height_of_row.(i) :=
              max max_height_of_row.(i) (wsh.sh_height + 2 * wsh.sh_border);
            if colspan = 1 then
              max_width_of_col.(j) :=
                max max_width_of_col.(j) (wsh.sh_width + 2 * wsh.sh_border)
            else ()
          }
        | None -> () ]
      }
      else ();
    }
  };
  for i = 0 to nrow - 1 do {
    for j = 0 to ncol - 1 do {
      if j < Array.length wshaa.(i) then do {
        match wshaa.(i).(j) with
        [ Some (_, 1) | None -> ()
        | Some (wsh, colspan) ->
            let w =
              loop 0 0 where rec loop w k =
                if k = colspan then w
                else loop (w + max_width_of_col.(j+k)) (k + 1)
            in
            if wsh.sh_width + 2 * wsh.sh_border <= w then ()
            else
              (* "colspanned" width is greater than sum of its columns max
                 widths; dispatching its width difference in all columns: *)
              let dw = wsh.sh_width + 2 * wsh.sh_border - w in
              loop 0 0 where rec loop r k =
                if k = colspan then ()
                else do {
                  let wi = max_width_of_col.(j+k) in
                  let d = wi * dw + r in
                  max_width_of_col.(j+k) := wi + d / w;
                  loop (d mod w) (k + 1);
                } ]
      }
      else ();
    };
  };
  (max_width_of_col, max_height_of_row)
};

value colspan_of_tdal =
  loop where rec loop =
    fun
    [ [Colspan x :: _] -> x
    | [] -> 1 ]
;

value wshll_of_wdescll xd wdescll =
  List.map
    (fun wdescl ->
       loop [] wdescl where rec loop rev_wshl =
         fun
         [ [] ->
             List.rev rev_wshl
         | [TD tdal wdesc :: wdescl] ->
             let colspan = colspan_of_tdal tdal in
             let wsh = wdesc.wsize xd in
             let rev_wshl = [Some (wsh, colspan) :: rev_wshl] in
             let rev_wshl =
               loop rev_wshl (colspan - 1) where rec loop rev_wshl n =
                 if n = 0 then rev_wshl
                 else loop [None :: rev_wshl] (n - 1)
             in
             loop rev_wshl wdescl ])
    wdescll
;

value table_wsize att_val args_ref xd =
  let wdescll = args_ref.val in
  let wshll = wshll_of_wdescll xd wdescll in
  let wshaa = Array.of_list (List.map Array.of_list wshll) in
  let (max_width_of_col, max_height_of_row) = max_size_of_dim xd wshaa in
  let w = Array.fold_left \+ 0 max_width_of_col in
  let h = Array.fold_left \+ 0 max_height_of_row in
  let b = Util.opt_val 0 att_val.Util.border_att in
  {sh_width = w; sh_height = h; sh_border = b; base_width = w;
   base_height = h; width_inc = -1; height_inc = -1}
;

value compute_sizes args_ref xd w h = do {
  let wdescll = args_ref.val in
  let wshll = wshll_of_wdescll xd wdescll in
  let wshaa = Array.of_list (List.map Array.of_list wshll) in
  let (max_width_of_col, max_height_of_row) = max_size_of_dim xd wshaa in
  let sum_w = max 1 (Array.fold_left \+ 0 max_width_of_col) in
  let dw = abs (w - sum_w) in
  let sign = if w > sum_w then 1 else -1 in
  loop 0 0 where rec loop col r =
    if col = Array.length max_width_of_col then ()
    else do {
      let wi = max_width_of_col.(col) in
      let d = wi * dw + r in
      max_width_of_col.(col) := wi + sign * d / sum_w;
      loop (col + 1) (d mod sum_w)
    };
  let sum_h = max 1 (Array.fold_left \+ 0 max_height_of_row) in
  let dh = abs (h - sum_h) in
  let sign = if h > sum_h then 1 else -1 in
  loop 0 0 where rec loop row r =
    if row = Array.length max_height_of_row then ()
    else do {
      let hi = max_height_of_row.(row) in
      let d = hi * dh + r in
      max_height_of_row.(row) := hi + sign * d / sum_h;
      loop (row + 1) (d mod sum_h)
    };
  (wshaa, max_width_of_col, max_height_of_row)
};

value table_wcreate att_val args_ref xd pwin is_top in_popup wdesc x y
    wsh = do {
  let w = wsh.sh_width in
  let h = wsh.sh_height in
  let (wshaa, max_width_of_col, max_height_of_row) =
    compute_sizes args_ref xd w h
  in
  let win =
    Util.create_window xd pwin is_top x y wsh att_val structureNotifyMask
  in
  match att_val.Util.backg_att with
  [ None -> Motif.motif_backg xd win
  | _ -> () ];
  let wdescll = args_ref.val in
  let wdescaa = Array.of_list (List.map Array.of_list wdescll) in
  let widll =
    loop_row [] 0 0 where rec loop_row rev_widll row y_pos =
      if row = Array.length wshaa then List.rev rev_widll
      else
        loop_col [] 0 0 0 where rec loop_col rev_widl col j x_pos =
          if col = Array.length wshaa.(row) then
            loop_row [List.rev rev_widl :: rev_widll] (row + 1)
              (y_pos + max_height_of_row.(row))
          else
            match wdescaa.(row).(j) with
            [ TD tdal wdesc ->
                let colspan = colspan_of_tdal tdal in
                let b =
                  match wshaa.(row).(col) with
                  [ Some (wsh, _) -> wsh.sh_border
                  | None -> 0 ]
                in
                let w =
                  loop 0 col colspan where rec loop w col n =
                    if n = 0 then w
                    else loop (w + max_width_of_col.(col)) (col + 1) (n - 1)
                in
                let wsh =
                  let h = max_height_of_row.(row) - 2 * b in
                  {sh_width = w - 2 * b; sh_height = h; sh_border = b;
                   base_width = w - 2 * b; base_height = h;
                   width_inc = -1; height_inc = -1}
                in
                let wid =
                  wdesc.wcreate xd win False in_popup wdesc x_pos y_pos wsh
                in
                let rev_widl = [(wid, colspan) :: rev_widl] in
                loop_col rev_widl (col + colspan) (j + 1) (x_pos + w) ]
  in
  let info = table_local_info {widll = widll} in
  xMapSubwindows (xd.dpy, win);
  let widlist = List.map fst (List.flatten widll) in
  List.iter (fun wid -> wid.is_mapped := True) widlist;
  let wid = Util.create_widget xd win is_top x y wsh wdesc info widlist in
  Util.add_widget att_val.Util.name_att win wid
};

value table_wresize args_ref wid w h = do {
  let xd = wid.wid_xd in
  let (wshaa, max_width_of_col, max_height_of_row) =
    compute_sizes args_ref xd w h
  in
  let li = get_table_local_info wid.info in
  let widaa = Array.of_list (List.map Array.of_list li.widll) in
  loop_row 0 0 where rec loop_row row y_pos =
    if row = Array.length widaa then ()
    else
      loop_col 0 0 0 where rec loop_col col j x_pos =
        if j = Array.length widaa.(row) then
          loop_row (row + 1) (y_pos + max_height_of_row.(row))
        else do {
          let (wid, colspan) = widaa.(row).(j) in
          let b =
            match wshaa.(row).(col) with
            [ Some (wsh, _) -> wsh.sh_border
            | None -> 0 ]
          in
          let w =
            loop 0 col colspan where rec loop w col n =
              if n = 0 then w - 2 * b
              else loop (w + max_width_of_col.(col)) (col + 1) (n - 1)
          in
          let h = max_height_of_row.(row) - 2 * b in
          xMoveResizeWindow (xd.dpy, wid.win, x_pos, y_pos, w, h);
          wid.wdesc.wresize wid w h;
          loop_col (col + colspan) (j + 1) (x_pos + w + 2 * b)
        };
};

value table_wdestroy att_val wid =
  Util.remove_widget att_val.Util.name_att wid.win wid
;

value table_wdispatch args_ref wid xev =
  let evt = xEvent_type xev in
  if evt = configureNotify then do {
    let xev = xEvent_xconfigure xev in
    let x = xConfigureEvent_x xev in
    let y = xConfigureEvent_y xev in
    let w = xConfigureEvent_width xev in
    let h = xConfigureEvent_height xev in
    wid.x := x;
    wid.y := y;
    if w <> wid.width || h <> wid.height then do {
      let xd = wid.wid_xd in
      let (wshaa, max_width_of_col, max_height_of_row) =
        compute_sizes args_ref xd w h
      in
      let li = get_table_local_info wid.info in
      let widaa = Array.of_list (List.map Array.of_list li.widll) in
      loop_row 0 0 where rec loop_row row y_pos =
        if row = Array.length widaa then ()
        else
          loop_col 0 0 0 where rec loop_col col j x_pos =
            if j = Array.length widaa.(row) then
              loop_row (row + 1) (y_pos + max_height_of_row.(row))
            else do {
              let (wid, colspan) = widaa.(row).(j) in
              let b =
                match wshaa.(row).(col) with
                [ Some (wsh, _) -> wsh.sh_border
                | None -> 0 ]
              in
              let w =
                loop 0 col colspan where rec loop w col n =
                  if n = 0 then w
                  else loop (w + max_width_of_col.(col)) (col + 1) (n - 1)
              in
              let h = max 1 (max_height_of_row.(row) - 2 * b) in
              xMoveResizeWindow
                (xd.dpy, wid.win, x_pos, y_pos, max 1 (w - 2 * b), h);
              loop_col (col + colspan) (j + 1) (x_pos + w)
            };

      wid.width := w;
      wid.height := h;
    }
    else ();
  }
  else ()
;

value table_wfreeze wid = failwith "not impl: table_wfreeze";

value table_wargs args_ref = table_args args_ref;

value table_desc attl args callb =
  let att_val = Util.attribute_values attl in
  let args_ref = ref args in
  {wsize = table_wsize att_val args_ref;
   wcreate = table_wcreate att_val args_ref; wresize = table_wresize args_ref;
   wdestroy = table_wdestroy att_val; wdispatch = table_wdispatch args_ref;
   wfreeze = table_wfreeze; wselect _ _ = ();
   wargs = table_wargs args_ref; filler = List.mem FillerAtt attl}
;

value table_insert_row wid n wdescl = do {
  let xd = wid.wid_xd in
  let args_ref = get_table_args wid.wdesc.wargs in
  args_ref.val :=
    loop 0 args_ref.val where rec loop i =
      fun
      [ [arg :: args] ->
          if i >= n then [wdescl; arg :: args]
          else [arg :: loop (i + 1) args]
      | [] -> [wdescl] ];
  let widl =
    List.map
      (fun
       [ TD tdal wdesc ->
           let wsh = wdesc.wsize xd in
           let colspan = colspan_of_tdal tdal in
           let wid = wdesc.wcreate xd wid.win False False wdesc 0 0 wsh in
           (wid, colspan) ])
      wdescl
  in
  let li = get_table_local_info wid.info in
  li.widll :=
    loop 0 li.widll where rec loop i =
      fun
      [ [widl1 :: widll] ->
          if i >= n then [widl; widl1 :: widll]
          else [widl1 :: loop (i + 1) widll]
      | [] -> [widl] ];
  wid.children := wid.children @ (List.map fst widl);
  xMapSubwindows (xd.dpy, wid.win);
};

value table_add_row wid wdescl = do {
  let xd = wid.wid_xd in
  let args_ref = get_table_args wid.wdesc.wargs in
  args_ref.val := args_ref.val @ [wdescl];
  let widl =
    List.map
      (fun
       [ TD tdal wdesc ->
           let wsh = wdesc.wsize xd in
           let colspan = colspan_of_tdal tdal in
           let wid = wdesc.wcreate xd wid.win False False wdesc 0 0 wsh in
           (wid, colspan) ])
      wdescl
  in
  let li = get_table_local_info wid.info in
  li.widll := li.widll @ [widl];
  wid.children := wid.children @ (List.map fst widl);
  xMapSubwindows (xd.dpy, wid.win);
};

value table_remove_nth_row wid n = do {
  let args_ref = get_table_args wid.wdesc.wargs in
  let wdescll = args_ref.val in
  let (rev_wdescll, _) =
    List.fold_left
      (fun (rev_wdescll, i) wdescl ->
         (if i = n then rev_wdescll else [wdescl :: rev_wdescll], i + 1))
      ([], 0) wdescll
  in
  args_ref.val := List.rev rev_wdescll;
  let li = get_table_local_info wid.info in
  let (rev_new_widll, _) =
    List.fold_left
      (fun (rev_wll, i) wl ->
         if i = n then do {
           List.iter (fun (wid, _) -> Util.destroy_widget wid) wl;
           (rev_wll, i + 1)
         }
         else ([wl :: rev_wll], i + 1))
      ([], 0) li.widll
  in
  li.widll := List.rev rev_new_widll;
  wid.children := List.map fst (List.flatten li.widll);
};
