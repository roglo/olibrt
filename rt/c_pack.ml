(* $Id: c_pack.ml,v 1.23 2008/04/10 15:57:56 deraugla Exp $
 *
 * Rogloglo Toolkit: pack widget class
 *)

open Keysym;
open Motif;
open Std;
open Rtdecl;
open Util;
open Xlib;

type pack_event =
  [ PackEvKeyPress of modified keysym ]
;
type pack_args = (orientation * list widget_desc);
type pack_event_handler = widget -> pack_event -> unit;

type pack_local_info =
  { att_val : attribute_values; widlist : mutable list widget }
;

value (pack_local_info, get_pack_local_info) =
  dynamo_local_info "pack_local_info" (ref (None : option pack_local_info))
;

value (pack_args, get_pack_args) =
  dynamo_args_info "pack_args_info" (ref None)
;

value pack_border = ref 1 and pack_band = ref 4 and pack_inter = ref 2;

value pack_wsize att_val ref_args xd =
  let (orient, wlist) = ref_args.val in
  let pinter = opt_val pack_inter.val att_val.inter_att in
  let nfill =
    List.fold_left (fun n wdesc -> if wdesc.filler then n + 1 else n) 0 wlist
  in
  let all_filler = nfill == List.length wlist in
  let wsh =
    match orient with
    [ Vertical ->
        List.fold_left
          (fun csh wdesc ->
             let sh = wdesc.wsize xd in
             let {sh_width = w; sh_height = h; sh_border = b} = sh in
             let filler = wdesc.filler || all_filler in
             {sh_width = max (b + w + b) csh.sh_width;
              sh_height = csh.sh_height + pinter + b + h + b;
              sh_border = csh.sh_border;
              base_width =
                if sh.width_inc == -1 then csh.base_width
                else max (b + sh.base_width + b) csh.base_width;
              base_height =
                csh.base_height + pinter + b +
                (if filler then sh.base_height else h) + b;
              width_inc = max sh.width_inc csh.width_inc;
              height_inc =
                if filler then max sh.height_inc csh.height_inc
                else csh.height_inc})
          {sh_width = 0; sh_height = -pinter; sh_border = 0; base_width = 0;
           base_height = -pinter; width_inc = -1; height_inc = -1}
          wlist
    | Horizontal ->
        List.fold_left
          (fun csh wdesc ->
             let sh = wdesc.wsize xd in
             let {sh_width = w; sh_height = h; sh_border = b} = sh in
             let filler = wdesc.filler || all_filler in
             {sh_width = csh.sh_width + pinter + b + w + b;
              sh_height = max (b + h + b) csh.sh_height;
              sh_border = csh.sh_border;
              base_width =
                csh.base_width + pinter + b +
                (if filler then sh.base_width else w) + b;
              base_height =
                if sh.height_inc == -1 then csh.base_height
                else max (b + sh.base_height + b) csh.base_height;
              width_inc =
                if filler then max sh.width_inc csh.width_inc
                else csh.width_inc;
              height_inc = max sh.height_inc csh.height_inc})
          {sh_width = -pinter; sh_height = 0; sh_border = 0;
           base_width = -pinter; base_height = 0; width_inc = -1;
           height_inc = -1}
          wlist
    | InDepth ->
        List.fold_left
          (fun csh wdesc ->
             let sh = wdesc.wsize xd in
             let b = sh.sh_border in
             {sh_width = max csh.sh_width (b + sh.sh_width + b);
              sh_height = max csh.sh_height (b + sh.sh_height + b);
              sh_border = csh.sh_border;
              base_width =
                if sh.width_inc == -1 then csh.base_width
                else max csh.base_width (b + sh.base_width + b);
              base_height =
                if sh.height_inc == -1 then csh.base_height
                else max csh.base_height (b + sh.base_height + b);
              width_inc = max sh.width_inc csh.width_inc;
              height_inc = max sh.height_inc csh.height_inc})
          {sh_width = 1; sh_height = 1; sh_border = 0; base_width = 0;
           base_height = 0; width_inc = -1; height_inc = -1}
          wlist ]
  in
  let pband = opt_val pack_band.val att_val.band_att in
  let width =
    max (opt_val 1 att_val.width_att) (pband + wsh.sh_width + pband)
  in
  let height =
    max (opt_val 1 att_val.height_att) (pband + wsh.sh_height + pband)
  in
  {sh_width = width; sh_height = height;
   sh_border = opt_val pack_border.val att_val.border_att;
   base_width =
     if wsh.width_inc == -1 then width else pband + wsh.base_width + pband;
   base_height =
     if wsh.height_inc == -1 then height else pband + wsh.base_height + pband;
   width_inc = wsh.width_inc; height_inc = wsh.height_inc}
;

value do_pack_widlist xd att_val ref_args width height a_cdr a_list b_cons
    b_nil b_make =
  let wsh = pack_wsize att_val ref_args xd in
  let {sh_width = fW; sh_height = fH; base_width = mW; base_height = mH} =
    wsh
  in
  let (orient, wdlist) = ref_args.val in
  let nfill =
    List.fold_left (fun n wdesc -> if wdesc.filler then n + 1 else n) 0 wdlist
  in
  let pband = opt_val pack_band.val att_val.band_att in
  let pinter = opt_val pack_inter.val att_val.inter_att in
  let all_filler = nfill = List.length wdlist in
  let nfill = if all_filler then List.length wdlist else nfill in
  let too_small =
    match orient with
    [ Vertical -> height < mH
    | Horizontal -> width < mW
    | InDepth -> False ]
  in
  let w = width - 2 * pband
  and h = height - 2 * pband in
  if not too_small then
    (**)
        (* This code is translated from the old new Rt toolkit (in C), is
           shorter and simpler than the commented code which follows and
           seem to work as well. Perhaps all this function must be reviewed
           using only that code, and for the case "too_small", a translation
           to be done also. *)
    let inc =
      match orient with
      [ Horizontal -> if nfill = 0 then 0 else (width - fW) / nfill
      | Vertical -> if nfill = 0 then 0 else (height - fH) / nfill
      | _ -> 0 ]
    in
    action_loop pband pband a_list wdlist where rec action_loop sx sy a_list =
      fun
      [ [wdesc :: wdl] ->
          let (swidth, sheight) =
            match orient with
            [ Horizontal ->
                let xssh = wdesc.wsize xd in
                (if wdesc.filler then xssh.sh_width + inc else xssh.sh_width,
                 height - 2 * pband)
            | Vertical ->
                let xssh = wdesc.wsize xd in
                (width - 2 * pband,
                 if wdesc.filler then xssh.sh_height + inc
                 else xssh.sh_height)
            | InDepth -> (width - 2 * pband, height - 2 * pband) ]
          in
          let swidth = max swidth 1 in
          let sheight = max sheight 1 in
          let k = b_make wdesc a_list sx sy swidth sheight 0 in
          let (sx, sy) =
            match orient with
            [ Horizontal -> (sx + swidth + pinter, sy)
            | Vertical -> (sx, sy + sheight + pinter)
            | InDepth -> (sx, sy) ]
          in
          b_cons k (action_loop sx sy (a_cdr a_list) wdl)
      | [] -> b_nil ]
  else
    let fW = (if too_small then mW else fW) - 2 * pband
    and fH = (if too_small then mH else fH) - 2 * pband in
    action_loop pband pband 0 a_list wdlist
    where rec action_loop x y err a_list =
      fun
      [ [wdesc :: wdl] ->
          let {sh_width = fw; sh_height = fh; sh_border = b} =
            wdesc.wsize xd
          in
(*
          let (fw, fh) =
            if too_small && wdesc.filler then
              match orient with
              [ Vertical -> (fw, 1)
              | Horizontal -> (1, fh)
              | InDepth -> (fw, fh) ]
            else (fw, fh)
          in
*)
          let (ow, oh, dx, dy) =
            match orient with
            [ Vertical ->
                (w, (err + (fh + 2 * b) * h) / fH, 0,
                 err + (fh + 2 * b + pinter) * h)
            | Horizontal ->
                ((err + (fw + 2 * b) * w) / fW, h,
                 err + (fw + 2 * b + pinter) * w, 0)
            | InDepth -> (w, h, 0, 0) ]
          in
          let err =
            match orient with
            [ Vertical -> dy mod fH
            | Horizontal -> dx mod fW
            | InDepth -> 0 ]
          in
          let k =
            b_make wdesc a_list x y (max (ow - 2 * b) 1) (max (oh - 2 * b) 1)
              b
          in
          b_cons k
            (action_loop (x + dx / max 1 fW) (y + dy / max 1 fH) err
	     (a_cdr a_list) wdl)
      | _ -> b_nil ]
;

value pack_wdispatch att_val ref_args callb wid xev =
  let t = xEvent_type xev in
  if t == configureNotify then do {
    let xev = xEvent_xconfigure xev in
    let x = xConfigureEvent_x xev
    and y = xConfigureEvent_y xev
    and width = xConfigureEvent_width xev
    and height = xConfigureEvent_height xev
    and border = xConfigureEvent_border_width xev in
    wid.x := x;
    wid.y := y;
    if width <> wid.width || height <> wid.height then
      let xd = wid.wid_xd in
      let li = get_pack_local_info wid.info in
      do_pack_widlist xd att_val ref_args width height List.tl li.widlist
        (fun _ y -> y) ()
        (fun _ widl x y w h b ->
           let wid = List.hd widl in
           xMoveResizeWindow (xd.dpy, wid.win, x, y, w, h))
    else ();
    wid.width := width;
    wid.height := height;
    wid.border := border
  }
  else if t == keyPress then
    if not wid.frozen then
      let xkey_ev = xEvent_xkey xev in
      let state = xKeyEvent_state xkey_ev in
      let ksym =
        let k = keysym_int_of_xevent xev in
        keysym_of_keysym_int k state
      in
      let ev = PackEvKeyPress ksym in callb wid ev
    else ()
  else ()
;

value pack_desc attr args callb =
  let att_val = attribute_values attr in
  let ref_args = ref args in
  {wsize = pack_wsize att_val ref_args;
   wcreate xd pwin is_top in_popup wdesc x y wsh = do {
     let {sh_width = width; sh_height = height; sh_border = border} = wsh in
     let win =
       create_window xd pwin is_top x y wsh att_val structureNotifyMask
     in
     match att_val.backg_att with
     [ None -> motif_backg xd win
     | _ -> () ];
     let widlist =
       do_pack_widlist xd att_val ref_args width height (fun x -> x) ()
         (fun x y -> [x :: y]) []
         (fun wdesc _ x y w h b ->
            wdesc.wcreate xd win False in_popup wdesc x y
              {sh_width = w; sh_height = h; sh_border = b; base_width = 0;
               base_height = 0; width_inc = 0; height_inc = 0})
     in
     xMapSubwindows (xd.dpy, win);
     match (args, widlist) with
     [ ((InDepth, _), [wid :: _]) -> xRaiseWindow (xd.dpy, wid.win)
     | _ -> () ];
     List.iter (fun wid -> wid.is_mapped := True) widlist;
     let info = pack_local_info {att_val = att_val; widlist = widlist} in
     let wid = create_widget xd win is_top x y wsh wdesc info widlist in
     add_widget att_val.name_att win wid
   };
   wresize wid width height = do {
     let xd = wid.wid_xd in
     let wargs = get_pack_args wid.wdesc.wargs in
     let li = get_pack_local_info wid.info in
     do_pack_widlist xd li.att_val wargs width height List.tl li.widlist
       (fun _ y -> y) ()
       (fun _ widl x y w h b -> do {
          let wid = List.hd widl in
          xMoveResizeWindow (xd.dpy, wid.win, x, y, w, h);
          wid.wdesc.wresize wid w h;
        })
   };
   wdestroy wid = remove_widget att_val.name_att wid.win wid;
   wdispatch = pack_wdispatch att_val ref_args callb; wselect _ _ = ();
   wfreeze wid = (); wargs = pack_args ref_args;
   filler = List.mem FillerAtt attr}
;

value pack_extend wid wdescl = do {
  let xd = wid.wid_xd in
  let wargs = get_pack_args wid.wdesc.wargs in
  let (orient, wlist) = wargs.val in
  wargs.val := (orient, wlist @ wdescl);
  let wsh = wid.wdesc.wsize xd in
  let li = get_pack_local_info wid.info in
  let new_widlist =
    do_pack_widlist xd li.att_val wargs wsh.sh_width wsh.sh_height
      (fun [ [_ :: widl] -> widl | [] -> [] ]) li.widlist
      (fun x y -> [x :: y]) []
      (fun wdesc widl x y w h b ->
         match widl with
         [ [] ->
             let in_popup = False (* to be checked *) in
             wdesc.wcreate xd wid.win False in_popup wdesc x y
               {sh_width = w; sh_height = h; sh_border = b; base_width = 0;
                base_height = 0; width_inc = 0; height_inc = 0}
         | [wid :: _] ->
             wid ])
  in
  li.widlist := new_widlist;
  wid.children := new_widlist;
  xMapSubwindows (xd.dpy, wid.win);
};

value pack_remove_nth wid n = do {
  let wargs = get_pack_args wid.wdesc.wargs in
  let (orient, wlist) = wargs.val in
  let (rev_wlist, _) =
    List.fold_left
      (fun (rev_wl, i) w -> (if i = n then rev_wl else [w :: rev_wl], i + 1))
      ([], 1) wlist
  in
  wargs.val := (orient, List.rev rev_wlist);
  let li = get_pack_local_info wid.info in
  let (rev_new_widlist, _) =
    List.fold_left
      (fun (rev_wl, i) w ->
         if i = n then do {
           destroy_widget w;
           (rev_wl, i + 1)
         }
         else ([w :: rev_wl], i + 1))
      ([], 1) li.widlist
  in
  li.widlist := List.rev rev_new_widlist;
  wid.children := li.widlist;
};
