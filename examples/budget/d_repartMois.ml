(* $Id: d_repartMois.ml,v 1.3 2006/05/30 14:41:33 deraugla Exp $ *)

open RtN;

value action_X wid =
  let xd = rt_xdata_of_widget wid in
  rt_unmap_widget (rt_widget_named xd "REPART MOIS")
;

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "REPART MOIS" in
  let (width, height) = rt_adjust_widget wid in
  let x = (rt_widget_width mwid - width) / 2 in
  let y = (rt_widget_height mwid - height) / 2 in
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid
};

value wdesc =
  pack_desc [NameAtt "REPART MOIS"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "REPART MOIS titre"] (1, 11) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       term_desc [NameAtt "REPART MOIS term"] (0, 31) Action.butt_term)])
    Action.no_pack
;
