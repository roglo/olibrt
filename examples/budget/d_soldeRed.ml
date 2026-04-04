(* $Id: d_soldeRed.ml,v 1.3 2006/05/30 14:41:33 deraugla Exp $ *)

open RtN;

value action_X wid =
  let xd = rt_xdata_of_widget wid in
  rt_unmap_widget (rt_widget_named xd "SOLDE REDUIT")
;

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "SOLDE REDUIT" in
  let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
  let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid
};

value wdesc =
  pack_desc [NameAtt "SOLDE REDUIT"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "SOLDE REDUIT titre"] (1, 20)
              Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       term_desc [NameAtt "SOLDE REDUIT term"] (4, 28) Action.no_term)])
    Action.no_pack
;
