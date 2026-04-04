(* $Id: d_infoCartes.ml,v 1.5 2006/05/31 03:05:15 deraugla Exp $ *)

open File;
open RtN;

value action_X wid =
  let xd = rt_xdata_of_widget wid in
  rt_unmap_widget (rt_widget_named xd "INFO CARTES")
;

value action wid =
  let xd = rt_xdata_of_widget wid in
  match budget.cartes with
  [ [] -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err "Pas de cartes";
      rt_map_widget err
    }
  | plist -> do {
      let mwid = rt_widget_named xd "Main" in
      let wid = rt_widget_named xd "INFO CARTES" in
      let twid = rt_widget_named xd "INFO CARTES term" in
      term_set_params twid (List.length plist, 25);
      let (width, height) = rt_adjust_widget wid in
      let x = (rt_widget_width mwid - width) / 2 in
      let y = (rt_widget_height mwid - height) / 2 in
      term_send twid "\027[2J\027[H\027[?35h";
      List.iter
        (fun (nom, lib) -> do {
           term_send twid nom;
           for i = String.length nom + 1 to 2 do { term_send twid " " };
           term_send twid " : ";
           term_send twid lib;
           term_send twid "\n"
         })
        plist;
      rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
      rt_map_widget wid
    } ]
;

value wdesc =
  pack_desc [NameAtt "INFO CARTES"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "INFO CARTES titre"] (1, 12) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       term_desc [NameAtt "INFO CARTES term"] (0, 25) Action.butt_term)])
    Action.no_pack
;
