(* $Id: d_infoComptes.ml,v 1.4 2006/05/31 03:05:15 deraugla Exp $ *)

open File;
open RtN;

value action_X wid =
  let xd = rt_xdata_of_widget wid in
  rt_unmap_widget (rt_widget_named xd "INFO COMPTES")
;

value display_info_comptes_term twid plist = do {
  term_send twid "\027[2J\027[H\027[?35h";
  List.iter
    (fun (nom, (lib, _)) -> do {
       term_send twid nom;
       for i = String.length nom to String.length "C99" do {
         term_send twid " ";
       };
       term_send twid ": ";
       term_send twid lib;
       term_send twid "\n"
     })
    plist
};

value action wid =
  let xd = rt_xdata_of_widget wid in
  match
    mklist 1 budget.comptes where rec mklist i =
      fun
      [ [Some c :: comptes] -> [(nom_compte i, c) :: mklist (succ i) comptes]
      | [None :: comptes] -> mklist (succ i) comptes
      | [] -> [] ]
  with
  [ [] -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err "Pas de comptes spéciaux";
      rt_map_widget err
    }
  | plist -> do {
      let mwid = rt_widget_named xd "Main" in
      let wid = rt_widget_named xd "INFO COMPTES" in
      let twid = rt_widget_named xd "INFO COMPTES term" in
      term_set_params twid (List.length plist, 6 + Const.taille_nom_compte);
      let (width, height) = rt_adjust_widget wid in
      let x = (rt_widget_width mwid - width) / 2 in
      let y = (rt_widget_height mwid - height) / 2 in
      display_info_comptes_term twid plist;
      rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
      rt_map_widget wid
    } ]
;

value action_A wid =
  let mlist =
    mklist 1 budget.comptes where rec mklist i =
      fun
      [ [Some c :: comptes] -> [(nom_compte i, c) :: mklist (succ i) comptes]
      | [None :: comptes] -> mklist (succ i) comptes
      | [] -> [] ]
  in
  let mlist = List.sort (fun (_, c1) (_, c2) -> compare c1 c2) mlist in
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "INFO COMPTES term" in
  display_info_comptes_term twid mlist
;

value wdesc =
  pack_desc [NameAtt "INFO COMPTES"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "INFO COMPTES titre"] (1, 12) Action.no_term);
           (FIXSZ, button_desc [] ("A", None) (Action.button action_A))])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       term_desc [NameAtt "INFO COMPTES term"]
         (0, 6 + Const.taille_nom_compte) Action.butt_term)])
    Action.no_pack
;
