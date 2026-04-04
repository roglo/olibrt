(* $Id: d_listeMois.ml,v 1.4 2006/05/31 03:05:15 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd;
  state.buttonAct := fun _ _ -> ()
};

value select_mois xd ligne = do {
  mois.val := ligne;
  let (debit, credit) =
    List.fold_left
      (fun (debit, credit) ->
         fun
         [ {lmontant = Left x} -> (debit + x, credit)
         | {lmontant = Right x} -> (debit, credit + x) ])
      (0, 0) mois.val.lignes
  in
  rt_unmap_alert xd;
  state.noPage := 0;
  state.tdebit := debit;
  state.tcredit := credit;
  state.buttonAct := fun _ _ -> ();
  Show.mois xd;
  Show.lignes xd state.noPage;
  Show.selection xd state.noPage;
  Show.total xd;
  Show.repart_mois xd;
  match state.action with
  [ Ajoutant -> do {
      let twid = rt_widget_named xd "AJOUT term" in
      let ip = get_ip () in
      term_send twid "\0277";
      set_field ip 2 (Fint mois.val.mois);
      set_field ip 3 (Fint (mois.val.annee mod 100));
      term_send twid "\0278"
    }
  | _ -> () ]
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "Liste mois action" in
  let twid = rt_widget_named xd "Liste mois term" in
  term_set_params twid (List.length budget.listeMois, 14);
  let (width, height) = rt_adjust_widget wid in
  let y = (rt_widget_height mwid - height) / 2 in
  term_send twid "\027[2J\027[H\027[?35h";
  List.iter
    (fun e -> do {
       term_send twid (capitalize (string_of_mois e.mois));
       term_send twid " ";
       term_send twid (string_of_int e.annee);
       term_send twid "\n"
     })
    budget.listeMois;
  state.buttonAct :=
    Action.button_select xd ["Liste mois term"] budget.listeMois (fun _ -> 0)
      (select_mois xd);
  rt_move_widget wid (rt_widget_x mwid) (rt_widget_y mwid + y);
  rt_map_alert wid
};

value wdesc =
  pack_desc [NameAtt "Liste mois action"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "Liste mois titre"] (1, 12) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       term_desc [NameAtt "Liste mois term"] (0, 12) Action.butt_term)])
    (fun _ _ -> ())
;
