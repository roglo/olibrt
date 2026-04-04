(* $Id: d_ajout.ml,v 1.6 2006/06/02 00:22:00 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value rec init_ajouter xd = do {
  let twid = rt_widget_named xd "AJOUT term" in
  let ip = input_of_pic twid AjoutModif.image in
  term_send twid "\027[H\027[2J\027[?35l";
  term_send twid (empty_pic AjoutModif.image);
  set_field ip 2 (Fint mois.val.mois);
  set_field ip 3 (Fint (mois.val.annee mod 100));
  set_field ip 4 (Fstring (string_of_int budget.noCheque));
  lock_field ip 2;
  lock_field ip 3;
  goto_field ip 1 False;
  state.iP := Some ip;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      (fun ip ->
         if AjoutModif.verif ip then
           state.keyPressAct :=
             fun _ ksym ->
               match ksym with
               [ Rt.K_Return -> action_correct_oui twid
               | Rt.K_Escape -> action_correct_non twid
               | _ -> () ]
         else ())
}
and action_correct_oui wid = do {
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  let (ind, ligne) = ajouter_ligne ip True in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  state.noPage := ind / state.nlin;
  state.iP := None;
  maj_total \+ ligne;
  Show.lignes xd state.noPage;
  Show.total xd;
  Show.solde_reduit xd;
  Show.repart_mois xd;
  init_ajouter xd
}
and action_correct_non wid = do {
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "AJOUT term" in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  term_send twid "\027[?35l\027[?7l";
  goto_field ip 1 False;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      (fun ip ->
         if AjoutModif.verif ip then
           state.keyPressAct :=
             fun _ ksym ->
               match ksym with
               [ Rt.K_Return -> action_correct_oui twid
               | Rt.K_Escape -> action_correct_non twid
               | _ -> () ]
         else ())
};

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "Calculatrice"; "Bloc-notes";
     "Mise à jour"];
  rt_unmap_widget (rt_widget_named xd "AJOUT");
  rt_map_widget (rt_widget_named xd "NoErr");
  state.keyPressAct := fun _ _ -> ();
  state.action := Rien
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "AJOUT" in
  let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
  let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "Calculatrice"; "Bloc-notes";
     "Mise à jour"];
  init_ajouter xd;
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid;
  state.action := Ajoutant
};

value wdesc =
  pack_desc [NameAtt "AJOUT"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "AJOUT titre"] (1, 20) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "AJOUT term"] (7, 30) Action.no_term)])
    Action.no_pack
;
