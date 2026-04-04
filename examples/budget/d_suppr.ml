(* $Id: d_suppr.ml,v 1.5 2006/05/31 03:05:16 deraugla Exp $ *)

open State;
open File;
open RtN;

value action_fin wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "NoErr");
  state.buttonAct := fun _ _ -> ();
  for i = 0 to state.nbMarques - 1 do { supprimer_ligne_marquee xd };
  state.nbMarques := 0;
  state.action := Rien;
  let ind = List.length mois.val.lignes - 1 in
  if state.noPage > ind / state.nlin then state.noPage := ind / state.nlin
  else ();
  Show.lignes xd state.noPage;
  Show.selection xd state.noPage;
  Show.total xd;
  Show.solde_reduit xd;
  Show.repart_mois xd
};

value action_annule wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "NoErr");
  state.buttonAct := fun _ _ -> ();
  List.iter
    (fun ligne ->
       if ligne.lselection then do {
         ligne.lselection := False;
         maj_total \+ ligne;
         state.nbMarques := state.nbMarques - 1
       }
       else ())
    mois.val.lignes;
  state.action := Rien;
  Show.selection xd state.noPage;
  Show.total xd
};

value select_ligne xd ligne = do {
  if ligne.lselection then do {
    ligne.lselection := False;
    state.nbMarques := state.nbMarques - 1;
    maj_total \+ ligne
  }
  else do {
    ligne.lselection := True;
    state.nbMarques := state.nbMarques + 1;
    maj_total \- ligne
  };
  Show.selection xd state.noPage;
  Show.total xd
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "Supprimer action");
  state.buttonAct :=
    Action.button_select xd
      ["Retire term"; "Date term"; "Nature term"; "Poste term";
       "Libelle term"; "Debit term"; "Credit term"]
      mois.val.lignes (fun _ -> state.noPage * state.nlin) (select_ligne xd);
  state.nbMarques := 0
};

value fin_annule_wdesc =
  pack_desc [NameAtt "Supprimer action"; BandAtt 0]
    (DIRx,
     [(FIXSZ, term_desc [NameAtt "Supprimer titre"] (1, 12) Action.no_term);
      (FIXSZ, button_desc [] ("Fin", None) (Action.button action_fin));
      (FIXSZ, button_desc [] ("Annule", None) (Action.button action_annule));
      Action.filler])
    Action.no_pack
;
