(* $Id: d_supprAuto.ml,v 1.5 2006/05/31 03:05:16 deraugla Exp $ *)

open State;
open File;
open RtN;

value action_fin wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Traitement"];
  rt_map_widget (rt_widget_named xd "MA NoErr");
  state.buttonAct := fun _ _ -> ();
  budget.virAuto :=
    enlever budget.virAuto where rec enlever =
      fun
      [ [] -> []
      | [v :: vl] ->
          if v.vselection then enlever vl else [v :: enlever vl] ];
  state.nbMarques := 0;
  state.action := Rien;
  Show.lignes_auto xd;
  Show.selection_auto xd;
  modif xd
};

value action_annule wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Traitement"];
  rt_map_widget (rt_widget_named xd "MA NoErr");
  state.buttonAct := fun _ _ -> ();
  List.iter
    (fun ligne ->
       if ligne.vselection then do {
         ligne.vselection := False;
         state.nbMarques := state.nbMarques - 1
       }
       else ())
    budget.virAuto;
  state.action := Rien;
  Show.selection_auto xd
};

value select_ligne xd ligne = do {
  if ligne.vselection then do {
    ligne.vselection := False;
    state.nbMarques := state.nbMarques - 1
  }
  else do {
    ligne.vselection := True;
    state.nbMarques := state.nbMarques + 1
  };
  Show.selection_auto xd
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Traitement"];
  rt_map_widget (rt_widget_named xd "MA Supprimer action");
  state.buttonAct :=
    Action.button_select xd
      ["MA Nombre term"; "MA Dernier term"; "MA Jour term"; "MA Poste term";
       "MA Libelle term"; "MA Debit term"; "MA Credit term"]
      budget.virAuto (fun _ -> 0) (select_ligne xd);
  state.nbMarques := 0
};

value fin_annule_wdesc =
  pack_desc [NameAtt "MA Supprimer action"; BandAtt 0]
    (DIRx,
     [(FIXSZ,
       term_desc [NameAtt "MA Supprimer titre"] (1, 12) Action.no_term);
      (FIXSZ, button_desc [] ("Fin", None) (Action.button action_fin));
      (FIXSZ, button_desc [] ("Annule", None) (Action.button action_annule));
      Action.filler])
    Action.no_pack
;
