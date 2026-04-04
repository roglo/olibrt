(* $Id: d_auto.ml,v 1.5 2006/05/31 03:05:15 deraugla Exp $ *)

open State;
open File;
open RtN;

value ajouter_ligne xd v = do {
  let ligne =
    {lselection = False; lretire = False; ljour = v.vjour;
     lnature = Some "Auto."; lposte = v.vposte; llibelle = v.vlibelle;
     lmontant = v.vmontant}
  in
  let (ind, lignes) =
    add 0 mois.val.lignes where rec add i =
      fun
      [ [l :: ll] ->
          if ligne.ljour < l.ljour then (i, [ligne; l :: ll])
          else
            let (i, lignes) = add (succ i) ll in
            (i, [l :: lignes])
      | [] -> (i, [ligne]) ]
  in
  mois.val.lignes := lignes;
  maj_total \+ ligne;
  state.noPage := ind / state.nlin;
  v.vmois := mois.val.mois;
  v.vannee := mois.val.annee;
  v.vnombre := v.vnombre + 1;
  modif xd
};

value action_fin wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  List.iter
    (fun v ->
       if v.vselection then do { v.vselection := False; ajouter_ligne xd v }
       else ())
    budget.virAuto;
  state.buttonAct := fun _ _ -> ();
  state.nbMarques := 0;
  state.action := Rien;
  modif xd;
  rt_unmap_widget (rt_widget_named xd "VIR AUTO");
  Show.lignes xd state.noPage;
  Show.total xd;
  Show.solde_reduit xd;
  Show.repart_mois xd
};

value action_annule wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
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
  rt_unmap_widget (rt_widget_named xd "VIR AUTO")
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
  Show.selection_sel_auto xd
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "VIR AUTO");
  rt_map_widget (rt_widget_named xd "VA Vir Auto action");
  state.buttonAct :=
    Action.button_select xd
      ["VA Nombre term"; "VA Dernier term"; "VA Jour term"; "VA Poste term";
       "VA Libelle term"; "VA Debit term"; "VA Credit term"]
      budget.virAuto (fun _ -> 0) (select_ligne xd);
  state.nbMarques := 0;
  Show.lignes_sel_auto xd;
  Show.selection_sel_auto xd
};

value fin_annule_wdesc =
  pack_desc [NameAtt "VA Vir Auto action"; BandAtt 0]
    (DIRx,
     [(FIXSZ,
       term_desc [NameAtt "VA Vir Auto titre"] (1, 25) Action.no_term);
      (FIXSZ, button_desc [] ("Fin", None) (Action.button action_fin));
      (FIXSZ, button_desc [] ("Annule", None) (Action.button action_annule));
      Action.filler])
    Action.no_pack
;

value nlin = 17;

value wdesc =
  pack_desc [NameAtt "VIR AUTO"]
    (DIRy,
     [Action.filler;
      (FIXSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ,
            pack_desc []
              (DIRy,
               [(FIXSZ,
                 term_desc [NameAtt "VIR AUTO titre"] (1, 1) Action.no_term);
                (INCSZ,
                 pack_desc [BandAtt 0]
                   (DIRx,
                    [(FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRx,
                         [(FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "VA Selection titre"]
                                  (1, 1) Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "VA Selection term"]
                                  (nlin, 1) Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "VA Nombre titre"] (1, 3)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "VA Nombre term"]
                                  (nlin, 3) Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "VA Dernier titre"] (1, 5)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "VA Dernier term"]
                                  (nlin, 5) Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "VA Jour titre"] (1, 2)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "VA Jour term"] (nlin, 2)
                                  Action.butt_term)])
                             Action.no_pack);
                          (INCSZ,
                           pack_desc [BandAtt 0; InterAtt 0]
                             (DIRx,
                              [(FIXSZ,
                                pack_desc [BandAtt 0]
                                  (DIRy,
                                   [(FIXSZ,
                                     term_desc [NameAtt "VA Poste titre"]
                                       (1, 2) Action.no_term);
                                    (INCSZ,
                                     term_desc [NameAtt "VA Poste term"]
                                       (nlin, 2) Action.butt_term)])
                                  Action.no_pack);
                               (INCSZ,
                                pack_desc [BandAtt 0]
                                  (DIRy,
                                   [(FIXSZ,
                                     term_desc [NameAtt "VA Libelle titre"]
                                       (1, 20) Action.no_term);
                                    (INCSZ,
                                     term_desc [NameAtt "VA Libelle term"]
                                       (nlin, 20) Action.butt_term)])
                                  Action.no_pack)])
                             Action.no_pack)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "VA Debit titre"] (1, 10)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "VA Debit term"] (nlin, 10)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "VA Credit titre"] (1, 10)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "VA Credit term"] (nlin, 10)
                             Action.butt_term)])
                        Action.no_pack)])
                   Action.no_pack)])
              Action.no_pack);
           Action.filler])
         Action.no_pack);
      Action.filler;
      (FIXSZ,
       pack_desc [BandAtt 0]
         (DIRx, [(FIXSZ, fin_annule_wdesc); Action.filler]) Action.no_pack)])
    Action.no_pack
;
