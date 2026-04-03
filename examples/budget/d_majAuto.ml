(* $Id: d_majAuto.ml,v 1.7 2006/05/31 17:06:57 deraugla Exp $ *)

open Action;
open State;
open RtN;

value init_vir_auto xd = do { Show.lignes_auto xd; Show.selection_auto xd };

value action_correct_oui wid =
  match state.action with
  [ Ajoutant -> D_ajoutAuto.action_correct_oui wid
  | ModifierOuiNon -> D_modifAuto.action_correct_oui wid
  | _ -> failwith "erreur interne dans D_majAuto action_correct_oui" ]
;

value action_correct_non wid =
  match state.action with
  [ Ajoutant -> D_ajoutAuto.action_correct_non wid
  | ModifierOuiNon -> D_modifAuto.action_correct_non wid
  | _ -> failwith "erreur interne dans D_majAuto action_correct_non" ]
;

value action_quit wid = do {
  state.majAuto := False;
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Liste mois"; "Nouveau mois"; "TRAIT Virements automatiques";
     "Répartition par poste"; "Solde général"; "Bilan de l'année";
     "Mise à jour"];
  rt_unmap_widget (rt_widget_named xd "MAJ AUTO")
};

value action wid = do {
  state.majAuto := True;
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Liste mois"; "Nouveau mois"; "TRAIT Virements automatiques";
     "Répartition par poste"; "Solde général"; "Bilan de l'année";
     "Mise à jour"];
  init_vir_auto xd;
  rt_map_widget (rt_widget_named xd "MAJ AUTO")
};

value center_wdesc wattr wdesc =
  pack_desc wattr
    (DIRy,
     [filler;
      (FIXSZ, pack_desc [] (DIRx, [filler; (FIXSZ, wdesc); filler]) no_pack);
      filler])
;

value nlin = 17;

value wdesc =
  pack_desc [NameAtt "MAJ AUTO"]
    (DIRy,
     [filler;
      (FIXSZ,
       pack_desc []
         (DIRx,
          [filler;
           (FIXSZ,
            pack_desc []
              (DIRy,
               [(FIXSZ,
                 term_desc [NameAtt "MAJ AUTO titre"] (1, 1) Action.no_term);
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
                                term_desc [NameAtt "MA Selection titre"]
                                  (1, 1) Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "MA Selection term"]
                                  (nlin, 1) Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "MA Nombre titre"] (1, 3)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "MA Nombre term"]
                                  (nlin, 3) Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "MA Dernier titre"] (1, 5)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "MA Dernier term"]
                                  (nlin, 5) Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "MA Jour titre"] (1, 2)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "MA Jour term"] (nlin, 2)
                                  Action.butt_term)])
                             Action.no_pack);
                          (INCSZ,
                           pack_desc [BandAtt 0; InterAtt 0]
                             (DIRx,
                              [(FIXSZ,
                                pack_desc [BandAtt 0]
                                  (DIRy,
                                   [(FIXSZ,
                                     term_desc [NameAtt "MA Poste titre"]
                                       (1, 2) Action.no_term);
                                    (INCSZ,
                                     term_desc [NameAtt "MA Poste term"]
                                       (nlin, 2) Action.butt_term)])
                                  Action.no_pack);
                               (INCSZ,
                                pack_desc [BandAtt 0]
                                  (DIRy,
                                   [(FIXSZ,
                                     term_desc [NameAtt "MA Libelle titre"]
                                       (1, 20) Action.no_term);
                                    (INCSZ,
                                     term_desc [NameAtt "MA Libelle term"]
                                       (nlin, 20) Action.butt_term)])
                                  Action.no_pack)])
                             Action.no_pack)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "MA Debit titre"] (1, 10)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "MA Debit term"] (nlin, 10)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "MA Credit titre"] (1, 10)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "MA Credit term"] (nlin, 10)
                             Action.butt_term)])
                        Action.no_pack)])
                   Action.no_pack)])
              Action.no_pack);
           filler])
         Action.no_pack);
      Action.filler;
      (FIXSZ,
       pack_desc [BandAtt 0]
         (DIRx,
          [(FIXSZ,
            pack_desc [BandAtt 0]
              (DIRz,
               [(INCSZ,
                 pack_desc [NameAtt "MA NoErr"] (DIRx, []) Action.no_pack);
                (INCSZ, term_desc [NameAtt "MA Err"] (1, 35) Action.no_term);
                (INCSZ,
                 pack_desc [NameAtt "MA Correct alert"; BandAtt 0]
                   (DIRx,
                    [(FIXSZ,
                      term_desc [NameAtt "MA Correct titre"] (1, 10)
                        Action.no_term);
                     (FIXSZ,
                      button_desc [] ("Oui", None)
                        (Action.button action_correct_oui));
                     (FIXSZ,
                      button_desc [] ("Non", None)
                        (Action.button action_correct_non));
                     Action.filler])
                   Action.no_pack);
                (INCSZ, D_modifAuto.fin_annule_wdesc);
                (INCSZ, D_supprAuto.fin_annule_wdesc)])
              Action.no_pack);
           Action.filler])
         Action.no_pack)])
    Action.no_pack
;
