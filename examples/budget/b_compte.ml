(* $Id: b_compte.ml,v 1.10 2006/05/31 03:05:15 deraugla Exp $ *)

open State;
open File;
open RtN;

value action_up_arrow wid =
  fun
  [ ArrowEvPress ->
      let xd = rt_xdata_of_widget wid in
      if state.noPage > 0 then do {
        state.noPage := state.noPage - 1;
        Show.lignes xd state.noPage;
        Show.selection xd state.noPage;
        Show.total xd
      }
      else ()
  | _ -> () ]
;

value action_down_arrow wid =
  fun
  [ ArrowEvPress ->
      let xd = rt_xdata_of_widget wid in
      if state.noPage < (List.length mois.val.lignes - 1) / state.nlin then
      do {
        state.noPage := state.noPage + 1;
        Show.lignes xd state.noPage;
        Show.selection xd state.noPage;
        Show.total xd
      }
      else ()
  | _ -> () ]
;

value action_correct_oui wid =
  match state.action with
  [ Ajoutant -> D_ajout.action_correct_oui wid
  | ModifierOuiNon -> D_modif.action_correct_oui wid
  | MajPostesOuiNon -> D_majPostes.action_correct_oui wid
  | MajCartesOuiNon -> D_majCartes.action_correct_oui wid
  | MajComptesOuiNon -> D_majComptes.action_correct_oui wid
  | Transferant -> D_transf.action_correct_oui wid
  | _ -> failwith "erreur interne dans action_correct_oui" ]
;

value action_correct_non wid =
  match state.action with
  [ Ajoutant -> D_ajout.action_correct_non wid
  | ModifierOuiNon -> D_modif.action_correct_non wid
  | MajPostesOuiNon -> D_majPostes.action_correct_non wid
  | MajCartesOuiNon -> D_majCartes.action_correct_non wid
  | MajComptesOuiNon -> D_majComptes.action_correct_non wid
  | Transferant -> D_transf.action_correct_non wid
  | _ -> failwith "erreur interne dans action_correct_non" ]
;

value action_quit wid =
  let xd = rt_xdata_of_widget wid in
  if is_frozen (rt_widget_named xd "Quitter") ||
     is_frozen (rt_widget_named xd "Fichier")
  then
    ()
  else if state.majAuto then D_majAuto.action_quit wid
  else
    match state.action with
    [ EcranRepartPoste -> D_repartPoste.action_quit wid
    | EcranSoldeGen -> D_soldeGen.action_quit wid
    | EcranBilan -> D_bilan.action_quit wid
    | _ -> do {
        List.iter (fun n -> rt_unmap_widget (rt_widget_named xd n))
          ["compte bancaire"; "SOLDE REDUIT"; "REPART MOIS"; "INFO POSTES";
           "INFO CARTES"; "INFO COMPTES"];
        state.action_quit := fun _ -> state.quit := True
      } ]
;

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let (debit, credit) =
    List.fold_left
      (fun (debit, credit) ->
         fun
         [ {lmontant = Left x} -> (debit + x, credit)
         | {lmontant = Right x} -> (debit, credit + x) ])
      (0, 0) mois.val.lignes
  in
  state.action_quit := action_quit;
  state.tdebit := debit;
  state.tcredit := credit;
  state.noPage := 0;
  Show.mois xd;
  Show.lignes xd state.noPage;
  Show.selection xd state.noPage;
  Show.total xd;
  Show.solde_reduit xd;
  Show.repart_mois xd;
  rt_map_widget (rt_widget_named xd "compte bancaire")
};

value lignes_wdesc =
  pack_desc []
    (DIRx,
     [(INCSZ,
       pack_desc []
         (DIRy,
          [(FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(INCSZ, term_desc [NameAtt "Mois"] (1, 1) Action.no_term);
                (FIXSZ, term_desc [NameAtt "Page"] (1, 8) Action.no_term)])
              Action.no_pack);
           (INCSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(INCSZ,
                 pack_desc [BandAtt 0]
                   (DIRx,
                    [(FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "Selection titre"] (1, 1)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "Selection term"] (20, 1)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "Retire titre"] (1, 1)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "Retire term"] (20, 1)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "Date titre"] (1, 8)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "Date term"] (20, 8)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "Nature titre"] (1, 10)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "Nature term"] (20, 10)
                             Action.butt_term)])
                        Action.no_pack);
                     (INCSZ,
                      pack_desc [BandAtt 0; InterAtt 0]
                        (DIRx,
                         [(FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "Poste titre"] (1, 2)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "Poste term"] (20, 2)
                                  Action.butt_term)])
                             Action.no_pack);
                          (INCSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "Libelle titre"] (1, 20)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "Libelle term"] (20, 20)
                                  Action.butt_term)])
                             Action.no_pack)])
                        Action.no_pack)])
                   Action.no_pack);
                (FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRy,
                    [(FIXSZ,
                      term_desc [NameAtt "Debit titre"] (1, 10)
                        Action.no_term);
                     (INCSZ,
                      term_desc [NameAtt "Debit term"] (20, 10)
                        Action.butt_term)])
                   Action.no_pack);
                (FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRy,
                    [(FIXSZ,
                      term_desc [NameAtt "Credit titre"] (1, 10)
                        Action.no_term);
                     (INCSZ,
                      term_desc [NameAtt "Credit term"] (20, 10)
                        Action.butt_term)])
                   Action.no_pack)])
              Action.no_pack);
           (FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRz,
                    [(INCSZ,
                      pack_desc [NameAtt "NoErr"] (DIRx, []) Action.no_pack);
                     (INCSZ,
                      term_desc [NameAtt "Err"] (1, 35) Action.no_term);
                     (INCSZ,
                      pack_desc [NameAtt "Correct alert"; BandAtt 0]
                        (DIRx,
                         [(FIXSZ,
                           term_desc [NameAtt "Correct titre"] (1, 10)
                             Action.no_term);
                          (FIXSZ,
                           button_desc [] ("Oui", None)
                             (Action.button action_correct_oui));
                          (FIXSZ,
                           button_desc [] ("Non", None)
                             (Action.button action_correct_non));
                          Action.filler])
                        Action.no_pack);
                     (INCSZ, D_modif.fin_annule_wdesc);
                     (INCSZ, D_suppr.fin_annule_wdesc)])
                   Action.no_pack);
                Action.filler;
                (FIXSZ,
                 term_desc [NameAtt "Tdebit term"] (1, 10) Action.no_term);
                (FIXSZ,
                 term_desc [NameAtt "Tcredit term"] (1, 10) Action.no_term)])
              Action.no_pack)])
         Action.no_pack);
      (FIXSZ,
       pack_desc []
         (DIRy,
          [(FIXSZ, arrow_desc [] ORup action_up_arrow); Action.filler;
           (FIXSZ, arrow_desc [] ORdown action_down_arrow)])
         Action.no_pack)])
    Action.no_pack
;

value wdesc =
  pack_desc [NameAtt "compte bancaire"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ,
            button_desc [] ("Fichier", None) (Action.popup "Fichier popup"));
           (FIXSZ,
            button_desc [] ("Traitement", None)
              (Action.popup "Traitement popup"));
           (FIXSZ,
            button_desc [] ("Budget", None) (Action.popup "Budget popup"));
           (FIXSZ,
            button_desc [] ("Informations", None)
              (Action.popup "Informations popup"));
           (FIXSZ,
            button_desc [] ("Mise à jour", None)
              (Action.popup "Mise à jour popup"))])
         Action.no_pack);
      (INCSZ,
       pack_desc [BandAtt 0]
         (DIRz,
          [(INCSZ, lignes_wdesc); (INCSZ, D_auto.wdesc);
           (INCSZ, D_repartPoste.wdesc); (INCSZ, D_soldeGen.wdesc);
           (INCSZ, D_bilan.wdesc); (INCSZ, D_majAuto.wdesc)])
         Action.no_pack)])
    Action.no_pack
;
