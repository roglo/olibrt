(* $Id: b_pointage.ml,v 1.9 2006/05/31 19:36:24 deraugla Exp $ *)

open State;
open File;
open RtN;

value up_arrow wid =
  fun
  [ ArrowEvPress ->
      let xd = rt_xdata_of_widget wid in
      if state.noPage > 0 then do {
        state.noPage := state.noPage - 1;
        Show.lignes_pt xd state.noPage;
        Show.selection_pt xd state.noPage;
        Show.total_pt xd
      }
      else ()
  | _ -> () ]
;

value down_arrow wid =
  fun
  [ ArrowEvPress ->
      let xd = rt_xdata_of_widget wid in
      let (nlin, _) =
        term_get_params (rt_widget_named xd "PT Libelle term")
      in
      let necr = List.length state.ptSelection in
      if state.noPage < (necr - 1) / nlin then do {
        state.noPage := state.noPage + 1;
        Show.lignes_pt xd state.noPage;
        Show.selection_pt xd state.noPage;
        Show.total_pt xd
      }
      else ()
  | _ -> () ]
;

value select_ligne xd (ligne, _, _) = do {
  let op = if not ligne.val.lselection then \+ else \- in
  ligne.val.lselection := not ligne.val.lselection;
  match ligne.val.lmontant with
  [ Left x -> do {
      state.tdebit := op state.tdebit (- x);
      state.ptSoldeBanque := op state.ptSoldeBanque (- x);
      match ligne.val.lnature with
      [ Some c ->
          if List.mem_assoc c budget.cartes then
            state.ptEnCoursCartes := op state.ptEnCoursCartes (- x)
          else ()
      | None -> () ]
    }
  | Right x -> do {
      state.tcredit := op state.tcredit (- x);
      state.ptSoldeBanque := op state.ptSoldeBanque x
    } ];
  Show.selection_pt xd state.noPage;
  Show.total_pt xd;
  Show.solde_pt xd
};

value montrer_pointage xd selecteur = do {
  let (sel, debit, credit) =
    List.fold_left
      (fun (sel, debit, credit) m ->
         List.fold_right
           (fun ligne (sel, debit, credit) ->
              if not ligne.lretire then
                let nat =
                  match ligne.lnature with
                  [ Some n -> n
                  | _ -> "" ]
                in
                if selecteur nat then
                  let l = (ref ligne, m.mois, m.annee) in
                  match ligne.lmontant with
                  [ Left x -> ([l :: sel], debit + x, credit)
                  | Right x -> ([l :: sel], debit, credit + x) ]
                else (sel, debit, credit)
              else (sel, debit, credit))
           m.lignes (sel, debit, credit))
      ([], 0, 0) budget.listeMois
  in
  let s = calculer_soldes budget in
  state.tdebit := debit;
  state.tcredit := credit;
  state.ptSelection := sel;
  state.ptSoldeBanque := s.soldeBanque;
  state.ptEnCoursCartes :=
    List.fold_left
      (fun n (c, d) -> if List.mem_assoc c budget.cartes then n + d else n) 0
      s.enCours;
  state.noPage := 0;
  state.buttonAct :=
    Action.button_select xd
      ["PT Retire term"; "PT Date term"; "PT Nature term"; "PT Poste term";
       "PT Libelle term"; "PT Debit term"; "PT Credit term"]
      state.ptSelection
      (fun () ->
         let (nlin, _) =
           term_get_params (rt_widget_named xd "PT Libelle term")
         in
         state.noPage * nlin)
      (select_ligne xd);
  Show.lignes_pt xd state.noPage;
  Show.selection_pt xd state.noPage;
  Show.total_pt xd;
  Show.solde_pt xd
};

value action_quit xd = do {
  rt_unmap_alert xd;
  state.buttonAct := fun _ _ -> ();
  rt_unmap_widget (rt_widget_named xd "POINTAGE");
  state.action_quit := fun _ -> state.quit := True
};

value select_ligne2 xd ligne =
  match fst ligne with
  [ "quit" -> action_quit xd
  | _ -> do {
      rt_unmap_alert xd;
      state.buttonAct := fun _ _ -> ();
      rt_map_widget (rt_widget_named xd "PT buttons");
      let f =
        match fst ligne with
        [ "autr" -> fun c -> not (List.mem_assoc c budget.cartes)
        | "tout" -> fun c -> True
        | c -> \= c ]
      in
      montrer_pointage xd f
    } ]
;

value subtract _xxx1 _xxx2 =
  match (_xxx1, _xxx2) with
  [ (f, []) -> f
  | (f, e) ->
      subtract_e f where rec subtract_e =
        fun
        [ [] -> []
        | [elem :: l] ->
            if List.mem elem e then subtract_e l
            else [elem :: subtract_e l] ] ]
;

value init_pointage xd = do {
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "PT Question" in
  let twid = rt_widget_named xd "PT Question term" in
  let (nlin, ncol) = term_get_params twid in
  let clist = subtract budget.cartes [("", "")] in
  let (nsel, lignes) =
    match clist with
    [ [] -> (2, [("tout", ""); ("quit", "")])
    | _ ->
        (List.length clist + 3,
         clist @ [("autr", ""); ("tout", ""); ("quit", "")]) ]
  in
  let (width, height) =
    if nlin != nsel then do {
      term_set_params twid (nsel, ncol);
      rt_adjust_widget wid
    }
    else (rt_widget_width wid, rt_widget_height wid)
  in
  let x = (rt_widget_width mwid - width) / 2 in
  let y = (rt_widget_height mwid - height) / 2 in
  term_send twid "\027[2J\027[H\027[?35h";
  match clist with
  [ [] -> ()
  | _ -> do {
      List.iter
        (fun (_, lib) -> do { term_send twid lib; term_send twid "\n" })
        clist;
      term_send twid "Autre que carte\n"
    } ];
  term_send twid "Tout sélectionner\n";
  term_send twid "Quitter";
  state.buttonAct :=
    Action.button_select xd ["PT Question term"] lignes (fun _ -> 0)
      (select_ligne2 xd);
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_alert wid
};

value action_annule wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun (ligne, _, _) -> ligne.val.lselection := False)
    state.ptSelection;
  state.ptSelection := [];
  state.buttonAct := fun _ _ -> ();
  rt_unmap_widget (rt_widget_named xd "PT buttons");
  montrer_pointage xd (fun _ -> False);
  init_pointage xd
};

value action_fin wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter
    (fun (ligne, _, _) ->
       if ligne.val.lselection then do {
         modif xd;
         ligne.val.lselection := False;
         ligne.val.lretire := True
       }
       else ())
    state.ptSelection;
  rt_unmap_widget (rt_widget_named xd "PT buttons");
  montrer_pointage xd (fun _ -> False);
  init_pointage xd
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  state.action_quit := fun _ -> action_quit xd;
  rt_map_widget (rt_widget_named xd "POINTAGE");
  montrer_pointage xd (fun _ -> False);
  init_pointage xd
};

value wdesc =
  pack_desc [NameAtt "POINTAGE"]
    (DIRx,
     [(INCSZ,
       pack_desc [BandAtt 0]
         (DIRy,
          [(FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(INCSZ,
                 term_desc [NameAtt "PT titre"] (1, 1) Action.no_term);
                (FIXSZ,
                 term_desc [NameAtt "PT Page"] (1, 8) Action.no_term)])
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
                           term_desc [NameAtt "PT Selection titre"] (1, 1)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "PT Selection term"] (20, 1)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "PT Retire titre"] (1, 1)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "PT Retire term"] (20, 1)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "PT Date titre"] (1, 8)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "PT Date term"] (20, 8)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "PT Nature titre"] (1, 10)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "PT Nature term"] (20, 10)
                             Action.butt_term)])
                        Action.no_pack);
                     (INCSZ,
                      pack_desc [BandAtt 0; InterAtt 0]
                        (DIRx,
                         [(FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "PT Poste titre"] (1, 2)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "PT Poste term"] (20, 2)
                                  Action.butt_term)])
                             Action.no_pack);
                          (INCSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "PT Libelle titre"]
                                  (1, 20) Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "PT Libelle term"]
                                  (20, 20) Action.butt_term)])
                             Action.no_pack)])
                        Action.no_pack)])
                   Action.no_pack);
                (FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRy,
                    [(FIXSZ,
                      term_desc [NameAtt "PT Debit titre"] (1, 10)
                        Action.no_term);
                     (INCSZ,
                      term_desc [NameAtt "PT Debit term"] (20, 10)
                        Action.butt_term)])
                   Action.no_pack);
                (FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRy,
                    [(FIXSZ,
                      term_desc [NameAtt "PT Credit titre"] (1, 10)
                        Action.no_term);
                     (INCSZ,
                      term_desc [NameAtt "PT Credit term"] (20, 10)
                        Action.butt_term)])
                   Action.no_pack)])
              Action.no_pack);
           (FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRy,
                    [(FIXSZ,
                      term_desc [NameAtt "PT solde term"] (2, 30)
                        Action.no_term);
                     Action.filler])
                   Action.no_pack);
                Action.filler;
                (FIXSZ,
                 pack_desc []
                   (DIRz,
                    [(INCSZ,
                      pack_desc [BandAtt 0] (DIRy, []) Action.no_pack);
                     (INCSZ,
                      pack_desc [NameAtt "PT buttons"; BandAtt 0]
                        (DIRx,
                         [(FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [Action.filler;
                               (FIXSZ,
                                button_desc [] ("Fin", None)
                                  (Action.button action_fin));
                               Action.filler])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [Action.filler;
                               (FIXSZ,
                                button_desc [] ("Annule", None)
                                  (Action.button action_annule));
                               Action.filler])
                             Action.no_pack);
                          Action.filler])
                        Action.no_pack)])
                   Action.no_pack);
                Action.filler;
                (FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRy,
                    [(FIXSZ,
                      term_desc [NameAtt "PT Tdebit term"] (1, 10)
                        Action.no_term);
                     Action.filler])
                   Action.no_pack);
                (FIXSZ,
                 pack_desc [BandAtt 0]
                   (DIRy,
                    [(FIXSZ,
                      term_desc [NameAtt "PT Tcredit term"] (1, 10)
                        Action.no_term);
                     Action.filler])
                   Action.no_pack)])
              Action.no_pack)])
         Action.no_pack);
      (FIXSZ,
       pack_desc [BandAtt 0]
         (DIRy,
          [(FIXSZ, arrow_desc [] ORup up_arrow); Action.filler;
           (FIXSZ, arrow_desc [] ORdown down_arrow)])
         Action.no_pack)])
    Action.no_pack
;

value question_wdesc =
  pack_desc [NameAtt "PT Question"]
    (DIRy,
     [(FIXSZ,
       term_desc [NameAtt "PT Question titre"] (1, 31) Action.no_term);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ,
            term_desc [NameAtt "PT Question term"] (0, 17) Action.butt_term);
           Action.filler])
         Action.no_pack)])
    Action.no_pack
;
