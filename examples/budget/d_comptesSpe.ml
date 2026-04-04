(* $Id: d_comptesSpe.ml,v 1.6 2006/06/07 14:53:39 deraugla Exp $ *)
(* inutilisé bicoz problème conceptuel... *)

open State;
open File;
open RtN;

value modif_image =
  "\
Code         : %2s
Libellé      : %15s
Quantité     : %12.5f
Cours        : %12.2f
Valorisation : %12.2f"
;

value string_of_decimal len (m, d, n) =
  let s =
    let s = string_of_int d in
    if n > String.length s then String.make (n - String.length s) '0' ^ s
    else String.sub s 0 n
  in
  let s = string_of_int m ^ "," ^ s in
  if String.length s > len then String.make len '#'
  else String.make (len - String.length s) ' ' ^ s
;

value string_of_decimal_int len n v =
  string_of_decimal len (decimal_of_int n v)
;

value ligne_marquee () =
  find_selected state.cptLignes where find_selected =
    fun
    [ [] -> failwith "ligne_marquee"
    | [l :: ll] -> if l.cs_sel then l else find_selected ll ]
;

value init_modifier xd = do {
  let twid = rt_widget_named xd "MODIF CPTE term" in
  let ip = Jmage.input_of_pic twid modif_image in
  let ligne = ligne_marquee () in
  let valo = ligne.cs_valo in
  term_send twid "\027[H\027[2J";
  term_send twid (Jmage.empty_pic modif_image);
  Jmage.set_field ip 0 (Jmage.Fstring ligne.cs_code);
  Jmage.set_field ip 1 (Jmage.Fstring ligne.cs_lib);
  Jmage.lock_field ip 0;
  Jmage.lock_field ip 1;
  Jmage.set_field ip 4 (Jmage.Fdecimal (decimal_of_int 2 valo));
  Jmage.goto_field ip 2 False;
  ip
};

value montrer_selection xd = do {
  let ecr = "CS " in
  let lignes = state.cptLignes in
  let selection_wid = rt_widget_named xd (ecr ^ "Selection term") in
  term_send selection_wid "\027[H\027[2J\027[?35h\027[?7l";
  let _ =
    List.fold_left
      (fun n l -> do {
         if l.cs_sel then term_send selection_wid "*" else ();
         term_send selection_wid "\n";
         succ n
       ))
      0 lignes
  in
  ()
};

value montrer_lignes xd = do {
  let ecr = "CS " in
  let code_wid = rt_widget_named xd (ecr ^ "Code term") in
  let libelle_wid = rt_widget_named xd (ecr ^ "Libelle term") in
  let quantite_wid = rt_widget_named xd (ecr ^ "Quantite term") in
  let cours_wid = rt_widget_named xd (ecr ^ "Cours term") in
  let valo_wid = rt_widget_named xd (ecr ^ "Valorisation term") in
  List.iter (fun w -> term_send w "\027[H\027[2J\027[?35h\027[?7l")
    [code_wid; libelle_wid; quantite_wid; cours_wid; valo_wid];
  List.iter
    (fun ligne -> do {
       term_send code_wid ligne.cs_code;
       term_send libelle_wid ligne.cs_lib;
       if ligne.cs_quantite = 0 then ()
       else
         term_send quantite_wid
           (string_of_decimal_int 11 5 ligne.cs_quantite);
       if ligne.cs_cours = 0 then ()
       else term_send cours_wid (string_of_decimal_int 8 2 ligne.cs_cours);
       term_send valo_wid (string_of_decimal_int 11 2 ligne.cs_valo);
       term_send code_wid "\n";
       term_send libelle_wid "\n";
       term_send quantite_wid "\n";
       term_send cours_wid "\n";
       term_send valo_wid "\n"
     ))
    state.cptLignes
};

value select_ligne xd ligne = do {
  if ligne.cs_sel then do {
    ligne.cs_sel := False;
    state.nbMarques := state.nbMarques - 1
  )
  else do {
    ligne.cs_sel := True;
    state.nbMarques := state.nbMarques + 1
  );
  montrer_selection xd
};

value quantite_cours_valo ip =
  let q = Jmage.get_field ip 2 in
  let c = Jmage.get_field ip 3 in
  let v = Jmage.get_field ip 4 in
  match (q, c, v) with
  [ (Jmage.Fdecimal qi, Jmage.Fdecimal ci, _) ->
      let vi =
        decimal_of_float 2 (float_of_decimal qi *. float_of_decimal ci)
      in
      (q, c, Jmage.Fdecimal vi)
  | (_, Jmage.Fdecimal ci, Jmage.Fdecimal vi) ->
      let qi =
        decimal_of_float 5 (float_of_decimal vi /. float_of_decimal ci)
      in
      let vi =
        decimal_of_float 2 (float_of_decimal qi *. float_of_decimal ci)
      in
      (Jmage.Fdecimal qi, c, Jmage.Fdecimal vi)
  | (Jmage.Fdecimal qi, _, Jmage.Fdecimal vi) ->
      let ci =
        decimal_of_float 2 (float_of_decimal vi /. float_of_decimal qi)
      in
      let vi =
        decimal_of_float 2 (float_of_decimal qi *. float_of_decimal ci)
      in
      (q, Jmage.Fdecimal ci, Jmage.Fdecimal vi)
  | _ ->
      (q, c, v) ]
;

value verif ip = do {
  let twid = ip.Jmage.iPwid in
  let xd = rt_xdata_of_widget twid in
  let (quantite, cours, valo) = quantite_cours_valo ip in
  Jmage.set_field ip 2 quantite;
  Jmage.set_field ip 3 cours;
  Jmage.set_field ip 4 valo;
  term_send twid "\027[?35h\027[?7l";
  rt_map_alert (rt_widget_named xd "CS Correct alert");
  True
};

value rec action_correct_oui wid = do {
  let xd = rt_xdata_of_widget wid in
  let ligne = ligne_marquee () in
  match state.iP with
  [ Some ip -> do {
      let (quantite, cours, valo) = quantite_cours_valo ip in
      let n_valo =
        match valo with
        [ Jmage.Fdecimal d -> int_of_decimal d
        | _ -> ligne.cs_valo ]
      in
      budget.comptes :=
        insert budget.comptes where insert =
          fun
          [ [Some c :: l] ->
              if c.ccode = ligne.cs_code then
                let v = valorisation c.creport in
                let r =
                  match (quantite, cours) with
                  [ (Jmage.Fdecimal q, Jmage.Fdecimal c) ->
                      let v = v - ligne.cs_valo + n_valo in
                      let q = float v /. float_of_decimal c /. 100. in
                      let q = decimal_of_float 5 q in
                      let q = int_of_decimal q in
                      let c = int_of_decimal c in
                      Titres {quantite = q; cours = c}
                  | _ ->
                      let v = v - ligne.cs_valo + n_valo in
                      if v < 0 then Debit (- v) else Credit v ]
                in
                [Some {(c) with creport = r} :: l]
              else [Some c :: insert l]
          | [None :: l] -> [None :: insert l]
          | [] -> [] ];
      modif xd;
      match quantite with
      [ Jmage.Fdecimal d -> ligne.cs_quantite := int_of_decimal d
      | _ -> () ];
      match cours with
      [ Jmage.Fdecimal d -> ligne.cs_cours := int_of_decimal d
      | _ -> () ];
      match valo with
      [ Jmage.Fdecimal d -> ligne.cs_valo := int_of_decimal d
      | _ -> () ]
    )
  | None -> () ];
  ligne.cs_sel := False;
  state.nbMarques := state.nbMarques - 1;
  montrer_lignes xd;
  montrer_selection xd;
  rt_unmap_alert xd;
  boucle_modifier xd
)
and action_correct_non wid = do {
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "MODIF CPTE term" in
  state.action := ModCompteOuiNon;
  match state.iP with
  [ Some ip -> do {
      Jmage.goto_field ip 2 False;
      state.keyPressAct := action_lire xd twid ip
    )
  | None -> () ];
  rt_map_widget (rt_widget_named xd "CS NoErr");
  rt_unmap_alert xd
)
and action_lire xd twid ip =
  Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "CS NoErr"))
    (fun ip -> do {
       rt_map_alert (rt_widget_named xd "CS Correct alert");
       if verif ip then
         state.keyPressAct :=
           fun a b ->
             match (a, b) with
             [ (_, 0xff0d) -> action_correct_oui twid
             | (_, 0xff1b) -> action_correct_non twid
             | (_, _) -> () ]
       else ()
     ))
and boucle_modifier xd =
  let wid = rt_widget_named xd "MODIF CPTE" in
  let twid = rt_widget_named xd "MODIF CPTE term" in
  if state.nbMarques > 0 then do {
    let mwid = rt_widget_named xd "Main" in
    let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
    let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
    let ip = init_modifier xd in
    state.iP := Some ip;
    state.action := ModCompteOuiNon;
    state.keyPressAct := action_lire xd twid ip;
    rt_map_widget (rt_widget_named xd "CS NoErr");
    rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
    rt_map_widget wid
  )
  else do {
    rt_unmap_widget wid;
    state.keyPressAct := fun _ _ -> ();
    state.buttonAct :=
      Action.button_select xd
        ["CS Code term"; "CS Libelle term"; "CS Quantite term";
         "CS Cours term"; "CS Valorisation term"]
        state.cptLignes (fun _ -> 0) (select_ligne xd);
    rt_map_widget (rt_widget_named xd "CS Cptes Spe action")
  )
;

value action_modifier wid = do {
  let xd = rt_xdata_of_widget wid in
  state.buttonAct := fun _ _ -> ();
  boucle_modifier xd
};

value action_quitter wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  state.buttonAct := fun _ _ -> ();
  List.iter
    (fun ligne ->
       if ligne.cs_sel then do {
         ligne.cs_sel := False;
         state.nbMarques := state.nbMarques - 1
       )
       else ())
    state.cptLignes;
  state.action := Rien;
  state.cptLignes := [];
  rt_unmap_widget (rt_widget_named xd "CPTE SPE")
};

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  let ligne = ligne_marquee () in
  ligne.cs_sel := False;
  state.nbMarques := state.nbMarques - 1;
  montrer_selection xd;
  boucle_modifier xd
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "CPTE SPE");
  rt_map_widget (rt_widget_named xd "CS Cptes Spe action");
  state.cptLignes :=
    let soldes = (File.calculer_soldes budget).soldeComptes in
    let (comptes, _) =
      List.fold_left
        (fun (cl, n) c ->
           let cl =
             match c with
             [ Some c ->
                 let code = File.nom_compte n in
                 let valo = List.assoc code soldes in
                 let ligne =
                   {cs_lib = c.clibelle; cs_code = code; cs_quantite = 0;
                    cs_cours = 0; cs_valo = valo; cs_sel = False}
                 in
                 [ligne :: cl]
             | None -> cl ]
           in
           (cl, n + 1))
        ([], 1) budget.comptes
    in
    List.sort compare comptes;
  state.buttonAct :=
    Action.button_select xd
      ["CS Code term"; "CS Libelle term"; "CS Quantite term";
       "CS Cours term"; "CS Valorisation term"]
      state.cptLignes (fun _ -> 0) (select_ligne xd);
  state.nbMarques := 0;
  montrer_lignes xd;
  montrer_selection xd
};

value fin_annule_wdesc =
  pack_desc [NameAtt "CS Cptes Spe action"; BandAtt 0]
    (DIRx,
     [(FIXSZ,
       term_desc [NameAtt "CS Cptes Spe titre"] (1, 19) Action.no_term);
      (FIXSZ,
       button_desc [] ("Modifier", None) (Action.button action_modifier));
      (FIXSZ,
       button_desc [] ("Quitter", None) (Action.button action_quitter));
      Action.filler])
    Action.no_pack
;

value nlin = Const.nb_comptes;

value wdesc =
  pack_desc [NameAtt "CPTE SPE"]
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
                 term_desc [NameAtt "CPTE SPE titre"] (1, 1) Action.no_term);
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
                                term_desc [NameAtt "CS Selection titre"]
                                  (1, 1) Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "CS Selection term"]
                                  (nlin, 1) Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "CS Code titre"] (1, 2)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "CS Code term"] (nlin, 2)
                                  Action.butt_term)])
                             Action.no_pack);
                          (FIXSZ,
                           pack_desc [BandAtt 0]
                             (DIRy,
                              [(FIXSZ,
                                term_desc [NameAtt "CS Libelle titre"]
                                  (1, Const.taille_nom_compte)
                                  Action.no_term);
                               (INCSZ,
                                term_desc [NameAtt "CS Libelle term"]
                                  (nlin, Const.taille_nom_compte)
                                  Action.butt_term)])
                             Action.no_pack)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "CS Quantite titre"] (1, 12)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "CS Quantite term"] (nlin, 12)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "CS Cours titre"] (1, 8)
                             Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "CS Cours term"] (nlin, 8)
                             Action.butt_term)])
                        Action.no_pack);
                     (FIXSZ,
                      pack_desc [BandAtt 0]
                        (DIRy,
                         [(FIXSZ,
                           term_desc [NameAtt "CS Valorisation titre"]
                             (1, 12) Action.no_term);
                          (INCSZ,
                           term_desc [NameAtt "CS Valorisation term"]
                             (nlin, 12) Action.butt_term)])
                        Action.no_pack)])
                   Action.no_pack)])
              Action.no_pack);
           Action.filler])
         Action.no_pack);
      Action.filler;
      (FIXSZ,
       pack_desc [BandAtt 0]
         (DIRx,
          [(FIXSZ,
            pack_desc [BandAtt 0]
              (DIRz,
               [(INCSZ, fin_annule_wdesc);
                (INCSZ,
                 pack_desc [NameAtt "CS Correct alert"; BandAtt 0]
                   (DIRx,
                    [(FIXSZ,
                      term_desc [NameAtt "CS Correct titre"] (1, 10)
                        Action.no_term);
                     (FIXSZ,
                      button_desc [] ("Oui", None)
                        (Action.button action_correct_oui));
                     (FIXSZ,
                      button_desc [] ("Non", None)
                        (Action.button action_correct_non));
                     Action.filler])
                   Action.no_pack);
                (INCSZ,
                 pack_desc [NameAtt "CS NoErr"] (DIRx, []) Action.no_pack)])
              Action.no_pack);
           Action.filler])
         Action.no_pack)])
    Action.no_pack
;

value modif_wdesc =
  pack_desc [NameAtt "MODIF CPTE"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "MODIF CPTE titre"] (1, 20) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "MODIF CPTE term"] (5, 30) Action.no_term)])
    Action.no_pack
;
