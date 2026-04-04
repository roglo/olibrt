(* $Id: d_modifAuto.ml,v 1.6 2006/06/02 00:22:01 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value modifier_pic =
  "\
Nombre  : %3d
Dernier : %02d/%02d
Jour    : %02d
Poste   : %2s
Libellé : %20s
Débit   : %10.2f
Crédit  : %10.2f"
;

value ligne_marquee () =
  find_selected budget.virAuto where rec find_selected =
    fun
    [ [] -> failwith "ligne_marquee"
    | [l :: ll] -> if l.vselection then l else find_selected ll ]
;

value supprimer_ligne_marquee () =
  budget.virAuto :=
    enlever budget.virAuto where rec enlever =
      fun
      [ [] -> []
      | [v :: vl] -> if v.vselection then vl else [v :: enlever vl] ]
;

value init_modifier xd = do {
  let twid = rt_widget_named xd "MA MODIF term" in
  let ip = input_of_pic twid modifier_pic in
  let ligne = ligne_marquee () in
  term_send twid "\027[H\027[2J";
  term_send twid (empty_pic modifier_pic);
  set_field ip 0 (Fint ligne.vnombre);
  set_field ip 1 (Fint ligne.vmois);
  set_field ip 2 (Fint (ligne.vannee mod 100));
  set_field ip 3 (Fint ligne.vjour);
  match ligne.vposte with
  [ Some s -> set_field ip 4 (Fstring s)
  | _ -> () ];
  match ligne.vlibelle with
  [ Some s -> set_field ip 5 (Fstring s)
  | _ -> () ];
  match ligne.vmontant with
  [ Left n -> set_field ip 6 (Fdecimal (n / 100, n mod 100, 2))
  | Right n -> set_field ip 7 (Fdecimal (n / 100, n mod 100, 2)) ];
  goto_field ip 3 False;
  ip
};

value rec verif_modif ip = do {
  let twid = ip.iPwid in
  let xd = rt_xdata_of_widget twid in
  let nombre = get_int (get_field ip 0) in
  let mois = get_int (get_field ip 1) in
  let annee = get_int (get_field ip 2) in
  let jour = get_int (get_field ip 3) in
  let poste = uppercase (get_string (get_field ip 4)) in
  let debit = get_field ip 6 in
  let credit = get_field ip 7 in
  term_send twid "\0277";
  set_field ip 0 (Fint nombre);
  set_field ip 1 (Fint mois);
  set_field ip 2 (Fint (annee mod 100));
  set_field ip 3 (Fint jour);
  set_field ip 4 (Fstring poste);
  set_field ip 6 (get_field ip 6);
  set_field ip 7 (get_field ip 7);
  term_send twid "\0278";
  try do {
    if jour <= 0 || jour > 31 then raise (ErrField "Jour invalide" 3)
    else ();
    if debit = Fempty && credit = Fempty ||
       debit <> Fempty && credit <> Fempty
    then
      raise (ErrField "Renseignez débit ou crédit" 6)
    else ();
    match (trouver_poste poste, (debit, credit)) with
    [ ((True, _), (_, Fempty)) | ((_, True), (Fempty, _)) -> ()
    | ((False, False), _) -> raise (ErrField "Poste inexistant" 4)
    | _ -> raise (ErrField "Poste dans la mauvaise catégorie" 4) ];
    term_send twid "\027[?35h\027[?7l";
    state.action := ModifierOuiNon;
    rt_map_alert (rt_widget_named xd "MA Correct alert");
    state.keyPressAct :=
      fun _ ksym ->
        match ksym with
        [ Rt.K_Return -> action_correct_oui twid
        | Rt.K_Escape -> action_correct_non twid
        | _ -> () ];
    state.iP := Some ip
  }
  with
  [ ErrField str f -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err str;
      rt_map_widget err;
      goto_field ip f False
    } ]
}
and action_correct_oui wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  let ligne =
    {vselection = False; vnombre = get_int (get_field ip 0);
     vmois = get_int (get_field ip 1); vannee = get_int (get_field ip 2);
     vjour = get_int (get_field ip 3);
     vposte =
       match get_field ip 4 with
       [ Fstring s -> Some s
       | _ -> None ];
     vlibelle =
       match get_field ip 5 with
       [ Fstring s -> Some s
       | _ -> None ];
     vmontant =
       match (get_field ip 6, get_field ip 6) with
       [ (Fdecimal (m, d, 2), _) -> Left (m * 100 + d)
       | (_, Fdecimal (m, d, 2)) -> Right (m * 100 + d)
       | _ -> failwith "erreur interne dans ajouter Auto" ]}
  in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "MA NoErr");
  term_send (rt_widget_named xd "MA MODIF term") "\027[?35l\027[?7l";
  supprimer_ligne_marquee ();
  modif xd;
  state.nbMarques := state.nbMarques - 1;
  budget.virAuto := budget.virAuto @ [ligne];
  Show.lignes_auto xd;
  Show.selection_auto xd;
  boucle_modifier xd
}
and action_correct_non wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "MA MODIF term" in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "MA NoErr");
  term_send twid "\027[?35l\027[?7l";
  goto_field ip 3 False;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "MA NoErr"))
      verif_modif
}
and boucle_modifier xd =
  let wid = rt_widget_named xd "MA MODIF" in
  (*
    let twid = rt_widget_named xd "MA MODIF term" in
  *)
  if state.nbMarques > 0 then do {
    let mwid = rt_widget_named xd "Main" in
    let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
    let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
    let ip = init_modifier xd in
    state.keyPressAct :=
      Action.input ip
        (fun _ -> rt_map_widget (rt_widget_named xd "MA NoErr")) verif_modif;
    rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
    rt_map_widget wid
  }
  else do {
    rt_unmap_widget wid;
    List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
      ["Fichier"; "Traitement"];
    state.keyPressAct := fun _ _ -> ()
  }
;

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_map_widget (rt_widget_named xd "MA NoErr");
  let ligne = ligne_marquee () in
  ligne.vselection := False;
  state.nbMarques := state.nbMarques - 1;
  Show.selection_auto xd;
  boucle_modifier xd
};

value action_fin wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_map_widget (rt_widget_named xd "MA NoErr");
  state.buttonAct := fun _ _ -> ();
  boucle_modifier xd
};

value action_annule wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"];
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
    ["Fichier"; "Traitement"];
  rt_map_widget (rt_widget_named xd "MA Modifier action");
  state.buttonAct :=
    Action.button_select xd
      ["MA Nombre term"; "MA Dernier term"; "MA Jour term"; "MA Poste term";
       "MA Libelle term"; "MA Debit term"; "MA Credit term"]
      budget.virAuto (fun _ -> 0) (select_ligne xd);
  state.nbMarques := 0
};

value wdesc =
  pack_desc [NameAtt "MA MODIF"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "MA MODIF titre"] (1, 20) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "MA MODIF term"] (7, 30) Action.no_term)])
    Action.no_pack
;

value fin_annule_wdesc =
  pack_desc [NameAtt "MA Modifier action"; BandAtt 0]
    (DIRx,
     [(FIXSZ,
       term_desc [NameAtt "MA Modifier titre"] (1, 11) Action.no_term);
      (FIXSZ, button_desc [] ("Fin", None) (Action.button action_fin));
      (FIXSZ, button_desc [] ("Annule", None) (Action.button action_annule));
      Action.filler])
    Action.no_pack
;
