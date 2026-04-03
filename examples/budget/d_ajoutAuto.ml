(* $Id: d_ajoutAuto.ml,v 1.6 2006/06/02 00:22:00 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value ajout_pic =
  "\
Nombre  : %3d
Dernier : %02d/%02d
Jour    : %02d
Poste   : %2s
Libellé : %20s
Débit   : %10.2f
Crédit  : %10.2f"
;

value rec init_ajout xd = do {
  let twid = rt_widget_named xd "MA AJOUT term" in
  term_send twid "\027[H\027[2J\027[?35l";
  term_send twid (empty_pic ajout_pic);
  let ip = input_of_pic twid ajout_pic in
  goto_field ip 3 False;
  state.iP := Some ip;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "MA NoErr"))
      verif_ajout
}
and verif_ajout ip = do {
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
    rt_map_alert (rt_widget_named xd "MA Correct alert");
    state.keyPressAct :=
      fun _ ksym ->
        match ksym with
        [ Rt.K_Return -> action_correct_oui twid
        | Rt.K_Escape -> action_correct_non twid
        | _ -> () ]
  }
  with
  [ ErrField str f -> do {
      let err = rt_widget_named xd "MA Err" in
      Show.texte_centre err str;
      rt_map_widget err;
      goto_field ip f False
    } ]
}
and action_correct_oui wid = do {
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
       match (get_field ip 6, get_field ip 7) with
       [ (Fdecimal (m, d, 2), _) -> Left (m * 100 + d)
       | (_, Fdecimal (m, d, 2)) -> Right (m * 100 + d)
       | _ -> failwith "erreur interne dans ajouter Auto" ]}
  in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "MA NoErr");
  state.iP := None;
  budget.virAuto := budget.virAuto @ [ligne];
  modif xd;
  Show.lignes_auto xd;
  init_ajout xd
}
and action_correct_non wid = do {
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "MA AJOUT term" in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "MA NoErr");
  term_send twid "\027[?35l\027[?7l";
  goto_field ip 3 False;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "MA NoErr"))
      verif_ajout
};

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Quitter"];
  rt_unmap_widget (rt_widget_named xd "MA AJOUT");
  rt_map_widget (rt_widget_named xd "MA NoErr");
  state.keyPressAct := fun _ _ -> ();
  state.action := Rien
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "MA AJOUT" in
  let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
  let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Quitter"];
  init_ajout xd;
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid;
  state.action := Ajoutant
};

value wdesc =
  pack_desc [NameAtt "MA AJOUT"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "MA AJOUT titre"] (1, 20) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "MA AJOUT term"] (7, 30) Action.no_term)])
    Action.no_pack
;
