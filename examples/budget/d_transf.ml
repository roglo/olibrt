(* $Id: d_transf.ml,v 1.5 2006/06/02 00:22:01 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value image =
  "\
Retiré    : X
Date      : %02d/%02d/%02d
Nature    : %10s
Poste src : %2s
Poste dst : %2s
Libellé   : %20s
Somme     : %10.2f"
;

value verif ip = do {
  let twid = ip.iPwid in
  let xd = rt_xdata_of_widget twid in
  let jour = get_field ip 0 in
  let nature =
    let f = get_string (get_field ip 3) in
    let u = uppercase f in
    if List.mem_assoc u budget.cartes then u else f
  in
  let poste_src = uppercase (get_string (get_field ip 4)) in
  let poste_dst = uppercase (get_string (get_field ip 5)) in
  let somme = get_field ip 7 in
  term_send twid "\0277";
  if get_field ip 0 <> Fempty then set_field ip 0 (Fstring "X") else ();
  set_field ip 0 (get_field ip 0);
  set_field ip 3 (Fstring nature);
  set_field ip 4 (Fstring poste_src);
  set_field ip 5 (Fstring poste_dst);
  set_field ip 6 (get_field ip 6);
  set_field ip 7 (get_field ip 7);
  term_send twid "\0278";
  try do {
    if let j = get_int jour in
       j <= 0 || j > 31
    then
      raise (ErrField "Jour invalide" 0)
    else ();
    if somme = Fempty then
      raise (ErrField "Renseignez la somme à transférer" 7)
    else ();
    match (trouver_poste poste_src, trouver_poste poste_dst) with
    [ ((_, True), (True, _)) -> ()
    | ((False, False), _) -> raise (ErrField "Poste source inexistant" 4)
    | (_, (False, False)) ->
        raise (ErrField "Poste destination inexistant" 5)
    | ((_, False), _) -> raise (ErrField "Poste source incorrect" 4)
    | (_, (False, _)) -> raise (ErrField "Poste destination incorrect" 5) ];
    term_send twid "\027[?35h\027[?7l";
    rt_map_alert (rt_widget_named xd "Correct alert");
    True
  }
  with
  [ ErrField str f -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err str;
      rt_map_widget err;
      goto_field ip f False;
      False
    } ]
};

value ajouter_lignes ip incno = do {
  let somme =
    match get_field ip 7 with
    [ Fdecimal (m, d, 2) -> m * 100 + d
    | _ -> failwith "erreur interne dans ajouter_lignes" ]
  in
  let nature =
    match get_field ip 3 with
    [ Fstring s -> Some s
    | _ -> None ]
  in
  let libelle =
    match get_field ip 6 with
    [ Fstring s -> Some s
    | _ -> None ]
  in
  let ligne1 =
    {lselection = False; lretire = True; ljour = get_int (get_field ip 0);
     lnature = nature;
     lposte =
       match get_field ip 4 with
       [ Fstring s -> Some s
       | _ -> None ];
     llibelle = libelle; lmontant = Right somme}
  in
  let ligne2 =
    {lselection = False; lretire = True; ljour = get_int (get_field ip 0);
     lnature = nature;
     lposte =
       match get_field ip 5 with
       [ Fstring s -> Some s
       | _ -> None ];
     llibelle = libelle; lmontant = Left somme}
  in
  let (i, lignes) =
    add 0 mois.val.lignes where rec add i =
      fun
      [ [l :: ll] ->
          if ligne1.ljour < l.ljour then (i, [ligne1; ligne2; l :: ll])
          else
            let (i, lignes) = add (succ i) ll in
            (i, [l :: lignes])
      | [] -> (i, [ligne1; ligne2]) ]
  in
  mois.val.lignes := lignes;
  if incno then
    match get_field ip 3 with
    [ Fstring n -> try budget.noCheque := succ (int_of_string n) with _ -> ()
    | _ -> () ]
  else ();
  modif (rt_xdata_of_widget ip.iPwid);
  i
};

value rec init_transferer xd = do {
  let twid = rt_widget_named xd "TRANSF term" in
  let ip = input_of_pic twid image in
  term_send twid "\027[H\027[2J\027[?35l";
  term_send twid (empty_pic image);
  set_field ip 1 (Fint mois.val.mois);
  set_field ip 2 (Fint (mois.val.annee mod 100));
  set_field ip 3 (Fstring (string_of_int budget.noCheque));
  lock_field ip 1;
  lock_field ip 2;
  goto_field ip 0 False;
  state.iP := Some ip;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      (fun ip ->
         if verif ip then
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
  let ind = ajouter_lignes ip True in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  state.noPage := ind / state.nlin;
  state.iP := None;
  Show.lignes xd state.noPage;
  Show.total xd;
  Show.solde_reduit xd;
  Show.repart_mois xd;
  init_transferer xd
}
and action_correct_non wid = do {
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "TRANSF term" in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  term_send twid "\027[?35l\027[?7l";
  goto_field ip 0 False;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      (fun ip ->
         if verif ip then
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
  rt_unmap_widget (rt_widget_named xd "TRANSF");
  rt_map_widget (rt_widget_named xd "NoErr");
  state.keyPressAct := fun _ _ -> ();
  state.action := Rien
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "TRANSF" in
  let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
  let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "Calculatrice"; "Bloc-notes";
     "Mise à jour"];
  init_transferer xd;
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid;
  state.action := Transferant
};

value wdesc =
  pack_desc [NameAtt "TRANSF"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "TRANSF titre"] (1, 20) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "TRANSF term"] (7, 32) Action.no_term)])
    Action.no_pack
;
