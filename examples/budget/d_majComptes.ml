(* $Id: d_majComptes.ml,v 1.5 2006/06/02 00:22:01 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value comptes_pic =
  make 1 "" "" where rec make i sep s =
    if i > Const.nb_comptes then s
    else
      let s' =
        nom_compte i ^ " : %" ^ string_of_int Const.taille_nom_compte ^ "s"
      in
      make (succ i) "\n" (s ^ sep ^ s')
;

value maj_budget comptes = do {
  let (cs, _) =
    List.fold_left
      (fun (l, i) x ->
         let n = nom_compte i in
         let v =
           match x with
           [ Some _ -> True
           | None -> False ]
         in
         ([(n, v) :: l], succ i))
      ([], 1) comptes
  in
  List.iter
    (fun m ->
       List.iter
         (fun l ->
            match l.lposte with
            [ Some p ->
                try
                  if not (List.assoc p cs) then l.lposte := None else ()
                with
                [ Not_found -> () ]
            | _ -> () ])
         m.lignes)
    budget.listeMois;
  List.iter
    (fun v ->
       match v.vposte with
       [ Some p ->
           try if not (List.assoc p cs) then v.vposte := None else () with
           [ Not_found -> () ]
       | _ -> () ])
    budget.virAuto
};

value get_list ip debut fin =
  iterate debut budget.comptes where rec iterate i l =
    if i >= fin then l
    else
      let (c, l) =
        match get_field ip i with
        [ Fstring lib ->
            match l with
            [ [Some (_, n) :: l] -> (Some (lib, n), l)
            | [None :: l] -> (Some (lib, 0), l)
            | [] -> (Some (lib, 0), []) ]
        | _ ->
            match l with
            [ [_ :: l] -> (None, l)
            | [] -> (None, []) ] ]
      in
      [c :: iterate (succ i) l]
;

value comptes_of_ip ip = get_list ip 0 Const.nb_comptes;

value rec verif_maj_comptes ip =
  let xd = rt_xdata_of_widget ip.iPwid in
  try do {
    let comptes = comptes_of_ip ip in
    init_maj_comptes xd comptes;
    term_send ip.iPwid "\027[?35h";
    state.action := MajComptesOuiNon;
    rt_map_alert (rt_widget_named xd "Correct alert");
    state.keyPressAct :=
      fun _ ksym ->
        match ksym with
        [ Rt.K_Return -> action_correct_oui ip.iPwid
        | Rt.K_Escape -> action_correct_non ip.iPwid
        | _ -> () ]
  }
  with
  [ ErrField str f -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err str;
      rt_map_widget err;
      goto_field ip f False
    } ]
and action_correct_oui wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  let comptes = comptes_of_ip ip in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  rt_unmap_widget (rt_widget_named xd "MAJ COMPTES");
  state.keyPressAct := fun _ _ -> ();
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Comptes spéciaux";
     "Calculatrice"; "Bloc-notes"; "Mise à jour"];
  maj_budget comptes;
  budget.comptes :=
    let rec glip =
      fun
      [ [None :: l] -> glip l
      | l -> l ]
    in
    List.rev (glip (List.rev comptes));
  modif xd;
  Show.lignes xd state.noPage;
  Show.repart_mois xd;
  state.action := Rien
}
and action_correct_non wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  goto_field ip 0 False;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      verif_maj_comptes
}
and init_maj_comptes xd comptes = do {
  let twid = rt_widget_named xd "MAJ COMPTES term" in
  let ip = input_of_pic twid comptes_pic in
  term_send twid "\027[H\027[2J\027[?35h";
  term_send twid (empty_pic comptes_pic);
  let rec fill i =
    fun
    [ [compte :: comptes] -> do {
        let s =
          match compte with
          [ Some (lib, _) -> Fstring lib
          | None -> Fempty ]
        in
        set_field ip i s;
        fill (succ i) comptes
      }
    | [] -> () ]
  in
  fill 0 comptes;
  goto_field ip 0 False;
  state.iP := Some ip;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      verif_maj_comptes
};

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Comptes spéciaux";
     "Calculatrice"; "Bloc-notes"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "NoErr");
  rt_unmap_widget (rt_widget_named xd "MAJ COMPTES");
  state.action := Rien;
  state.keyPressAct := fun _ _ -> ()
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "MAJ COMPTES" in
  let twid = rt_widget_named xd "MAJ COMPTES term" in
  let (nlin, ncol) = term_get_params twid in
  let (width, height) =
    if Const.nb_comptes != nlin then do {
      term_set_params twid (Const.nb_comptes, ncol);
      rt_adjust_widget wid
    }
    else (rt_widget_width wid, rt_widget_height wid)
  in
  let x = (rt_widget_width mwid - width) / 2 in
  let y = (rt_widget_height mwid - height) / 2 in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Comptes spéciaux";
     "Calculatrice"; "Bloc-notes"; "Mise à jour"];
  init_maj_comptes xd budget.comptes;
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid;
  rt_unmap_widget (rt_widget_named xd "INFO COMPTES")
};

value wdesc =
  pack_desc [NameAtt "MAJ COMPTES"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "MAJ COMPTES titre"] (1, 1) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       term_desc [NameAtt "MAJ COMPTES term"]
         (Const.nb_comptes, 6 + Const.taille_nom_compte) Action.no_term)])
    Action.no_pack
;
