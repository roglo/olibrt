(* $Id: d_majCartes.ml,v 1.6 2006/06/02 00:22:01 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value cartes_pic =
  "1) %2s %20s\n2) %2s %20s\n3) %2s %20s\n4) %2s %20s\n5) %2s %20s"
;

value get_list ip debut fin plist =
  iterate plist debut where rec iterate plist i =
    if i >= fin then ([], plist)
    else
      match get_field ip (2 * i) with
      [ Fstring nom -> do {
          let nom = uppercase nom in
          set_field ip (2 * i) (Fstring nom);
          if List.mem nom plist then
            raise (ErrField "Code carte en double" (2 * i))
          else ();
          let lib =
            match get_field ip (2 * i + 1) with
            [ Fstring lib -> lib
            | _ ->
                raise
                  (ErrField "Il manque le libellé de la carte" (2 * i + 1)) ]
          in
          let (l, plist) = iterate [nom :: plist] (succ i) in
          ([(nom, lib) :: l], plist)
        }
      | _ ->
          match get_field ip (2 * i + 1) with
          [ Fstring _ -> raise (ErrField "Code carte absent" (2 * i))
          | _ -> iterate plist (succ i) ] ]
;

value cartes_of_ip ip = fst (get_list ip 0 5 []);

value rec verif_maj_cartes ip =
  let xd = rt_xdata_of_widget ip.iPwid in
  try do {
    let cartes = cartes_of_ip ip in
    init_maj_cartes xd cartes;
    term_send ip.iPwid "\027[?35h";
    state.action := MajCartesOuiNon;
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
  let cartes = cartes_of_ip ip in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  rt_unmap_widget (rt_widget_named xd "MAJ CARTES");
  state.keyPressAct := fun _ _ -> ();
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Cartes"; "Calculatrice";
     "Bloc-notes"; "Mise à jour"];
  budget.cartes := cartes;
  modif xd;
  state.action := Rien;
  Show.solde_reduit xd
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
      verif_maj_cartes
}
and init_maj_cartes xd cartes = do {
  let twid = rt_widget_named xd "MAJ CARTES term" in
  let ip = input_of_pic twid cartes_pic in
  term_send twid "\027[H\027[2J\027[?35h";
  term_send twid (empty_pic cartes_pic);
  fill 0 cartes where rec fill i =
    fun
    [ [(cc, cl) :: cartes] -> do {
        set_field ip (2 * i) (Fstring cc);
        set_field ip (2 * i + 1) (Fstring cl);
        fill (succ i) cartes
      }
    | [] -> () ];
  goto_field ip 0 False;
  state.iP := Some ip;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      verif_maj_cartes
};

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Cartes"; "Calculatrice";
     "Bloc-notes"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "NoErr");
  rt_unmap_widget (rt_widget_named xd "MAJ CARTES");
  state.action := Rien;
  state.keyPressAct := fun _ _ -> ()
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "MAJ CARTES" in
  let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
  let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Cartes"; "Calculatrice";
     "Bloc-notes"; "Mise à jour"];
  init_maj_cartes xd budget.cartes;
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid;
  rt_unmap_widget (rt_widget_named xd "INFO CARTES")
};

value wdesc =
  pack_desc [NameAtt "MAJ CARTES"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "MAJ CARTES titre"] (1, 22) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "MAJ CARTES term"] (5, 26) Action.no_term)])
    Action.no_pack
;
