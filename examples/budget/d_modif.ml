(* $Id: d_modif.ml,v 1.6 2006/06/02 00:22:01 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value init_modifier xd = do {
  let twid = rt_widget_named xd "MODIF term" in
  let ip = input_of_pic twid AjoutModif.image in
  term_send twid "\027[H\027[2J";
  term_send twid (empty_pic AjoutModif.image);
  set_field ip 2 (Fint mois.val.mois);
  set_field ip 3 (Fint (mois.val.annee mod 100));
  lock_field ip 2;
  lock_field ip 3;
  do {
    let ligne = State.ligne_marquee () in
   if ligne.lretire then set_field ip 0 (Fstring "X") else ();
   set_field ip 1 (Fint ligne.ljour);
   match ligne.lnature with
   [ Some s -> set_field ip 4 (Fstring s)
   | _ -> () ];
   match ligne.lposte with
   [ Some s -> set_field ip 5 (Fstring s)
   | _ -> () ];
   match ligne.llibelle with
   [ Some s -> set_field ip 6 (Fstring s)
   | _ -> () ];
   match ligne.lmontant with
   [ Left n -> set_field ip 7 (Fdecimal (n / 100, n mod 100, 2))
   | Right n -> set_field ip 8 (Fdecimal (n / 100, n mod 100, 2)) ];
   goto_field ip 1 False };
  ip
};

value rec action_correct_oui wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  term_send (rt_widget_named xd "MODIF term") "\027[?35l\027[?7l";
  supprimer_ligne_marquee xd;
  let (ind, ligne) = ajouter_ligne ip False in
  state.noPage := ind / state.nlin;
  state.nbMarques := state.nbMarques - 1;
  maj_total \+ ligne;
  Show.lignes xd state.noPage;
  Show.selection xd state.noPage;
  Show.total xd;
  Show.solde_reduit xd;
  Show.repart_mois xd;
  boucle_modifier xd
}
and action_correct_non wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "MODIF term" in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  term_send twid "\027[?35l\027[?7l";
  goto_field ip 1 False;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      (fun ip ->
         if AjoutModif.verif ip then do {
           state.keyPressAct :=
             fun _ ksym ->
               match ksym with
               [ Rt.K_Return -> action_correct_oui twid
               | Rt.K_Escape -> action_correct_non twid
               | _ -> () ];
           state.action := ModifierOuiNon;
           state.iP := Some ip
         }
         else ())
}
and boucle_modifier xd =
  let wid = rt_widget_named xd "MODIF" in
  let twid = rt_widget_named xd "MODIF term" in
  if state.nbMarques > 0 then do {
    let mwid = rt_widget_named xd "Main" in
    let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
    let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
    let ip = init_modifier xd in
    state.keyPressAct :=
      Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
        (fun ip ->
           if AjoutModif.verif ip then do {
             state.keyPressAct :=
               fun _ ksym ->
                 match ksym with
                 [ Rt.K_Return -> action_correct_oui twid
                 | Rt.K_Escape -> action_correct_non twid
                 | _ -> () ];
             state.action := ModifierOuiNon;
             state.iP := Some ip
           }
           else ());
    rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
    rt_map_widget wid
  }
  else do {
    rt_unmap_widget wid;
    List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
      ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
    state.keyPressAct := fun _ _ -> ()
  }
;

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_map_widget (rt_widget_named xd "NoErr");
  let ligne = ligne_marquee () in
  ligne.lselection := False;
  maj_total \+ ligne;
  state.nbMarques := state.nbMarques - 1;
  Show.selection xd state.noPage;
  Show.total xd;
  boucle_modifier xd
};

value action_fin wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_map_widget (rt_widget_named xd "NoErr");
  state.buttonAct := fun _ _ -> ();
  boucle_modifier xd
};

value action_annule wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "NoErr");
  state.buttonAct := fun _ _ -> ();
  List.iter
    (fun ligne ->
       if ligne.lselection then do {
         ligne.lselection := False;
         maj_total \+ ligne;
         state.nbMarques := state.nbMarques - 1
       }
       else ())
    mois.val.lignes;
  state.action := Rien;
  Show.selection xd state.noPage;
  Show.total xd
};

value select_ligne xd ligne = do {
  if ligne.lselection then do {
    ligne.lselection := False;
    state.nbMarques := state.nbMarques - 1;
    maj_total \+ ligne
  }
  else do {
    ligne.lselection := True;
    state.nbMarques := state.nbMarques + 1;
    maj_total \- ligne
  };
  Show.selection xd state.noPage;
  Show.total xd
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Fichier"; "Traitement"; "Budget"; "Informations"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "Modifier action");
  state.buttonAct :=
    Action.button_select xd
      ["Retire term"; "Date term"; "Nature term"; "Poste term";
       "Libelle term"; "Debit term"; "Credit term"]
      mois.val.lignes (fun _ -> state.noPage * state.nlin) (select_ligne xd);
  state.nbMarques := 0
};

value wdesc =
  pack_desc [NameAtt "MODIF"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "MODIF titre"] (1, 20) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "MODIF term"] (7, 30) Action.no_term)])
    Action.no_pack
;

value fin_annule_wdesc =
  pack_desc [NameAtt "Modifier action"; BandAtt 0]
    (DIRx,
     [(FIXSZ, term_desc [NameAtt "Modifier titre"] (1, 11) Action.no_term);
      (FIXSZ, button_desc [] ("Fin", None) (Action.button action_fin));
      (FIXSZ, button_desc [] ("Annule", None) (Action.button action_annule));
      Action.filler])
    Action.no_pack
;
