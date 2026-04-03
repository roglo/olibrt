(* $Id: b_init.ml,v 1.7 2006/06/02 00:22:00 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value initialisation_pic =
  "\
    Initialisation
    --------------

Entrez le premier mois

  mois/année : %02d/%02d%02d"
;

value rec verif_init ip = do {
  let xd = rt_xdata_of_widget ip.iPwid in
  let m = get_int (get_field ip 0) in
  let a1 = get_int (get_field ip 1) in
  let a2 = get_int (get_field ip 2) in
  set_field ip 0 (Fint m);
  set_field ip 1 (Fint a1);
  set_field ip 2 (Fint a2);
  try
    if m < 1 || m > 12 then raise (ErrField "Mois invalide" 0)
    else do {
      term_send ip.iPwid "\027[?35h";
      state.keyPressAct :=
        fun _ ksym ->
          match ksym with
          [ Rt.K_Return -> action_correct_oui ip.iPwid
          | Rt.K_Escape -> action_correct_non ip.iPwid
          | _ -> () ];
      rt_unmap_widget (rt_widget_named xd "Init annule");
      rt_map_alert (rt_widget_named xd "Init Correct alert")
    }
  with
  [ ErrField str f -> do {
      let err = rt_widget_named xd "Init Err" in
      Show.texte_centre err str;
      rt_map_widget err;
      goto_field ip f False
    } ]
}
and action_correct_oui wid = do {
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  (*
    let twid = rt_widget_named xd "initialisation term" in
  *)
  let m = get_int (get_field ip 0) in
  let a1 = get_int (get_field ip 1) in
  let a2 = get_int (get_field ip 2) in
  let e = {mois = m; annee = a1 * 100 + a2; lignes = []} in
  let b =
    {report = 0; noCheque = 0; postesDebit = []; postesCredit = [];
     cartes = []; comptes = []; virAuto = []; blocNotes = [];
     listeMois = [e]}
  in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "Init annule");
  rt_map_widget (rt_widget_named xd "entree");
  state.noPage := 0;
  state.keyPressAct := fun _ _ -> ();
  copier_budget b budget;
  mois.val := e;
  modif xd
}
and action_correct_non wid = do {
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  let twid = rt_widget_named xd "initialisation term" in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "Init annule");
  term_send twid "\027[?35l";
  goto_field ip 0 False;
  state.keyPressAct :=
    Action.input ip
      (fun _ -> rt_map_widget (rt_widget_named xd "Init NoErr")) verif_init
};

value action_annule wid = do {
  let xd = rt_xdata_of_widget wid in
  state.action_quit := fun _ -> state.quit := True;
  rt_map_widget (rt_widget_named xd "Init NoErr");
  rt_map_widget (rt_widget_named xd "entree");
  state.keyPressAct := fun _ _ -> ()
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd "initialisation term" in
  let ip = input_of_pic twid initialisation_pic in
  term_send twid "\027[H\027[2J\027[?35l";
  term_send twid (empty_pic initialisation_pic);
  set_field ip 1 (Fint 19);
  goto_field ip 0 False;
  state.action_quit := action_annule;
  rt_map_widget (rt_widget_named xd "initialisation");
  state.keyPressAct :=
    Action.input ip
      (fun _ -> rt_map_widget (rt_widget_named xd "Init NoErr")) verif_init;
  state.iP := Some ip
};

value wdesc =
  pack_desc [NameAtt "initialisation"]
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
                 term_desc [NameAtt "initialisation term"] (7, 23)
                   Action.no_term);
                (FIXSZ,
                 pack_desc []
                   (DIRx,
                    [(FIXSZ,
                      button_desc [NameAtt "Init annule"] ("Annule", None)
                        (Action.button action_annule));
                     Action.filler])
                   Action.no_pack)])
              Action.no_pack);
           Action.filler])
         Action.no_pack);
      Action.filler;
      (FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ,
            pack_desc []
              (DIRz,
               [(INCSZ,
                 pack_desc [NameAtt "Init NoErr"] (DIRx, []) Action.no_pack);
                (INCSZ,
                 term_desc [NameAtt "Init Err"] (1, 30) Action.no_term);
                (INCSZ,
                 pack_desc [NameAtt "Init Correct alert"; BandAtt 0]
                   (DIRx,
                    [(FIXSZ,
                      term_desc [NameAtt "Init Correct titre"] (1, 10)
                        Action.no_term);
                     (FIXSZ,
                      button_desc [] ("Oui", None)
                        (Action.button action_correct_oui));
                     (FIXSZ,
                      button_desc [] ("Non", None)
                        (Action.button action_correct_non));
                     Action.filler])
                   Action.no_pack)])
              Action.no_pack);
           Action.filler])
         Action.no_pack)])
    Action.no_pack
;
