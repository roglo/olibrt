(* $Id: budget.ml,v 1.8 2007/06/12 16:12:59 deraugla Exp $ *)

open State;
open File;
open RtN;

Rt.title_font.val := "-*-terminus-bold-o-*-32-*";

value no_del wid = do {Printf.printf "no del implemented\n"; flush stdout};

value budget_prog xd = do {
  prerr_string "Initialisation... ";
  flush Pervasives.stderr;
  state.action_quit := fun _ -> state.quit := True;
  let rw = rt_root_widget xd in
  let wid =
    rt_create_widget rw "Budget familial" "Budget" 0 0
      (Some (fun wid -> state.action_quit wid)) A_entree.wdesc
  in
  let _ =
    rt_create_widget rw "Calculatrice" "Calculatrice" 0 0 (Some no_del)
      D_calculat.wdesc
  in
  let _ =
    rt_create_widget rw "Bloc notes" "Bloc notes" 0 0 (Some no_del)
      D_blocnotes.wdesc
  in
  let _ = rt_create_popup_widget wid C_popup.fichier_wdesc in
  let _ = rt_create_popup_widget wid C_popup.traitement_wdesc in
  let _ = rt_create_popup_widget wid C_popup.budget_wdesc in
  let _ = rt_create_popup_widget wid C_popup.informations_wdesc in
  let _ = rt_create_popup_widget wid C_popup.mise_a_jour_wdesc in
  let _ =
    rt_create_transient_widget wid "Choisissez un type de sélection"
      (Some (fun _ -> B_pointage.action_quit xd)) B_pointage.question_wdesc
  in
  let _ =
    rt_create_transient_widget wid "AJOUT/MODIF" (Some D_ajout.action_X)
      D_ajout.wdesc
  in
  let _ =
    rt_create_transient_widget wid "AJOUT/MODIF" (Some no_del) D_modif.wdesc
  in
  let _ =
    rt_create_transient_widget wid "TRANSFERT" (Some no_del) D_transf.wdesc
  in
  let _ =
    rt_create_transient_widget wid "MOIS" (Some no_del) D_listeMois.wdesc
  in
  let _ =
    rt_create_transient_widget wid "Création d'un nouveau mois ?"
      (Some no_del) D_nouvMois.question_wdesc
  in
  let _ =
    rt_create_transient_widget wid "SOLDE RÉDUIT" (Some no_del)
      D_soldeRed.wdesc
  in
  let _ =
    rt_create_transient_widget wid "REPARTITION" (Some no_del)
      D_repartMois.wdesc
  in
  let _ =
    rt_create_transient_widget wid "POSTES" (Some no_del)
      D_repartPoste.wdesc_list
  in
  let _ =
    rt_create_transient_widget wid "POSTES" (Some no_del) D_infoPostes.wdesc
  in
  let _ =
    rt_create_transient_widget wid "LISTE DES CARTES" (Some no_del)
      D_infoCartes.wdesc
  in
  let _ =
    rt_create_transient_widget wid "COMPTES" (Some D_infoComptes.action_X)
      D_infoComptes.wdesc
  in
  let _ =
    rt_create_transient_widget wid "MISE À JOUR DES POSTES"
      (Some D_majPostes.action_X) D_majPostes.wdesc
  in
  let _ =
    rt_create_transient_widget wid "MISE À JOUR DES CARTES" (Some no_del)
      D_majCartes.wdesc
  in
  let _ =
    rt_create_transient_widget wid "M.À.J. COMPTES" (Some no_del)
      D_majComptes.wdesc
  in
  let _ =
    rt_create_transient_widget wid "AJOUT/MODIF" (Some no_del)
      D_ajoutAuto.wdesc
  in
  let _ =
    rt_create_transient_widget wid "AJOUT/MODIF" (Some no_del)
      D_modifAuto.wdesc
  in
  let _ =
    rt_create_transient_widget wid "Attention !" (Some no_del)
      D_nouvMois.efface_wdesc
  in
  let _ =
    rt_create_transient_widget wid "Virements automatiques" (Some no_del)
      D_nouvMois.auto_wdesc
  in
  let ind = List.length mois.val.lignes - 1 in
  let (nlin, _) = term_get_params (rt_widget_named xd "Libelle term") in
  prerr_string "Lecture données... ";
  flush Pervasives.stderr;
  charger_fichier ();
  prerr_string "ok\n";
  flush Pervasives.stderr;
  init_state ();
  state.nlin := nlin;
  state.noPage := ind / nlin;
  state.modifie := False;
  rt_freeze_widget (rt_widget_named xd "Sauver");
  Show.titres xd;
  rt_map_widget wid;
  while not state.quit do { rt_treat_one_event xd };
  if state.modifie then sauver_fichier () else ()
};

value go dname = do {
  let xd = rt_open dname in
  Rt.button_font.val := "mono:size=12";
  Rt.term_font.val := "mono:size=14";
Printf.printf "pix_of_mm = %d\n" (Rt.pix_of_mm (rt_xdata_of_xdata xd) 1.5);
flush stdout;
  let r = try budget_prog xd with x -> do { rt_close xd; raise x } in
  rt_close xd;
  r
};
