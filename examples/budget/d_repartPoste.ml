(* $Id: d_repartPoste.ml,v 1.6 2006/05/31 17:06:57 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value init_repart_poste xd =
  let twid = rt_widget_named xd "REPART POSTE term" in
  if state.action <> EcranRepartPoste then
    term_send twid "\027[2J\027[H\027[?35h"
  else ()
;

value select_poste xd (p, lib) = do {
  let twid = rt_widget_named xd "REPART POSTE term" in
  let tab =
    List.map
      (fun m ->
         let rep = calculer_repart [m] in
         let ((_, v), t) =
           try (List.assoc p rep.postesDeb, rep.totalDep) with
           [ Not_found -> (List.assoc p rep.postesCre, rep.totalRec) ]
         in
         (m.mois, m.annee, v, t))
      budget.listeMois
  in
  let moy =
    List.fold_left (fun s (_, _, v, t) -> s + v) 0 tab /
    List.length budget.listeMois
  in
  rt_unmap_widget (rt_widget_named xd "RP List");
  Show.texte_centre twid ("- " ^ lib ^ " -");
  term_send twid
    "

    Mois        Montant        %
-----------------------------------
";
  List.iter
    (fun (mois, annee, val, tot) -> do {
       let sm = capitalize (string_of_mois mois) in
       term_send twid sm;
       for i = String.length sm + 1 to 10 do { term_send twid " " };
       term_send twid (Printf.sprintf "%02d" (annee mod 100));
       term_send twid "   ";
       term_send twid (string_of_somme 10 val);
       term_send twid "   ";
       term_send twid (string_of_somme 6 (pourcent val tot));
       term_send twid "%\n"
     })
    tab;
  term_send twid "              ------------\n";
  term_send twid "      Moyenne  ";
  term_send twid (string_of_somme 10 moy)
};

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_unmap_widget (rt_widget_named xd "RP List");
  state.buttonAct := fun _ _ -> ()
};

value action_quit wid = do {
  let xd = rt_xdata_of_widget wid in
  state.action := Rien;
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Liste mois"; "Nouveau mois"; "Traitement"; "Solde réduit";
     "Répartition du mois"; "Solde général"; "Bilan de l'année";
     "INFO Postes"; "INFO Cartes"; "INFO Comptes spéciaux"; "Mise à jour"];
  rt_unmap_widget (rt_widget_named xd "REPART POSTE");
  rt_unmap_widget (rt_widget_named xd "RP List");
  state.buttonAct := fun _ _ -> ()
};

value action wid =
  let xd = rt_xdata_of_widget wid in
  match
    glop (budget.postesDebit @ budget.postesCredit) where rec glop =
      fun
      [ [Some p :: pl] -> [p :: glop pl]
      | [None :: pl] -> glop pl
      | [] -> [] ]
  with
  [ [] -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err "Pas de postes";
      rt_map_widget err
    }
  | plist -> do {
      let mwid = rt_widget_named xd "Main" in
      let wid = rt_widget_named xd "RP List" in
      let twid = rt_widget_named xd "RP List term" in
      let (nlin, ncol) = term_get_params twid in
      let (width, height) =
        if nlin != List.length plist then do {
          term_set_params twid (List.length plist, ncol);
          rt_adjust_widget wid
        }
        else (rt_widget_width wid, rt_widget_height wid)
      in
      let y = (rt_widget_height mwid - height) / 2 in
      List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
        ["Liste mois"; "Nouveau mois"; "Traitement"; "Solde réduit";
         "Répartition du mois"; "Solde général"; "Bilan de l'année";
         "INFO Postes"; "INFO Cartes"; "INFO Comptes spéciaux";
         "Mise à jour"];
      term_send twid "\027[2J\027[H\027[?35h";
      List.iter
	(fun (nom, lib) -> do { term_send twid lib; term_send twid "\n" })
        plist;
      state.buttonAct :=
        Action.button_select xd ["RP List term"] plist (fun _ -> 0)
          (select_poste xd);
      init_repart_poste xd;
      rt_map_widget (rt_widget_named xd "REPART POSTE");
      rt_move_widget wid (rt_widget_x mwid) (rt_widget_y mwid + y);
      rt_map_widget wid;
      state.action := EcranRepartPoste
       } ]
;

value wdesc_list =
  pack_desc [NameAtt "RP List"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "RP List titre"] (1, 6) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ, term_desc [NameAtt "RP List term"] (0, 10) Action.butt_term)])
    Action.no_pack
;

value wdesc =
  pack_desc [NameAtt "REPART POSTE"]
    (DIRy,
     [Action.filler;
      (FIXSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ,
            term_desc [NameAtt "REPART POSTE term"] (30, 35) Action.no_term);
           Action.filler])
         Action.no_pack);
      Action.filler])
    Action.no_pack
;
