(* $Id: d_nouvMois.ml,v 1.5 2006/05/31 03:05:16 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value auto_oui wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd;
  D_auto.action wid
};

value auto_non wid =
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd
;

value creation xd = do {
  let m = (List.hd budget.listeMois).mois in
  let a = (List.hd budget.listeMois).annee in
  let (m, a) = if m == 12 then (1, succ a) else (succ m, a) in
  let e = {mois = m; annee = a; lignes = []} in
  rt_unmap_alert xd;
  budget.listeMois := [e :: budget.listeMois];
  mois.val := e;
  state.noPage := 0;
  state.tdebit := 0;
  state.tcredit := 0;
  Show.mois xd;
  Show.lignes xd state.noPage;
  Show.selection xd state.noPage;
  Show.total xd;
  Show.repart_mois xd;
  modif xd;
  match budget.virAuto with
  [ [] -> ()
  | _ -> do {
      let wid = rt_widget_named xd "NOUV MOIS AUTO" in
      Action.place_transient wid;
      rt_map_alert wid
    } ]
};

value detruire_ancien_mois () = do {
  let eff = List.hd (List.rev budget.listeMois) in
  let bb =
    {report = budget.report; noCheque = 0; postesDebit = [];
     postesCredit = []; cartes = []; comptes = budget.comptes; virAuto = [];
     blocNotes = []; listeMois = [eff]}
  in
  let s = calculer_soldes bb in
  budget.listeMois := List.rev (List.tl (List.rev budget.listeMois));
  budget.report := s.soldeReel;
  budget.comptes :=
    glop budget.comptes s.soldeComptes where rec glop _xxx1 _xxx2 =
      match (_xxx1, _xxx2) with
      [ ([Some (lib, _) :: cs], [(_, n) :: sc]) ->
          [Some (lib, n) :: glop cs sc]
      | ([None :: cs], sc) -> [None :: glop cs sc]
      | ([], []) -> []
      | (_, _) -> failwith "erreur interne dans detruire ancien mois" ]
};

value impr_annule wid =
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd
;

value impr_non wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd;
  detruire_ancien_mois ();
  creation xd
};

value impr_oui wid = do {
  mois.val := List.hd (List.rev budget.listeMois);
  D_imprimer.action wid;
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd;
  detruire_ancien_mois ();
  creation xd
};

value message_efface xd = do {
  let wid = rt_widget_named xd "MOIS EFFACE" in
  let twid = rt_widget_named xd "MOIS EFFACE term" in
  let eff = List.hd (List.rev budget.listeMois) in
  term_send twid "\027[H\027[2J\027[?35h";
  term_send twid "Attention !\n\nLe mois de ";
  term_send twid (capitalize (string_of_mois eff.mois));
  term_send twid " ";
  term_send twid (string_of_int eff.annee);
  term_send twid "\nva être effacé";
  Action.place_transient wid;
  rt_map_alert wid
};

value action_oui wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd;
  if List.length budget.listeMois == 24 then message_efface xd
  else creation xd
};

value action_non wid =
  let xd = rt_xdata_of_widget wid in
  rt_unmap_alert xd
;

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let wid = rt_widget_named xd "Ajouter mois question" in
  Action.place_transient wid;
  rt_map_alert wid
};

value question_wdesc =
  pack_desc [NameAtt "Ajouter mois question"]
    (DIRy,
     [(FIXSZ,
       term_desc [NameAtt "Ajouter mois question titre"] (1, 28)
         Action.no_term);
      (FIXSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ, button_desc [] ("Oui", None) (Action.button action_oui));
           Action.filler;
           (FIXSZ, button_desc [] ("Non", None) (Action.button action_non));
           Action.filler])
         Action.no_pack)])
    Action.no_pack
;

value efface_wdesc =
  pack_desc [NameAtt "MOIS EFFACE"]
    (DIRy,
     [(FIXSZ, term_desc [NameAtt "MOIS EFFACE term"] (5, 25) Action.no_term);
      (FIXSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ,
            term_desc [NameAtt "IMPR EFFACE titre"] (1, 12) Action.no_term);
           (FIXSZ, button_desc [] ("Oui", None) (Action.button impr_oui));
           (FIXSZ, button_desc [] ("Non", None) (Action.button impr_non));
           Action.filler])
         Action.no_pack);
      (FIXSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ,
            button_desc [] ("Annule", None) (Action.button impr_annule));
           Action.filler])
         Action.no_pack)])
    Action.no_pack
;

value auto_wdesc =
  pack_desc [NameAtt "NOUV MOIS AUTO"]
    (DIRy,
     [(FIXSZ, term_desc [NameAtt "NM Auto titre"] (1, 40) Action.no_term);
      (INCSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ,
            term_desc [NameAtt "NM Selection titre"] (1, 12) Action.no_term);
           (FIXSZ, button_desc [] ("Oui", None) (Action.button auto_oui));
           (FIXSZ, button_desc [] ("Non", None) (Action.button auto_non));
           Action.filler])
         Action.no_pack)])
    Action.no_pack
;
