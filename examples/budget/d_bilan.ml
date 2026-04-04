(* $Id: d_bilan.ml,v 1.6 2006/05/31 19:36:24 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value init_bilan xd = do {
  let twid = rt_widget_named xd "BILAN term" in
  let twid1 = rt_widget_named xd "BI Depenses term" in
  let twid2 = rt_widget_named xd "BI Recettes term" in
  let annee = (List.hd budget.listeMois).annee in
  let str = large ("BILAN POUR " ^ string_of_int annee) in
  let (listemois, report) =
    List.fold_right
      (fun m (l, r) ->
         if m.annee == annee then ([m :: l], r)
         else
           let r =
             List.fold_left
               (fun r l ->
                  match l.lmontant with
                  [ Left x -> r - x
                  | Right x -> r + x ])
               r m.lignes
           in
           (l, r))
      budget.listeMois ([], budget.report)
  in
  let (report_deb, report_cre) =
    if report < 0 then (- report, 0) else (0, report)
  in
  let rep = calculer_repart listemois in
  Show.texte_centre twid str;
  term_send twid1 "\027[2J\027[H\027[?35h";
  term_send twid2 "\027[2J\027[H\027[?35h";
  if report_deb > 0 then do {
    term_send twid1 "     Report     : ";
    term_send twid1 (string_of_somme 10 report_deb)
  }
  else ();
  if report_cre >= 0 then do {
    term_send twid2 (String.make 9 ' ');
    term_send twid2 (string_of_somme 10 report_cre);
    term_send twid2 " : Report"
  }
  else ();
  term_send twid1 "\n";
  term_send twid2 "\n";
  List.iter
    (fun (_, (lib, r)) -> do {
       term_send twid1 "     ";
       term_send twid1 lib;
       for i = String.length lib + 1 to 10 do { term_send twid1 " " };
       term_send twid1 " : ";
       term_send twid1 (string_of_somme 10 r);
       term_send twid1 " ";
       term_send twid1 (string_of_somme 6 (pourcent r rep.totalDep));
       term_send twid1 "%\n"
     })
    rep.postesDeb;
  List.iter
    (fun (_, (lib, r)) -> do {
       term_send twid2 " ";
       term_send twid2 (string_of_somme 6 (pourcent r rep.totalRec));
       term_send twid2 "% ";
       term_send twid2 (string_of_somme 10 r);
       term_send twid2 " : ";
       term_send twid2 lib;
       term_send twid2 "\n"
     })
    rep.postesCre;
  for i = List.length rep.postesDeb + 1 to 10 do { term_send twid1 "\n" };
  for i = List.length rep.postesCre + 1 to 10 do { term_send twid2 "\n" };
  term_send twid1 "     Placement  : ";
  term_send twid1 (string_of_somme 10 rep.placement);
  term_send twid1 " ";
  term_send twid1 (string_of_somme 6 (pourcent rep.placement rep.totalDep));
  term_send twid1 "%\n";
  term_send twid2 " ";
  term_send twid2 (string_of_somme 6 (pourcent rep.retrait rep.totalRec));
  term_send twid2 "% ";
  term_send twid2 (string_of_somme 10 rep.retrait);
  term_send twid2 " : Retrait\n";
  term_send twid1 "     Non imputé : ";
  term_send twid1 (string_of_somme 10 rep.nonImpDeb);
  term_send twid1 "\n";
  term_send twid2 "         ";
  term_send twid2 (string_of_somme 10 rep.nonImpCre);
  term_send twid2 " : Non imputé\n";
  let total_dep = report_deb + rep.totalDep + rep.nonImpDeb in
  let total_rec = report_cre + rep.totalRec + rep.nonImpCre in
  if total_dep <= total_rec then do {
    term_send twid1 "Solde créditeur : ";
    term_send twid1 (string_of_somme 10 (total_rec - total_dep))
  }
  else ();
  term_send twid1 "\n";
  term_send twid1 (String.make 17 ' ');
  term_send twid1 (String.make 12 '=');
  term_send twid1 "\n";
  term_send twid1 "          Total : ";
  term_send twid1 (string_of_somme 10 (max total_dep total_rec));
  if total_rec < total_dep then do {
    term_send twid2 "         ";
    term_send twid2 (string_of_somme 10 (total_dep - total_rec));
    term_send twid2 " : Solde débiteur"
  }
  else ();
  term_send twid2 "\n";
  term_send twid2 (String.make 8 ' ');
  term_send twid2 (String.make 12 '=');
  term_send twid2 "\n";
  term_send twid2 (String.make 9 ' ');
  term_send twid2 (string_of_somme 10 (max total_dep total_rec));
  term_send twid2 " : Total"
};

value action_quit wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Liste mois"; "Nouveau mois"; "Traitement"; "Budget"; "INFO Postes";
     "INFO Cartes"; "INFO Comptes spéciaux"; "Mise à jour"];
  rt_unmap_widget (rt_widget_named xd "BILAN")
};

value action wid = do {
  state.action := EcranBilan;
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Liste mois"; "Nouveau mois"; "Traitement"; "Budget"; "INFO Postes";
     "INFO Cartes"; "INFO Comptes spéciaux"; "Mise à jour"];
  init_bilan xd;
  rt_map_widget (rt_widget_named xd "BILAN")
};

value wdesc =
  pack_desc [NameAtt "BILAN"]
    (DIRx,
     [Action.filler;
      (FIXSZ,
       pack_desc []
         (DIRy,
          [(FIXSZ,
            pack_desc []
              (DIRx,
               [Action.filler;
                (FIXSZ,
                 term_desc [NameAtt "BILAN term"] (1, 35) Action.no_term);
                Action.filler])
              Action.no_pack);
           Action.filler;
           (FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(FIXSZ,
                 term_desc [NameAtt "BI Depenses titre"] (1, 37)
                   Action.no_term);
                (FIXSZ,
                 term_desc [NameAtt "BI Recettes titre"] (1, 37)
                   Action.no_term)])
              Action.no_pack);
           (FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(FIXSZ,
                 term_desc [NameAtt "BI Depenses term"] (16, 37)
                   Action.no_term);
                (FIXSZ,
                 term_desc [NameAtt "BI Recettes term"] (16, 37)
                   Action.no_term)])
              Action.no_pack);
           Action.filler])
         Action.no_pack);
      Action.filler])
    Action.no_pack
;
