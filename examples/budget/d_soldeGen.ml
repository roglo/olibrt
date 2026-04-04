(* $Id: d_soldeGen.ml,v 1.4 2006/05/31 03:05:16 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value init_solde_general xd = do {
  let twid1 = rt_widget_named xd "SG CC term" in
  let twid2 = rt_widget_named xd "SG CS term" in
  let twid3 = rt_widget_named xd "SG SG term" in
  let s = calculer_soldes budget in
  let solde_fictif =
    List.fold_left (fun n (_, d) -> n + d) s.soldeReel s.enCours
  in
  term_send twid1 "\027[2J\027[H\027[?35h";
  term_send twid2 "\027[2J\027[H\027[?35h";
  term_send twid3 "\027[2J\027[H\027[?35h";
  let tm = Unix.localtime (Unix.time ()) in
  Show.texte_centre (rt_widget_named xd "SG titre")
    ("SOLDE GÉNÉRAL DU " ^
     string_of_date tm.Unix.tm_mday (succ tm.Unix.tm_mon)
       (1900 + tm.Unix.tm_year));
  term_send twid1 " Solde banque : ";
  term_send twid1 (string_of_somme 10 s.soldeBanque);
  term_send twid1 "\n";
  term_send twid1 " Solde fictif : ";
  term_send twid1 (string_of_somme 10 solde_fictif);
  term_send twid1 "\n";
  List.iter
    (fun (c, v) -> do {
       term_send twid1 " En-cours ";
       term_send twid1 c;
       for i = String.length c + 1 to 2 do { term_send twid1 " " };
       term_send twid1 "  : ";
       term_send twid1 (string_of_somme 10 v);
       term_send twid1 "\n"
     })
    s.enCours;
  for i = List.length s.enCours + 1 to Const.nb_comptes do {
    term_send twid1 "\n";
  };
  term_send twid1 " Solde réel   : ";
  term_send twid1 (string_of_somme 10 s.soldeReel);
  term_send twid1 "\n";
  let (comptes, tot) =
    glop budget.comptes s.soldeComptes where rec glop _xxx1 _xxx2 =
      match (_xxx1, _xxx2) with
      [ ([Some (lib, _) :: cs], [(_, n) :: sc]) ->
          let (comptes, tot) = glop cs sc in
          ([(lib, n) :: comptes], n + tot)
      | ([None :: cs], sc) -> glop cs sc
      | (_, _) -> ([], 0) ]
  in
  let comptes =
    List.sort
      (fun (lib1, _) (lib2, _) -> if lib1 < lib2 then -1 else 1)
      comptes
  in
  List.iter
    (fun (lib, n) -> do {
       term_send twid2 " ";
       term_send twid2 (string_of_somme 10 n);
       term_send twid2 " :   ";
       term_send twid2 lib;
       term_send twid2 "\n"
     })
    comptes;
  for i = List.length comptes to Const.nb_comptes do {
    term_send twid2 "\n";
  };
  term_send twid2 "------------\n ";
  term_send twid2 (string_of_somme 10 tot);
  term_send twid2 "  Total\n";
  term_send twid3 "Solde général : ";
  term_send twid3 (string_of_somme 10 (s.soldeReel + tot))
};

value action_quit wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Liste mois"; "Nouveau mois"; "Traitement"; "Budget"; "INFO Postes";
     "INFO Cartes"; "INFO Comptes spéciaux"; "Mise à jour"];
  rt_unmap_widget (rt_widget_named xd "SOLDE GENERAL")
};

value action wid = do {
  state.action := EcranSoldeGen;
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Liste mois"; "Nouveau mois"; "Traitement"; "Budget"; "INFO Postes";
     "INFO Cartes"; "INFO Comptes spéciaux"; "Mise à jour"];
  init_solde_general xd;
  rt_map_widget (rt_widget_named xd "SOLDE GENERAL")
};

value nlin = Const.nb_comptes + 4;

value wdesc =
  pack_desc [NameAtt "SOLDE GENERAL"]
    (DIRx,
     [Action.filler;
      (FIXSZ,
       pack_desc []
         (DIRy,
          [Action.filler;
           (FIXSZ, term_desc [NameAtt "SG titre"] (1, 1) Action.no_term);
           Action.filler;
           (FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(FIXSZ,
                 term_desc [NameAtt "SG CC titre"] (1, 27) Action.no_term);
                (FIXSZ,
                 term_desc [NameAtt "SG CS titre"]
                   (1, 1 + 10 + 5 + Const.taille_nom_compte + 1)
                   Action.no_term)])
              Action.no_pack);
           (FIXSZ,
            pack_desc [BandAtt 0]
              (DIRx,
               [(FIXSZ,
                 term_desc [NameAtt "SG CC term"] (nlin, 27) Action.no_term);
                (FIXSZ,
                 term_desc [NameAtt "SG CS term"]
                   (1, 1 + 10 + 5 + Const.taille_nom_compte + 1)
                   Action.no_term)])
              Action.no_pack);
           Action.filler;
           (FIXSZ,
            pack_desc []
              (DIRx,
               [Action.filler;
                (FIXSZ,
                 term_desc [NameAtt "SG SG term"] (1, 27) Action.no_term);
                Action.filler])
              Action.no_pack);
           Action.filler])
         Action.no_pack);
      Action.filler])
    Action.no_pack
;
