(* $Id: d_imprimer.ml,v 1.4 2006/05/31 19:36:24 deraugla Exp $ *)

open State;
open File;
open Jmage;
open Printf;

value rec nth _xxx1 _xxx2 =
  match (_xxx1, _xxx2) with
  [ (_, []) -> invalid_arg "nth"
  | (0, [x :: _]) -> x
  | (n, [_ :: l]) -> nth (pred n) l ]
;

value imprimer_mois () = do {
  let m = mois.val in
  let titre = mois_annee_large m.mois m.annee in
  let ligne =
    "---------------------------------------------------------------------"
  in
  printf "\012";
  printf "%s\n" ligne;
  printf "|";
  for i = 2 to (String.length ligne - String.length titre) / 2 do {
    printf " ";
  };
  printf "%s" titre;
  for i = 2 to (String.length ligne + 1 - String.length titre) / 2 do {
    printf " ";
  };
  printf "|\n";
  printf
    "\
%s
|R|  Date  |  Nature  |P       Libellé        |   Débit  |  Crédit  |
%s
"
    ligne ligne;
  List.iter
    (fun l -> do {
       printf "|%s" (if l.lretire then "X" else " ");
       printf "|%02d/%02d/%02d" l.ljour m.mois (m.annee mod 100);
       printf "|%-10s"
         (match l.lnature with
          [ Some s -> s
          | _ -> "" ]);
       printf "|%-2s"
         (match l.lposte with
          [ Some s -> s
          | _ -> "" ]);
       printf " %-20s"
         (match l.llibelle with
          [ Some s -> s
          | _ -> "" ]);
       printf "|%-10s"
         (match l.lmontant with
          [ Left x -> string_of_somme 10 x
          | Right _ -> "" ]);
       printf "|%-10s"
         (match l.lmontant with
          [ Left _ -> ""
          | Right x -> string_of_somme 10 x ]);
       printf "|\n"
     })
    m.lignes;
  printf "%s\n" ligne;
  printf "%46s|%-10s|%-10s|\n" "" (string_of_somme 10 state.tdebit)
    (string_of_somme 10 state.tcredit);
  printf "%46s-----------------------\n" "";
  flush stdout
};

value imprimer_n_fois n c = for i = 1 to n do { print_char c };

value imprimer_solde_general () = do {
  let s = calculer_soldes budget in
  let solde_fictif =
    List.fold_left (fun n (_, d) -> n + d) s.soldeReel s.enCours
  in
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
    List.sort (fun (lib1, _) (lib2, _) -> compare lib1 lib2) comptes
  in
  let long_ligne = 47 + Const.taille_nom_compte in
  printf "\012";
  imprimer_n_fois long_ligne '-';
  printf "\n";
  let txt =
    let tm = Unix.localtime (Unix.time ()) in
    "SOLDE GÉNÉRAL DU " ^
    string_of_date tm.Unix.tm_mday (succ tm.Unix.tm_mon)
      (1900 + tm.Unix.tm_year)
  in
  printf "|";
  imprimer_n_fois ((long_ligne - 2 - String.length txt + 1) / 2) ' ';
  printf "%s" txt;
  imprimer_n_fois ((long_ligne - 2 - String.length txt) / 2) ' ';
  printf "|\n";
  imprimer_n_fois long_ligne '-';
  printf "\n";
  printf "|%7sCOMPTE COURANT%6s|" "" "";
  let txt = "COMPTES SPECIAUX" in
  imprimer_n_fois ((long_ligne - 30 - String.length txt + 1) / 2) ' ';
  printf "%s" txt;
  imprimer_n_fois ((long_ligne - 30 - String.length txt) / 2) ' ';
  printf "|\n";
  imprimer_n_fois long_ligne '-';
  printf "\n";
  for i = 1 to max (2 + List.length s.enCours) (3 + List.length comptes) do {
    if i == 1 then
      printf "| Solde banque : %-10s |" (string_of_somme 10 s.soldeBanque)
    else if i == 2 then
      printf "| Solde fictif : %-10s |" (string_of_somme 10 solde_fictif)
    else if i - 3 < List.length s.enCours then
      let (n, x) = nth (i - 3) s.enCours in
      printf "| En-cours %2s  : %-10s |" n (string_of_somme 10 x)
    else printf "|%27s|" "";
    if i - 1 < List.length comptes then do {
      let (lib, n) = nth (i - 1) comptes in
      printf " %-10s :   " (string_of_somme 10 n);
      let nsp = max 0 (Const.taille_nom_compte - String.length lib) in
      printf "%s%s" lib (String.make nsp ' ');
      printf " |"
    }
    else if i - 1 == List.length comptes + 1 then
      printf "------------%20s|" ""
    else if i - 1 == List.length comptes + 2 then
      printf " %-10s  Total%14s|" (string_of_somme 10 tot) ""
    else printf "%32s|" "";
    printf "\n";
  };
  for i = 3 to List.length s.enCours do { printf "|%27s|%32s|\n" "" "" };
  printf "| Solde réel   : %-10s |%32s|\n" (string_of_somme 10 s.soldeReel)
    "";
  imprimer_n_fois long_ligne '-';
  printf "\n";
  printf "|%13sSolde général : %-10s%21s|\n" ""
    (string_of_somme 10 (s.soldeReel + tot)) "";
  imprimer_n_fois long_ligne '-';
  printf "\n";
  flush stdout
};

value imprimer_bilan () = do {
  let annee = (List.hd budget.listeMois).annee in
  let titre = large ("BILAN POUR " ^ string_of_int annee) in
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
  let ligne =
    "\
-----------------------------------------------------------------------------"
  in
  printf "\012";
  printf "%s\n" ligne;
  printf "|";
  for i = 2 to (String.length ligne - String.length titre) / 2 do {
    printf " ";
  };
  printf "%s" titre;
  for i = 2 to (String.length ligne + 1 - String.length titre) / 2 do {
    printf " ";
  };
  printf "|\n";
  printf "%s\n" ligne;
  printf "|%11sD E P E N S E S%11s|%11sR E C E T T E S%11s|\n" "" "" "" "";
  printf "%s\n" ligne;
  printf "|";
  if report_deb > 0 then do {
    printf "     Report     : ";
    printf "%s" (string_of_somme 10 report_deb)
  }
  else printf "%28s" "";
  printf "%9s|%1s" "" "";
  if report_cre >= 0 then do {
    printf "%8s" "";
    printf "%s" (string_of_somme 10 report_cre);
    printf " : Report"
  }
  else printf "%27s" "";
  printf "%9s|\n" "";
  for i = 0 to
    max (List.length rep.postesDeb) (List.length rep.postesCre) - 1
  do {
    if i < List.length rep.postesDeb then do {
      let (_, (lib, r)) = nth i rep.postesDeb in
      printf "|     ";
      printf "%s" lib;
      for i = String.length lib + 1 to 10 do { printf " " };
      printf " : ";
      printf "%s" (string_of_somme 10 r);
      printf " ";
      printf "%s" (string_of_somme 6 (pourcent r rep.totalDep));
      printf "%%"
      }
    else printf "%34s" "";
    printf "%1s|%1s" "" "";
    if i < List.length rep.postesCre then do {
      let (_, (lib, r)) = nth i rep.postesCre in
      printf "%s" (string_of_somme 6 (pourcent r rep.totalRec));
      printf "%% ";
      printf "%s" (string_of_somme 10 r);
      printf " : ";
      printf "%s" lib;
      for i = String.length lib + 1 to 10 do { printf " " };
      printf "%4s" ""
      }
    else printf "%35s" "";
    printf " |\n";
  };
  printf "|     Placement  : ";
  printf "%s" (string_of_somme 10 rep.placement);
  printf " ";
  printf "%s" (string_of_somme 6 (pourcent rep.placement rep.totalDep));
  printf "%%";
  printf " | ";
  printf "%s" (string_of_somme 6 (pourcent rep.retrait rep.totalRec));
  printf "%% ";
  printf "%s" (string_of_somme 10 rep.retrait);
  printf " : Retrait%8s|\n" "";
  printf "|     Non imputé : ";
  printf "%s" (string_of_somme 10 rep.nonImpDeb);
  printf "%9s|%9s" "" "";
  printf "%s" (string_of_somme 10 rep.nonImpCre);
  printf " : Non imputé%5s|\n" "";
  let total_dep = report_deb + rep.totalDep + rep.nonImpDeb in
  let total_rec = report_cre + rep.totalRec + rep.nonImpCre in
  printf "|";
  if total_dep <= total_rec then do {
    printf "Solde créditeur : ";
    printf "%s" (string_of_somme 10 (total_rec - total_dep))
      }
  else printf "%28s" "";
  printf "%9s|%1s" "" "";
  if total_rec < total_dep then do {
    printf "%8s" "";
    printf "%s" (string_of_somme 10 (total_dep - total_rec));
    printf " : Solde débiteur"
  }
  else printf "%35s" "";
  printf " |\n";
  printf "|%17s" "";
  printf "============";
  printf "%7s | " "";
  printf "%7s" "";
  printf "============";
  printf "%17s|\n" "";
  printf "|%10s" "";
  printf "Total : ";
  printf "%s" (string_of_somme 10 (max total_dep total_rec));
  printf "%9s| " "";
  printf "%8s" "";
  printf "%s" (string_of_somme 10 (max total_dep total_rec));
  printf " : Total";
  printf "%10s|\n" "";
  printf "%s\n" ligne;
  flush stdout
};

value action wid =
  if state.majAuto then ()
  else
    match state.action with
    [ EcranRepartPoste -> ()
    | EcranSoldeGen -> imprimer_solde_general ()
    | EcranBilan -> imprimer_bilan ()
    | _ -> imprimer_mois () ]
;
