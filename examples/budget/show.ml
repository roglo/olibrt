(* $Id: show.ml,v 1.1 2006/05/31 03:05:16 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value texte_centre wid txt = do {
  let (_, n) = term_get_params wid in
  term_send wid "\027[2J\027[H\027[?35h\027[?7l";
  for i = 1 to (n - String.length txt) / 2 do { term_send wid " " };
  term_send wid txt
};

value total xd = do {
  let tdebit_wid = rt_widget_named xd "Tdebit term" in
  let tcredit_wid = rt_widget_named xd "Tcredit term" in
  let str10 = String.make 10 ' ' in
  List.iter (fun w -> term_send w "\027[H\027[2J\027[?35h\027[?7l")
    [tdebit_wid; tcredit_wid];
  format_field "%10.2f" 'f' 2
    (Fdecimal (state.tdebit / 100, state.tdebit mod 100, 2)) str10;
  term_send tdebit_wid str10;
  format_field "%10.2f" 'f' 2
    (Fdecimal (state.tcredit / 100, state.tcredit mod 100, 2)) str10;
  term_send tcredit_wid str10
};

value total_pt xd = do {
  let tdebit_wid = rt_widget_named xd "PT Tdebit term" in
  let tcredit_wid = rt_widget_named xd "PT Tcredit term" in
  List.iter (fun w -> term_send w "\027[H\027[2J\027[?35h\027[?7l")
    [tdebit_wid; tcredit_wid];
  term_send tdebit_wid (string_of_somme 10 state.tdebit);
  term_send tcredit_wid (string_of_somme 10 state.tcredit)
};

value solde_pt xd = do {
  let twid = rt_widget_named xd "PT solde term" in
  term_send twid "\027[H\027[2J\027[?35h\027[?7l";
  term_send twid "Solde banque   : ";
  term_send twid (string_of_somme 10 state.ptSoldeBanque);
  term_send twid " Eu\n";
  term_send twid "En-cours carte : ";
  term_send twid (string_of_somme 10 state.ptEnCoursCartes);
  term_send twid " Eu\n"
};

value texte_titre xd wn n = texte_centre (rt_widget_named xd wn) n;

value titres xd = do {
  texte_titre xd "Selection titre" "S";
  texte_titre xd "Retire titre" "R";
  texte_titre xd "Date titre" "Date";
  texte_titre xd "Nature titre" "Nature";
  texte_titre xd "Poste titre" "P";
  texte_titre xd "Libelle titre" "Libellé";
  texte_titre xd "Debit titre" "Débit";
  texte_titre xd "Credit titre" "Crédit";
  texte_titre xd "Init Correct titre" "Correct : ";
  texte_titre xd "Correct titre" "Correct : ";
  texte_titre xd "PT Question titre" "Choisissez un type de sélection";
  texte_titre xd "Modifier titre" "Modifier : ";
  texte_titre xd "Supprimer titre" "Supprimer : ";
  texte_titre xd "AJOUT titre" "AJOUT/MODIF";
  texte_titre xd "MODIF titre" "AJOUT/MODIF";
  texte_titre xd "TRANSF titre" "TRANSFERER";
  texte_titre xd "Liste mois titre" "MOIS";
  texte_titre xd "Ajouter mois question titre"
    "Création d'un nouveau mois ?";
  texte_titre xd "SOLDE REDUIT titre" "SOLDE RÉDUIT";
  texte_titre xd "SG CC titre" "COMPTE COURANT";
  texte_titre xd "SG CS titre" "COMPTES SPECIAUX";
  texte_titre xd "REPART MOIS titre" "REPARTITION";
  texte_titre xd "RP List titre" "POSTES";
  texte_titre xd "INFO POSTES titre" "POSTES";
  texte_titre xd "INFO CARTES titre" "LISTE DES CARTES";
  texte_titre xd "INFO COMPTES titre" "COMPTES";
  texte_titre xd "MAJ POSTES titre" "MISE À JOUR DES POSTES";
  texte_titre xd "MAJP debit titre" "Débit";
  texte_titre xd "MAJP credit titre" "Crédit";
  texte_titre xd "MAJ CARTES titre" "MISE À JOUR DES CARTES";
  texte_titre xd "MAJ COMPTES titre" "M.À.J. COMPTES";
  texte_titre xd "IMPR EFFACE titre" "Impression";
  texte_titre xd "NM Auto titre" "Vous avez des virements automatiques";
  texte_titre xd "NM Selection titre" "Selection : ";
  texte_titre xd "PT titre" "- POINTAGE TOUS MOIS CONFONDUS -";
  texte_titre xd "PT Selection titre" "S";
  texte_titre xd "PT Retire titre" "R";
  texte_titre xd "PT Date titre" "Date";
  texte_titre xd "PT Nature titre" "Nature";
  texte_titre xd "PT Poste titre" "P";
  texte_titre xd "PT Libelle titre" "Libellé";
  texte_titre xd "PT Debit titre" "Débit";
  texte_titre xd "PT Credit titre" "Crédit";
  texte_titre xd "BI Depenses titre" "D E P E N S E S";
  texte_titre xd "BI Recettes titre" "R E C E T T E S";
  texte_titre xd "MAJ AUTO titre" "MISE À JOUR DES VIREMENTS AUTOMATIQUES";
  texte_titre xd "MA Selection titre" "S";
  texte_titre xd "MA Nombre titre" "Nbr";
  texte_titre xd "MA Dernier titre" "Der";
  texte_titre xd "MA Jour titre" "Jr";
  texte_titre xd "MA Poste titre" "P";
  texte_titre xd "MA Libelle titre" "Libellé";
  texte_titre xd "MA Debit titre" "Débit";
  texte_titre xd "MA Credit titre" "Crédit";
  texte_titre xd "MA AJOUT titre" "AJOUT/MODIF";
  texte_titre xd "MA MODIF titre" "AJOUT/MODIF";
  texte_titre xd "MA Correct titre" "Correct : ";
  texte_titre xd "MA Supprimer titre" "Supprimer : ";
  texte_titre xd "MA Modifier titre" "Modifier : ";
  texte_titre xd "VIR AUTO titre" "SÉLECTION DES VIREMENTS AUTOMATIQUES";
  texte_titre xd "VA Selection titre" "S";
  texte_titre xd "VA Nombre titre" "Nbr";
  texte_titre xd "VA Dernier titre" "Der";
  texte_titre xd "VA Jour titre" "Jr";
  texte_titre xd "VA Poste titre" "P";
  texte_titre xd "VA Libelle titre" "Libellé";
  texte_titre xd "VA Debit titre" "Débit";
  texte_titre xd "VA Credit titre" "Crédit";
  texte_titre xd "VA Vir Auto titre" "Virements automatiques : "
};

value mois xd =
  let txt = mois_annee_large mois.val.mois mois.val.annee in
  texte_centre (rt_widget_named xd "Mois") txt
;

value selection_gen xd page ecr lignes fl = do {
  let selection_wid = rt_widget_named xd (ecr ^ "Selection term") in
  let (nlin, _) = term_get_params selection_wid in
  term_send selection_wid "\027[H\027[2J\027[?35h\027[?7l";
  let _ =
    List.fold_left
      (fun n ligne -> do {
         let l = fl ligne in
         if n >= page * nlin && n < (page + 1) * nlin then do {
           if l.lselection then term_send selection_wid "*" else ();
           term_send selection_wid "\n"
         }
         else ();
         succ n
       })
      0 lignes
  in
  ()
};

value lignes_gen xd page ecr lignes fl fm fa = do {
  let retire_wid = rt_widget_named xd (ecr ^ "Retire term") in
  let date_wid = rt_widget_named xd (ecr ^ "Date term") in
  let nature_wid = rt_widget_named xd (ecr ^ "Nature term") in
  let poste_wid = rt_widget_named xd (ecr ^ "Poste term") in
  let libelle_wid = rt_widget_named xd (ecr ^ "Libelle term") in
  let debit_wid = rt_widget_named xd (ecr ^ "Debit term") in
  let credit_wid = rt_widget_named xd (ecr ^ "Credit term") in
  let str2 = ".." in
  let (nlin, _) = term_get_params date_wid in
  List.iter (fun w -> term_send w "\027[H\027[2J\027[?35h\027[?7l")
    [retire_wid; date_wid; nature_wid; poste_wid; libelle_wid; debit_wid;
     credit_wid];
  let _ =
    List.fold_left
      (fun n ligne ->
         let l = fl ligne in
         if n >= page * nlin && n < (page + 1) * nlin then do {
           match l.lretire with
           [ True -> term_send retire_wid "X"
           | _ -> () ];
           format_field "%02d" 'd' 0 (Fint l.ljour) str2;
           term_send date_wid str2;
           term_send date_wid "/";
           format_field "%02d" 'd' 0 (Fint (fm ligne)) str2;
           term_send date_wid str2;
           term_send date_wid "/";
           format_field "%02d" 'd' 0 (Fint (fa ligne mod 100)) str2;
           term_send date_wid str2;
           match l.lnature with
           [ Some s -> term_send nature_wid s
           | _ -> () ];
           match l.lposte with
           [ Some s -> term_send poste_wid s
           | _ -> () ];
           match l.llibelle with
           [ Some s -> term_send libelle_wid s
           | _ -> () ];
           match l.lmontant with
           [ Left x -> term_send debit_wid (string_of_somme 10 x)
           | Right x -> term_send credit_wid (string_of_somme 10 x) ];
           term_send retire_wid "\n";
           term_send date_wid "\n";
           term_send nature_wid "\n";
           term_send poste_wid "\n";
           term_send libelle_wid "\n";
           term_send debit_wid "\n";
           term_send credit_wid "\n";
           succ n
         }
         else succ n)
      0 lignes
  in
  texte_centre (rt_widget_named xd (ecr ^ "Page"))
    ("Page " ^ string_of_int (page + 1))
};

value selection xd page =
  selection_gen xd page "" File.mois.val.lignes (fun ligne -> ligne)
;

value selection_pt xd page =
  selection_gen xd page "PT " state.ptSelection
    (fun (ligne, _, _) -> ligne.val)
;

value lignes xd page =
  lignes_gen xd page "" File.mois.val.lignes (fun ligne -> ligne)
    (fun _ -> File.mois.val.mois) (fun _ -> File.mois.val.annee)
;

value lignes_pt xd page =
  lignes_gen xd page "PT " state.ptSelection (fun (ligne, _, _) -> ligne.val)
    (fun (_, mois, _) -> mois) (fun (_, _, annee) -> annee)
;

value lignes_auto_gen xd ecr = do {
  let page = 0 in
  let nombre_wid = rt_widget_named xd (ecr ^ "Nombre term") in
  let dernier_wid = rt_widget_named xd (ecr ^ "Dernier term") in
  let jour_wid = rt_widget_named xd (ecr ^ "Jour term") in
  let poste_wid = rt_widget_named xd (ecr ^ "Poste term") in
  let libelle_wid = rt_widget_named xd (ecr ^ "Libelle term") in
  let debit_wid = rt_widget_named xd (ecr ^ "Debit term") in
  let credit_wid = rt_widget_named xd (ecr ^ "Credit term") in
  let (nlin, _) = term_get_params dernier_wid in
  List.iter (fun w -> term_send w "\027[H\027[2J\027[?35h\027[?7l")
    [nombre_wid; dernier_wid; jour_wid; poste_wid; libelle_wid; debit_wid;
     credit_wid];
  let _ =
    List.fold_left
      (fun n ligne ->
         let l = ligne in
         if n >= page * nlin && n < (page + 1) * nlin then do {
           term_send nombre_wid (Printf.sprintf "%3d" l.vnombre);
           term_send dernier_wid (Printf.sprintf "%02d" l.vmois);
           term_send dernier_wid "/";
           term_send dernier_wid (Printf.sprintf "%02d" (l.vannee mod 100));
           term_send jour_wid (Printf.sprintf "%02d" l.vjour);
           match l.vposte with
           [ Some s -> term_send poste_wid s
           | _ -> () ];
           match l.vlibelle with
           [ Some s -> term_send libelle_wid s
           | _ -> () ];
           match l.vmontant with
           [ Left x -> term_send debit_wid (string_of_somme 10 x)
           | Right x -> term_send credit_wid (string_of_somme 10 x) ];
           term_send nombre_wid "\n";
           term_send dernier_wid "\n";
           term_send jour_wid "\n";
           term_send poste_wid "\n";
           term_send libelle_wid "\n";
           term_send debit_wid "\n";
           term_send credit_wid "\n";
           succ n
         }
         else succ n)
      0 budget.virAuto
  in
  ()
};

value selection_auto_gen xd ecr = do {
  let lignes = budget.virAuto in
  let fl ligne = ligne in
  let page = 0 in
  let selection_wid = rt_widget_named xd (ecr ^ "Selection term") in
  let (nlin, _) = term_get_params selection_wid in
  term_send selection_wid "\027[H\027[2J\027[?35h\027[?7l";
  let _ =
    List.fold_left
      (fun n ligne -> do {
         let l = fl ligne in
         if n >= page * nlin && n < (page + 1) * nlin then do {
           if l.vselection then term_send selection_wid "*" else ();
           term_send selection_wid "\n"
         }
         else ();
         succ n
       })
      0 lignes
  in
  ()
};

value lignes_auto xd = lignes_auto_gen xd "MA ";
value lignes_sel_auto xd = lignes_auto_gen xd "VA ";

value selection_auto xd = selection_auto_gen xd "MA ";
value selection_sel_auto xd = selection_auto_gen xd "VA ";

value solde_reduit xd = do {
  let s = calculer_soldes budget in
  let twid = rt_widget_named xd "SOLDE REDUIT term" in
  let en_cours = List.fold_left (fun n (_, d) -> n + d) 0 s.enCours in
  let solde_fictif = s.soldeReel + en_cours in
  term_send twid "\027[H\027[2J\027[?35h";
  term_send twid "Solde réel     : ";
  term_send twid (string_of_somme 10 s.soldeReel);
  term_send twid "\n";
  term_send twid "Total en-cours : ";
  term_send twid (string_of_somme 10 en_cours);
  term_send twid "\n";
  term_send twid "                  ----------";
  term_send twid "\n";
  term_send twid "Solde fictif   : ";
  term_send twid (string_of_somme 10 solde_fictif);
  term_send twid "\n";
  if s.soldeReel < 0 then
    if not state.decouvert then do {
      let err = rt_widget_named xd "Err" in
      texte_centre err "Attention, vous êtes en découvert";
      rt_map_widget err;
      state.decouvert := True;
      term_send err "\007"
    }
    else ()
  else state.decouvert := False
};

value repart_mois xd = do {
  let npostes =
    glop (budget.postesDebit @ budget.postesCredit) where rec glop =
      fun
      [ [Some _ :: pl] -> 1 + glop pl
      | [None :: pl] -> glop pl
      | [] -> 2 ]
  in
  let wid = rt_widget_named xd "REPART MOIS" in
  let twid = rt_widget_named xd "REPART MOIS term" in
  let (nlin, ncol) = term_get_params twid in
  let rep = calculer_repart [File.mois.val] in
  if nlin != npostes + 1 then do {
    term_set_params twid (npostes + 1, ncol);
    let _ = rt_adjust_widget wid in
    ()
  }
  else ();
  term_send twid "\027[2J\027[H\027[?35h";
  List.iter
    (fun (_, (lib, v)) -> do {
       term_send twid lib;
       for i = String.length lib + 1 to 10 do { term_send twid " " };
       term_send twid " : ";
       term_send twid (string_of_somme 10 v);
       term_send twid " ";
       term_send twid (string_of_somme 6 (pourcent v rep.totalDep));
       term_send twid "%\n"
     })
    rep.postesDeb;
  term_send twid "Placement  : ";
  term_send twid (string_of_somme 10 rep.placement);
  term_send twid " ";
  term_send twid (string_of_somme 6 (pourcent rep.placement rep.totalDep));
  term_send twid "%\n";
  term_send twid "\n";
  List.iter
    (fun (_, (lib, v)) -> do {
       term_send twid lib;
       for i = String.length lib + 1 to 10 do { term_send twid " " };
       term_send twid " : ";
       term_send twid (string_of_somme 10 v);
       term_send twid " ";
       term_send twid (string_of_somme 6 (pourcent v rep.totalRec));
       term_send twid "%\n"
     })
    rep.postesCre;
  term_send twid "Retrait    : ";
  term_send twid (string_of_somme 10 rep.retrait);
  term_send twid " ";
  term_send twid (string_of_somme 6 (pourcent rep.retrait rep.totalRec));
  term_send twid "%\n"
};
