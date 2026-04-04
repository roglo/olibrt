(* $Id: file.ml,v 1.6 2007/05/21 17:34:57 deraugla Exp $ *)

#load "pa_extend.cmo";

type choice 'a 'b = [ Left of 'a | Right of 'b ];

type ligne =
  { lselection : mutable bool;
    lretire : mutable bool;
    ljour : mutable int;
    lnature : mutable option string;
    lposte : mutable option string;
    llibelle : mutable option string;
    lmontant : mutable choice int int }
;
type virAuto =
  { vselection : mutable bool;
    vnombre : mutable int;
    vmois : mutable int;
    vannee : mutable int;
    vjour : mutable int;
    vposte : mutable option string;
    vlibelle : mutable option string;
    vmontant : mutable choice int int }
;
type mois = { mois : int; annee : int; lignes : mutable list ligne };
type budget =
  { report : mutable int;
    noCheque : mutable int;
    postesDebit : mutable list (option (string * string));
    postesCredit : mutable list (option (string * string));
    cartes : mutable list (string * string);
    comptes : mutable list (option (string * int));
    virAuto : mutable list virAuto;
    blocNotes : mutable list string;
    listeMois : mutable list mois }
;

type soldes =
  { soldeBanque : int;
    soldeReel : int;
    soldeComptes : list (string * int);
    enCours : list (string * int) }
;

type repart =
  { postesDeb : list (string * (string * int));
    postesCre : list (string * (string * int));
    placement : int;
    retrait : int;
    totalDep : int;
    totalRec : int;
    nonImpDeb : int;
    nonImpCre : int }
;

(* $Id: file.ml,v 1.6 2007/05/21 17:34:57 deraugla Exp $ *)

value source = ref ("BudgetData" ^ ".mm");

value budget_type_version = 7;

value mois = ref {mois = 1; annee = 0; lignes = []};
value budget =
  {report = 0; noCheque = 0; postesDebit = []; postesCredit = [];
   cartes = []; comptes = []; virAuto = []; blocNotes = [];
   listeMois = [mois.val]}
;
value copier_budget src dst = do {
  dst.report := src.report;
  dst.listeMois := src.listeMois;
  dst.noCheque := src.noCheque;
  dst.postesDebit := src.postesDebit;
  dst.postesCredit := src.postesCredit;
  dst.cartes := src.cartes;
  dst.comptes := src.comptes;
  dst.virAuto := src.virAuto;
  dst.blocNotes := src.blocNotes
};

value nom_compte i =
  let ext =
    if i < 10 then string_of_int i
    else String.make 1 (Char.chr (Char.code 'A' + i - 10))
  in
  "C" ^ ext
;

(* restauration *)

value string_unescaped s =
  let b = Buffer.create (String.length s) in
  loop 0 where rec loop i =
    if i = String.length s then Buffer.contents b
    else if s.[i] = '\\' && i + 1 < String.length s then do {
      let c =
        match s.[i + 1] with
        [ 'n' -> '\n'
        | 't' -> '\t'
        | c -> c ]
      in
      Buffer.add_char b c;
      loop (i + 2)
    }
    else do { Buffer.add_char b s.[i]; loop (i + 1) }
;

value euro = 6.55957;
value franc_vers_euro x = truncate (float x /. euro +. 0.5);
(*
value conv_monnaie = franc_vers_euro;
*)
value conv_monnaie x = x;

value gram = Grammar.gcreate (Plexer.gmake ());
value file_entry = Grammar.Entry.create gram "budget file";
EXTEND
  GLOBAL: file_entry;
  file_entry:
    [ [ "value"; "version"; "="; version = INT; ";"; "value"; "budget"; "=";
        "{"; "report"; "="; report = sint_entry; ";"; "noCheque"; "=";
        nocheque = INT; ";"; "postesDebit"; "="; "[";
        postes_debit = LIST0 poste_entry SEP ";"; "]"; ";"; "postesCredit";
        "="; "["; postes_credit = LIST0 poste_entry SEP ";"; "]"; ";";
        "cartes"; "="; "["; cartes = LIST0 carte_entry SEP ";"; "]"; ";";
        "comptes"; "="; "["; comptes = LIST0 compte_entry SEP ";"; "]"; ";";
        "virAuto"; "="; "["; virauto = LIST0 vir_auto_entry SEP ";"; "]";
        ";"; "blocNotes"; "="; "["; bloc_notes = LIST0 STRING SEP ";"; "]";
        ";"; "listeMois"; "="; "["; listemois = LIST0 mois_entry SEP ";";
        "]"; "}"; ";" -> do {
          let bloc_notes = List.map string_unescaped bloc_notes in
          if int_of_string version == budget_type_version then ()
          else raise (Stream.Error "outdated version");
          {report = report; noCheque = int_of_string nocheque;
           postesDebit = postes_debit; postesCredit = postes_credit;
           cartes = cartes; comptes = comptes; virAuto = virauto;
           blocNotes = bloc_notes; listeMois = listemois}
        } ] ]
  ;
  sint_entry:
    [ [ i = INT -> conv_monnaie (int_of_string i)
      | "~"; i = INT -> - conv_monnaie (int_of_string i) ] ]
  ;
  poste_entry:
    [ [ "None" -> None
      | "Some"; "("; x = STRING; ","; y = STRING; ")" -> Some (x, y) ] ]
  ;
  carte_entry:
    [ [ "("; cod = STRING; ","; lib = STRING; ")" -> (cod, lib) ] ]
  ;
  compte_entry:
    [ [ "None" -> None
      | "Some"; "("; x = STRING; ","; y = sint_entry; ")" -> Some (x, y) ] ]
  ;
  vir_auto_entry:
    [ [ "{"; "vselection"; "="; "False"; ";"; "vnombre"; "="; vnombre = INT;
        ";"; "vmois"; "="; vmois = INT; ";"; "vannee"; "="; vannee = INT;
        ";"; "vjour"; "="; vjour = INT; ";"; "vposte"; "=";
        vposte = [ "None" -> None | "Some"; i = STRING -> Some i ]; ";";
        "vlibelle"; "=";
        vlibelle = [ "None" -> None | "Some"; i = STRING -> Some i ]; ";";
        "vmontant"; "="; vmontant = somme_entry; "}" ->
          {vselection = False; vnombre = int_of_string vnombre;
           vmois = int_of_string vmois; vannee = int_of_string vannee;
           vjour = int_of_string vjour; vposte = vposte; vlibelle = vlibelle;
           vmontant = vmontant} ] ]
  ;
  somme_entry:
    [ [ "Left"; i = INT -> Left (conv_monnaie (int_of_string i))
      | "Right"; i = INT -> Right (conv_monnaie (int_of_string i)) ] ]
  ;
  mois_entry:
    [ [ "{"; "mois"; "="; mois = INT; ";"; "annee"; "="; annee = INT; ";";
        "lignes"; "="; "["; lignes = LIST0 ligne_entry SEP ";"; "]"; "}" ->
          {mois = int_of_string mois; annee = int_of_string annee;
           lignes = lignes} ] ]
  ;
  ligne_entry:
    [ [ "{"; "lselection"; "="; "False"; ";"; "lretire"; "=";
        lretire = [ "False" -> False | "True" -> True ]; ";"; "ljour"; "=";
        ljour = INT; ";"; "lnature"; "=";
        lnature = [ "None" -> None | "Some"; i = STRING -> Some i ]; ";";
        "lposte"; "=";
        lposte = [ "None" -> None | "Some"; i = STRING -> Some i ]; ";";
        "llibelle"; "=";
        llibelle = [ "None" -> None | "Some"; i = STRING -> Some i ]; ";";
        "lmontant"; "="; lmontant = somme_entry; "}" ->
          {lselection = False; lretire = lretire;
           ljour = int_of_string ljour; lnature = lnature; lposte = lposte;
           llibelle = llibelle; lmontant = lmontant} ] ]
  ;
END;
  
value charger_fichier () =
  match try Some (open_in source.val) with _ -> None with
  [ Some ic ->
      try do {
        let b = Grammar.Entry.parse file_entry (Stream.of_channel ic) in
        let tm = Unix.localtime (Unix.time ()) in
        mois.val :=
          mois_courant b.listeMois where rec mois_courant =
            fun
            [ [] -> List.hd b.listeMois
            | [m :: ml] ->
                if m.mois == succ tm.Unix.tm_mon &&
                   m.annee mod 100 == tm.Unix.tm_year mod 100
                then
                  m
                else mois_courant ml ];
        copier_budget b budget;
        close_in ic
      }
      with exc -> do {
        close_in ic;
        raise exc
      }
  | None -> () ]
;

(* sauvegarde *)

open Printf;

value sot n chouia =
  let ntab = (n + chouia) / 8 in
  let nspc = (n + chouia) mod 8 in
  String.make ntab '\t' ^ String.make nspc ' '
;

value sauver_option sauver_elem oc tab =
  fun
  [ Some e -> do { fprintf oc "Some "; sauver_elem oc tab e }
  | None -> fprintf oc "None" ]
;

value sauver_choice sauver_left sauver_right oc tab =
  fun
  [ Left e -> do { fprintf oc "Left "; sauver_left oc tab e }
  | Right e -> do { fprintf oc "Right "; sauver_right oc tab e } ]
;

value sauver_list sauver_elem oc tab el = do {
  fprintf oc "[";
  let tab = tab + 1 in
  let rec sauver_boucle sep el = do {
    fprintf oc "%s" sep;
    match el with
    [ [] -> fprintf oc "]"
    | [e] -> do { sauver_elem oc tab e; fprintf oc "]" }
    | [e :: el] -> do {
        sauver_elem oc tab e;
        sauver_boucle (";\n" ^ sot tab 0) el
      } ]
  }
  in
  sauver_boucle "" el
};

value string_escaped =
  let b = Buffer.create 1 in
  fun s -> do {
    Buffer.clear b;
    loop 0 where rec loop i =
      if i = String.length s then Buffer.contents b
      else do {
        if s.[i] = '"' then Buffer.add_char b '\\' else ();
        Buffer.add_char b s.[i];
        loop (i + 1)
      }
  }
;

value sauver_string oc tab s = fprintf oc "\"%s\"" (string_escaped s);
value sauver_bool oc tab b = fprintf oc "%s" (if b then "True" else "False");
value sauver_int oc tab i =
  if i < 0 then fprintf oc " ~%d" (- i) else fprintf oc "%d" i
;

value sauver_pair sauver_1 sauver_2 oc tab (e1, e2) = do {
  fprintf oc "(";
  sauver_1 oc tab e1;
  fprintf oc ", ";
  sauver_2 oc tab e2;
  fprintf oc ")"
};

value sauver_ligne oc tab l = do {
  fprintf oc "{lselection=";
  sauver_bool oc tab False;
  fprintf oc "; lretire=";
  sauver_bool oc tab l.lretire;
  fprintf oc "; ljour=%d" l.ljour;
  fprintf oc ";\n%slnature=" (sot 1 tab);
  sauver_option sauver_string oc tab l.lnature;
  fprintf oc "; lposte=";
  sauver_option sauver_string oc tab l.lposte;
  fprintf oc ";\n%sllibelle=" (sot 1 tab);
  sauver_option sauver_string oc tab l.llibelle;
  fprintf oc "; lmontant=";
  sauver_choice sauver_int sauver_int oc tab l.lmontant;
  fprintf oc "}"
};

value sauver_mois oc tab e = do {
  fprintf oc "{mois=%d" e.mois;
  fprintf oc "; annee=%d" e.annee;
  fprintf oc ";\n%slignes=\n%s" (sot tab 1) (sot tab 3);
  sauver_list sauver_ligne oc (tab + 3) e.lignes;
  fprintf oc "}"
};

value sauver_vir_auto oc tab v = do {
  fprintf oc "{vselection=";
  sauver_bool oc tab False;
  fprintf oc "; vnombre=%d" v.vnombre;
  fprintf oc "; vmois=%d" v.vmois;
  fprintf oc "; vannee=%d" v.vannee;
  fprintf oc "; vjour=%d" v.vjour;
  fprintf oc ";\n%svposte=" (sot 1 tab);
  sauver_option sauver_string oc tab v.vposte;
  fprintf oc "; vlibelle=";
  sauver_option sauver_string oc tab v.vlibelle;
  fprintf oc ";\n%svmontant=" (sot 1 tab);
  sauver_choice sauver_int sauver_int oc tab v.vmontant;
  fprintf oc "}"
};

value sauver_budget oc tab budget = do {
  fprintf oc "{report=";
  sauver_int oc tab budget.report;
  fprintf oc ";\n%snoCheque=%d" (sot 1 tab) budget.noCheque;
  fprintf oc ";\n%spostesDebit=\n%s" (sot 1 tab) (sot 3 tab);
  sauver_list (sauver_option (sauver_pair sauver_string sauver_string)) oc
    (tab + 3) budget.postesDebit;
  fprintf oc ";\n%spostesCredit=\n%s" (sot 1 tab) (sot 3 tab);
  sauver_list (sauver_option (sauver_pair sauver_string sauver_string)) oc
    (tab + 3) budget.postesCredit;
  fprintf oc ";\n%scartes=\n%s" (sot 1 tab) (sot 3 tab);
  sauver_list (sauver_pair sauver_string sauver_string) oc (tab + 3)
    budget.cartes;
  fprintf oc ";\n%scomptes=\n%s" (sot 1 tab) (sot 3 tab);
  sauver_list (sauver_option (sauver_pair sauver_string sauver_int)) oc
    (tab + 3) budget.comptes;
  fprintf oc ";\n%svirAuto=\n%s" (sot 1 tab) (sot 3 tab);
  sauver_list sauver_vir_auto oc (tab + 3) budget.virAuto;
  fprintf oc ";\n%sblocNotes=\n%s" (sot 1 tab) (sot 3 tab);
  sauver_list sauver_string oc (tab + 3) budget.blocNotes;
  fprintf oc ";\n%slisteMois=\n%s" (sot 1 tab) (sot 3 tab);
  sauver_list sauver_mois oc (tab + 3) budget.listeMois;
  fprintf oc "}"
};

value recopier_fichier () =
  match try Some (open_in source.val) with _ -> None with
  [ Some ic -> do {
      let oc = open_out (source.val ^ "~") in
      let buf = Bytes.create 512 in
      let rec copy () =
        let len = input ic buf 0 (Bytes.length buf) in
        if len > 0 then do { output oc buf 0 len; copy () } else ()
      in
      copy ();
      close_in ic;
      close_out oc
    }
  | _ -> () ]
;

value sauver_fichier () = do {
  recopier_fichier ();
  let oc = open_out source.val in
  fprintf oc "value version=%d" budget_type_version;
  fprintf oc ";\nvalue budget=\n  ";
  sauver_budget oc 2 budget;
  fprintf oc ";\n";
  close_out oc
};

value calculer_soldes budget =
  let report = budget.report in
  let tm = Unix.localtime (Unix.time ()) in
  let ecc = List.map (fun (c, _) -> (c, 0)) budget.cartes in
  let (cs, _) =
    List.fold_left
      (fun (l, i) x ->
         let l =
           match x with
           [ Some (_, n) -> [(nom_compte i, n) :: l]
           | None -> l ]
         in
         (l, succ i))
      ([], 1) budget.comptes
  in
  let (sr, ecc, eca, sb, cs) =
    List.fold_left
      (fun (sr, ecc, eca, sb, cs) m ->
         List.fold_left
           (fun (sr, ecc, eca, sb, cs) l ->
              let dsr =
                match l.lmontant with
                [ Left x -> - x
                | Right x -> x ]
              in
              let futur =
                m.annee > 1900 + tm.Unix.tm_year ||
                m.annee = 1900 + tm.Unix.tm_year &&
                (m.mois > succ tm.Unix.tm_mon ||
                 m.mois = succ tm.Unix.tm_mon && l.ljour > tm.Unix.tm_mday)
              in
              let (ecc, eca) =
                match l.lnature with
                [ Some c ->
                    if not l.lretire then
                      if List.mem_assoc c budget.cartes then
                        let rec insert =
                          fun
                          [ [(c1, n1) :: ecc] ->
                              if c = c1 then [(c1, n1 - dsr) :: ecc]
                              else [(c1, n1) :: insert ecc]
                          | [] ->
                              failwith "erreur interne dans calcule_soldes" ]
                        in
                        (insert ecc, eca)
                      else (ecc, if futur then eca - dsr else eca)
                    else (ecc, if futur then eca - dsr else eca)
                | None -> (ecc, if futur then eca - dsr else eca) ]
              in
              let cs =
                match l.lposte with
                [ Some c ->
                    insert cs where rec insert =
                      fun
                      [ [] -> []
                      | [(c1, n1) :: cs] when c = c1 ->
                          [(c1, n1 - dsr) :: cs]
                      | [c :: cs] -> [c :: insert cs] ]
                | None -> cs ]
              in
              let dsb = if l.lretire then dsr else 0 in
              (sr + dsr, ecc, eca, sb + dsb, cs))
           (sr, ecc, eca, sb, cs) m.lignes)
      (report, ecc, 0, report, cs) budget.listeMois
  in
  {soldeReel = sr; soldeBanque = sb; soldeComptes = List.rev cs;
   enCours = ecc @ [("", eca)]}
;

value calculer_repart listemois = do {
  let postes_deb =
    List.fold_right
      (fun p l ->
         match p with
         [ Some (cod, lib) -> [(cod, (lib, ref 0)) :: l]
         | None -> l ])
      budget.postesDebit []
  in
  let postes_cre =
    List.fold_right
      (fun p l ->
         match p with
         [ Some (cod, lib) -> [(cod, (lib, ref 0)) :: l]
         | None -> l ])
      budget.postesCredit []
  in
  let non_imp_deb = ref 0 in
  let non_imp_cre = ref 0 in
  let placement = ref 0 in
  let retrait = ref 0 in
  let (comptes, _) =
    List.fold_left
      (fun (l, i) x ->
         match x with
         [ Some _ -> ([nom_compte i :: l], succ i)
         | None -> (l, succ i) ])
      ([], 1) budget.comptes
  in
  let total_dep = ref 0 in
  let total_rec = ref 0 in
  List.iter
    (fun m ->
       List.iter
         (fun l ->
            match l.lmontant with
            [ Left x ->
                match l.lposte with
                [ Some p ->
                    try
                      let (_, r) = List.assoc p postes_deb in
                      r.val := r.val + x
                    with
                    [ Not_found ->
                        if List.mem p comptes then
                          placement.val := placement.val + x
                        else do {
                          Printf.printf
                            "Bizarre: poste debit non trouve: %s\n" p;
                          flush stdout
                        } ]
                | None -> non_imp_deb.val := non_imp_deb.val + x ]
            | Right x ->
                match l.lposte with
                [ Some p ->
                    try
                      let (_, r) = List.assoc p postes_cre in
                      r.val := r.val + x
                    with
                    [ Not_found ->
                        if List.mem p comptes then
                          retrait.val := retrait.val + x
                        else do {
                          Printf.printf
                            "Bizarre: poste credit non trouve: %s\n" p;
                          flush stdout
                        } ]
                | None -> non_imp_cre.val := non_imp_cre.val + x ] ])
         m.lignes)
    listemois;
  List.iter (fun (_, (_, r)) -> total_dep.val := total_dep.val + r.val)
    postes_deb;
  total_dep.val := total_dep.val + placement.val;
  List.iter (fun (_, (_, r)) -> total_rec.val := total_rec.val + r.val)
    postes_cre;
  total_rec.val := total_rec.val + retrait.val;
  {postesDeb = List.map (fun (p, (lib, r)) -> (p, (lib, r.val))) postes_deb;
   postesCre = List.map (fun (p, (lib, r)) -> (p, (lib, r.val))) postes_cre;
   placement = placement.val; retrait = retrait.val;
   totalDep = total_dep.val; totalRec = total_rec.val;
   nonImpDeb = non_imp_deb.val; nonImpCre = non_imp_cre.val}
};
