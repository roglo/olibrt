(* $Id: d_majPostes.ml,v 1.9 2006/06/02 06:58:16 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value debit_pic =
  " \
 1) %2s %10s
 2) %2s %10s
 3) %2s %10s
 4) %2s %10s
 5) %2s %10s
 6) %2s %10s
 7) %2s %10s
 8) %2s %10s
 9) %2s %10s
10) %2s %10s"
;

value credit_pic =
  " 1) %2s %10s\n 2) %2s %10s\n 3) %2s %10s\n 4) %2s %10s\n 5) %2s %10s"
;

value rec renomme p _xxx1 _xxx2 =
  match (_xxx1, _xxx2) with
  [ ([Some (p1, _) :: pl1], [Some (p2, _) :: pl2]) ->
      if p = p1 then Some p2 else renomme p pl1 pl2
  | ([Some (p1, _) :: pl1], [None :: pl2]) ->
      if p = p1 then None else renomme p pl1 pl2
  | ([_ :: pl1], [_ :: pl2]) ->
      renomme p pl1 pl2
  | (_, _) ->
      Some p ]
;

value maj_budget (pdeb1, pcre1) (pdeb2, pcre2) = do {
  List.iter
    (fun m ->
       List.iter
         (fun l ->
            match l.lposte with
            [ Some p ->
                match l.lmontant with
                [ Left _ -> l.lposte := renomme p pdeb1 pdeb2
                | Right _ -> l.lposte := renomme p pcre1 pcre2 ]
            | None -> () ])
         m.lignes)
    budget.listeMois;
  List.iter
    (fun v ->
       match v.vposte with
       [ Some p ->
           match v.vmontant with
           [ Left _ -> v.vposte := renomme p pdeb1 pdeb2
           | Right _ -> v.vposte := renomme p pcre1 pcre2 ]
       | None -> () ])
    budget.virAuto
};

value get_list ip debut fin plist =
  iterate plist debut where rec iterate plist i =
    if i >= fin then ([], plist)
    else
      match get_field ip (2 * i) with
      [ Fstring nom -> do {
          let nom = uppercase nom in
          set_field ip (2 * i) (Fstring nom);
          for j = 0 to String.length nom - 1 do {
            match nom.[j] with
            [ 'A'..'Z' | ' ' -> ()
            | _ -> raise (ErrField "Caractère invalide" (2 * i)) ];
          };
          if List.mem nom plist then
            raise (ErrField "Code poste en double" (2 * i))
          else ();
          let lib =
            match get_field ip (2 * i + 1) with
            [ Fstring lib -> lib
            | _ ->
                raise
                  (ErrField "Il manque le libellé du poste" (2 * i + 1)) ]
          in
          let (l, plist) = iterate [nom :: plist] (succ i) in
          ([Some (nom, lib) :: l], plist)
        }
      | _ ->
          match get_field ip (2 * i + 1) with
          [ Fstring _ -> raise (ErrField "Code poste absent" (2 * i))
          | _ ->
              let (l, plist) = iterate plist (succ i) in
              ([None :: l], plist) ] ]
;


value postes_of_ip ip =
  let plist = [] in
  let (deb, plist) = get_list ip 0 10 plist in
  let (cre, plist) = get_list ip 10 15 plist in
  (deb, cre)
;

value rec verif_maj_postes ip =
  let xd = rt_xdata_of_widget ip.iPwid in
  try do {
    let (postes_debit, postes_credit) = postes_of_ip ip in
    init_maj_postes xd postes_debit postes_credit;
    term_send ip.iPwid "\027[?35h";
    state.action := MajPostesOuiNon;
    rt_map_alert (rt_widget_named xd "Correct alert");
    state.keyPressAct :=
      fun _ ksym ->
        match ksym with
        [ Rt.K_Return -> action_correct_oui ip.iPwid
        | Rt.K_Escape -> action_correct_non ip.iPwid
        | _ -> () ]
  }
  with
  [ ErrField str f -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err str;
      rt_map_widget err;
      goto_field ip f False
    } ]
and action_correct_oui wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  let (postes_debit, postes_credit) = postes_of_ip ip in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  rt_unmap_widget (rt_widget_named xd "MAJ POSTES");
  state.keyPressAct := fun _ _ -> ();
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Postes"; "Calculatrice";
     "Bloc-notes"; "Mise à jour"];
  maj_budget (budget.postesDebit, budget.postesCredit)
    (postes_debit, postes_credit);
  budget.postesDebit :=
    let rec glip =
      fun
      [ [None :: l] -> glip l
      | l -> l ]
    in
    List.rev (glip (List.rev postes_debit));
  budget.postesCredit :=
    let rec glip =
      fun
      [ [None :: l] -> glip l
      | l -> l ]
    in
    List.rev (glip (List.rev postes_credit));
  Show.lignes xd state.noPage;
  Show.repart_mois xd;
  modif xd;
  state.action := Rien
}
and action_correct_non wid = do {
  state.action := Rien;
  let xd = rt_xdata_of_widget wid in
  let ip = get_ip () in
  rt_unmap_alert xd;
  rt_map_widget (rt_widget_named xd "NoErr");
  goto_field ip 0 False;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      verif_maj_postes
}
and init_maj_postes xd pdeb pcre = do {
  let twid1 = rt_widget_named xd "MAJP debit term" in
  let twid2 = rt_widget_named xd "MAJP credit term" in
  let ip1 = input_of_pic twid1 debit_pic in
  let ip2 = input_of_pic twid2 credit_pic in
  let ip =
    {iParr = Array.append ip1.iParr ip2.iParr; iPcur = 0; iPlin = 0;
     iPcol = 0; iPtyp = ' '; iPdec = 0; iPstr = ""; iPind = 0;
     iPwid = ip1.iPwid}
  in
  term_send twid1 "\027[H\027[2J\027[?35h";
  term_send twid2 "\027[H\027[2J\027[?35h";
  term_send twid1 (empty_pic debit_pic);
  term_send twid2 (empty_pic credit_pic);
  let sh = 0 in
  fill 0 pdeb where rec fill i =
    fun
    [ [Some (pc, pl) :: pdeb] -> do {
        set_field ip (sh + 2 * i) (Fstring pc);
        set_field ip (sh + 2 * i + 1) (Fstring pl);
        fill (succ i) pdeb
      }
    | [None :: pdeb] -> fill (succ i) pdeb
    | [] -> () ];
  let sh = 2 * 10 in
  fill 0 pcre where rec fill i =
    fun
    [ [Some (pc, pl) :: pcre] -> do {
        set_field ip (sh + 2 * i) (Fstring pc);
        set_field ip (sh + 2 * i + 1) (Fstring pl);
        fill (succ i) pcre
      }
    | [None :: pcre] -> fill (succ i) pcre
    | [] -> () ];
  goto_field ip 0 False;
  state.iP := Some ip;
  state.keyPressAct :=
    Action.input ip (fun _ -> rt_map_widget (rt_widget_named xd "NoErr"))
      verif_maj_postes
};

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  List.iter (fun n -> rt_unfreeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Postes"; "Calculatrice";
     "Bloc-notes"; "Mise à jour"];
  rt_map_widget (rt_widget_named xd "NoErr");
  rt_unmap_widget (rt_widget_named xd "MAJ POSTES");
  state.action := Rien;
  state.keyPressAct := fun _ _ -> ()
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let wid = rt_widget_named xd "MAJ POSTES" in
  let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
  let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
  List.iter (fun n -> rt_freeze_widget (rt_widget_named xd n))
    ["Nouveau mois"; "Quitter"; "Traitement"; "Répartition par poste";
     "Solde général"; "Bilan de l'année"; "INFO Postes"; "Calculatrice";
     "Bloc-notes"; "Mise à jour"];
  init_maj_postes xd budget.postesDebit budget.postesCredit;
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y);
  rt_map_widget wid;
  rt_unmap_widget (rt_widget_named xd "INFO POSTES")
};

value wdesc =
  pack_desc [NameAtt "MAJ POSTES"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "MAJ POSTES titre"] (1, 20) Action.no_term)])
         Action.no_pack);
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       pack_desc []
         (DIRx,
          [(INCSZ,
            pack_desc []
              (DIRy,
               [(FIXSZ,
                 term_desc [NameAtt "MAJP debit titre"] (1, 1)
                   Action.no_term);
                (INCSZ,
                 term_desc [NameAtt "MAJP debit term"] (10, 18)
                   Action.no_term)])
              Action.no_pack);
           (INCSZ,
            pack_desc []
              (DIRy,
               [(FIXSZ,
                 term_desc [NameAtt "MAJP credit titre"] (1, 1)
                   Action.no_term);
                (INCSZ,
                 term_desc [NameAtt "MAJP credit term"] (5, 18)
                   Action.no_term)])
              Action.no_pack)])
         Action.no_pack)])
    Action.no_pack
;
