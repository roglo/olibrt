(* $Id: ajoutModif.ml,v 1.4 2006/05/31 03:05:15 deraugla Exp $ *)

open State;
open File;
open Jmage;
open RtN;

value image =
  "\
Retiré  : %1s
Date    : %02d/%02d/%02d
Nature  : %10s
Poste   : %2s
Libellé : %20s
Débit   : %10.2f
Crédit  : %10.2f"
;

value verif ip = do {
  let twid = ip.iPwid in
  let xd = rt_xdata_of_widget twid in
  let jour = get_field ip 1 in
  let nature =
    let f = get_string (get_field ip 4) in
    let u = uppercase f in
    if List.mem_assoc u budget.cartes then u else f
  in
  let poste = uppercase (get_string (get_field ip 5)) in
  let debit = get_field ip 7 in
  let credit = get_field ip 8 in
  term_send twid "\0277";
  if get_field ip 0 <> Fempty then set_field ip 0 (Fstring "X") else ();
  set_field ip 1 (get_field ip 1);
  set_field ip 4 (Fstring nature);
  set_field ip 5 (Fstring poste);
  set_field ip 6 (get_field ip 6);
  set_field ip 7 (get_field ip 7);
  set_field ip 8 (get_field ip 8);
  term_send twid "\0278";
  try do {
    if let j = get_int jour in
       j <= 0 || j > 31
    then
      raise (ErrField "Jour invalide" 1)
    else ();
    if debit = Fempty && credit = Fempty ||
       debit <> Fempty && credit <> Fempty
    then
      raise (ErrField "Renseignez débit ou crédit" 7)
    else ();
    match (trouver_poste poste, (debit, credit)) with
    [ ((True, _), (_, Fempty)) | ((_, True), (Fempty, _)) -> ()
    | ((False, False), _) -> raise (ErrField "Poste inexistant" 5)
    | _ -> raise (ErrField "Poste dans la mauvaise catégorie" 5) ];
    term_send twid "\027[?35h\027[?7l";
    rt_map_alert (rt_widget_named xd "Correct alert");
    True
  }
  with
  [ ErrField str f -> do {
      let err = rt_widget_named xd "Err" in
      Show.texte_centre err str;
      rt_map_widget err;
      goto_field ip f False;
      False
    } ]
};
