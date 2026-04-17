(* $Id: state.ml,v 1.2 2006/06/02 00:22:01 deraugla Exp $ *)

open File;
open Jmage;
open RtN;

type action =
  [ ModifierOuiNon
  | MajPostesOuiNon
  | MajCartesOuiNon
  | MajComptesOuiNon
  | EcranRepartPoste
  | EcranSoldeGen
  | EcranBilan
  | Ajoutant
  | Transferant
  | Rien ]
;

type state =
  { quit : mutable bool;
    modifie : mutable bool;
    keyPressAct : mutable int -> Rt.keysym -> unit;
    buttonAct : mutable bool -> int -> unit;
    iP : mutable option inputPic;
    action : mutable action;
    action_quit : mutable widget -> unit;
    nbMarques : mutable int;
    noPage : mutable int;
    nlin : mutable int;
    tdebit : mutable int;
    tcredit : mutable int;
    decouvert : mutable bool;
    ptSelection : mutable list (ref ligne * int * int);
    ptSoldeBanque : mutable int;
    ptEnCoursCartes : mutable int;
    majAuto : mutable bool }
;

exception ErrField of string and int;

(* $Id: state.ml,v 1.2 2006/06/02 00:22:01 deraugla Exp $ *)

open File;
open Jmage;
open RtN;

value default_state () =
  {quit = False; modifie = False; keyPressAct _ _ = (); buttonAct _ _ = ();
   iP = None; action = Rien; action_quit _ = (); nbMarques = 0; noPage = 0;
   nlin = 0; tdebit = 0; tcredit = 0; decouvert = False; ptSelection = [];
   ptSoldeBanque = 0; ptEnCoursCartes = 0; majAuto = False}
;
value state = default_state ();
value init_state =
  let def = default_state () in
  fun () -> do {
    state.quit := def.quit;
    state.modifie := def.modifie;
    state.keyPressAct := def.keyPressAct;
    state.buttonAct := def.buttonAct;
    state.iP := def.iP;
    state.action := def.action;
    state.nbMarques := def.nbMarques;
    state.noPage := def.noPage;
    state.nlin := def.nlin;
    state.tdebit := def.tdebit;
    state.tcredit := def.tcredit;
    state.decouvert := def.decouvert;
    state.ptSelection := def.ptSelection;
    state.ptSoldeBanque := def.ptSoldeBanque;
    state.ptEnCoursCartes := def.ptEnCoursCartes;
    state.majAuto := def.majAuto
  }
;

value modif xd =
  if not state.modifie then do {
    state.modifie := True;
    rt_unfreeze_widget (rt_widget_named xd "Sauver")
  }
  else ()
;

value get_ip () =
  match state.iP with
  [ Some ip -> ip
  | _ -> failwith "get_ip" ]
;

value rec nth _xxx1 _xxx2 =
  match (_xxx1, _xxx2) with
  [ (_, []) -> invalid_arg "nth"
  | (0, [x :: _]) -> x
  | (n, [_ :: l]) -> nth (pred n) l ]
;

value ligne_marquee () =
  find_selected mois.val.lignes where rec find_selected =
    fun
    [ [] -> failwith "ligne_marquee"
    | [l :: ll] -> if l.lselection then l else find_selected ll ]
;

value maj_total op l =
  match l.lmontant with
  [ Left x -> state.tdebit := op state.tdebit x
  | Right x -> state.tcredit := op state.tcredit x ]
;

value rec mem_poste p =
  fun
  [ [Some (p1, _) :: pl] -> p = p1 || mem_poste p pl
  | [None :: pl] -> mem_poste p pl
  | [] -> False ]
;

value trouver_poste =
  fun
  [ "" -> (True, True)
  | nom ->
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
      if List.mem_assoc nom cs then (True, True)
      else
        (mem_poste nom budget.postesDebit,
         mem_poste nom budget.postesCredit) ]
;

value ajouter_ligne ip incno = do {
  let ligne =
    {lselection = False;
     lretire =
       match get_field ip 0 with
       [ Fstring _ -> True
       | _ -> False ];
     ljour = get_int (get_field ip 1);
     lnature =
       match get_field ip 4 with
       [ Fstring s -> Some s
       | _ -> None ];
     lposte =
       match get_field ip 5 with
       [ Fstring s -> Some s
       | _ -> None ];
     llibelle =
       match get_field ip 6 with
       [ Fstring s -> Some s
       | _ -> None ];
     lmontant =
       match (get_field ip 7, get_field ip 8) with
       [ (Fdecimal (m, d, 2), _) -> Left (m * 100 + d)
       | (_, Fdecimal (m, d, 2)) -> Right (m * 100 + d)
       | _ -> failwith "erreur interne dans ajouter_ligne" ]}
  in
  let (i, lignes) =
    add 0 mois.val.lignes where rec add i =
      fun
      [ [l :: ll] ->
          if ligne.ljour < l.ljour then (i, [ligne; l :: ll])
          else
            let (i, lignes) = add (succ i) ll in
            (i, [l :: lignes])
      | [] -> (i, [ligne]) ]
  in
  mois.val.lignes := lignes;
  if incno then
    match get_field ip 4 with
    [ Fstring n -> try budget.noCheque := succ (int_of_string n) with _ -> ()
    | _ -> () ]
  else ();
  modif (rt_xdata_of_widget ip.iPwid);
  (i, ligne)
};

value supprimer_ligne_marquee xd = do {
  mois.val.lignes :=
    enlever mois.val.lignes where rec enlever =
      fun
      [ [] -> []
      | [l :: ll] -> if l.lselection then ll else [l :: enlever ll] ];
  modif xd
};

value pourcent x y =
  if y == 0 then 0 else truncate (float x *. 10000.0 /. float y +. 0.5)
;
