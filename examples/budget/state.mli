(* $Id: state.mli,v 1.2 2006/06/02 00:22:01 deraugla Exp $ *)

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

value state : state;
value init_state : unit -> unit;
value get_ip : unit -> inputPic;

value ligne_marquee : unit -> ligne;
value maj_total : (int -> int -> int) -> ligne -> unit;

value trouver_poste : string -> (bool * bool);

value ajouter_ligne : inputPic -> bool -> (int * ligne);
value supprimer_ligne_marquee : xdata -> unit;

value modif : xdata -> unit;
value pourcent : int -> int -> int;
