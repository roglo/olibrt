(* $Id: file.mli,v 1.1 2006/05/30 17:13:16 deraugla Exp $ *)

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

value mois : ref mois;
value budget : budget;

value source : ref string;
value charger_fichier : unit -> unit;
value sauver_fichier : unit -> unit;
value copier_budget : budget -> budget -> unit;

value calculer_soldes : budget -> soldes;
value calculer_repart : list mois -> repart;

value nom_compte : int -> string;
