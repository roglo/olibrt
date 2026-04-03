(* $Id: show.mli,v 1.1 2006/05/31 03:05:17 deraugla Exp $ *)

open RtN;

value titres : xdata -> unit;
value texte_centre : widget -> string -> unit;

value mois : xdata -> unit;
value selection : xdata -> int -> unit;
value lignes : xdata -> int -> unit;
value total : xdata -> unit;

value selection_pt : xdata -> int -> unit;
value lignes_pt : xdata -> int -> unit;
value total_pt : xdata -> unit;
value solde_pt : xdata -> unit;

value selection_auto : xdata -> unit;
value lignes_auto : xdata -> unit;

value selection_sel_auto : xdata -> unit;
value lignes_sel_auto : xdata -> unit;

value solde_reduit : xdata -> unit;
value repart_mois : xdata -> unit;
