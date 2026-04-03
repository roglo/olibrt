(* $Id: c_popup.ml,v 1.5 2006/05/30 17:13:14 deraugla Exp $ *)

open State;
open File;
open RtN;

value action_sauver wid = do {
  sauver_fichier ();
  state.modifie := False;
  rt_freeze_widget wid
};

value action_ajouter wid =
  if state.majAuto then D_ajoutAuto.action wid else D_ajout.action wid
;
value action_modifier wid =
  if state.majAuto then D_modifAuto.action wid else D_modif.action wid
;
value action_supprimer wid =
  if state.majAuto then D_supprAuto.action wid else D_suppr.action wid
;
value action_transferer wid = D_transf.action wid;

value fichier_wdesc =
  pack_desc [NameAtt "Fichier popup"]
    (DIRy,
     [(FIXSZ,
       button_desc [] ("Liste mois", None)
         (Action.button D_listeMois.action));
      (FIXSZ,
       button_desc [] ("Nouveau mois", None)
         (Action.button D_nouvMois.action));
      (FIXSZ,
       button_desc [] ("Imprimer", Some 'p')
         (Action.button D_imprimer.action));
      (FIXSZ,
       button_desc [] ("Sauver", Some 's') (Action.button action_sauver));
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       button_desc [] ("Quitter", Some 'q')
         (Action.button B_compte.action_quit))])
    Action.no_pack
;

value traitement_wdesc =
  pack_desc [NameAtt "Traitement popup"]
    (DIRy,
     [(FIXSZ,
       button_desc [] ("Ajouter", None) (Action.button action_ajouter));
      (FIXSZ,
       button_desc [NameAtt "TRAIT Virements automatiques"]
         ("Virements automatiques", None) (Action.button D_auto.action));
      (FIXSZ, line_desc [] () Action.no_line);
      (FIXSZ,
       button_desc [] ("Modifier", None) (Action.button action_modifier));
      (FIXSZ,
       button_desc [] ("Supprimer", None) (Action.button action_supprimer));
      (FIXSZ,
       button_desc [] ("Transférer", None)
         (Action.button action_transferer))])
    Action.no_pack
;

value budget_wdesc =
  pack_desc [NameAtt "Budget popup"]
    (DIRy,
     [(FIXSZ,
       button_desc [] ("Solde réduit", None)
         (Action.button D_soldeRed.action));
      (FIXSZ,
       button_desc [] ("Répartition du mois", None)
         (Action.button D_repartMois.action));
      (FIXSZ,
       button_desc [] ("Répartition par poste", None)
         (Action.button D_repartPoste.action));
      (FIXSZ,
       button_desc [] ("Solde général", None)
         (Action.button D_soldeGen.action));
      (FIXSZ,
       button_desc [] ("Bilan de l'année", None)
         (Action.button D_bilan.action))])
    Action.no_pack
;

value informations_wdesc =
  pack_desc [NameAtt "Informations popup"]
    (DIRy,
     [(FIXSZ,
       button_desc [NameAtt "INFO Postes"] ("Postes", None)
         (Action.button D_infoPostes.action));
      (FIXSZ,
       button_desc [NameAtt "INFO Cartes"] ("Cartes", None)
         (Action.button D_infoCartes.action));
      (FIXSZ,
       button_desc [NameAtt "INFO Comptes spéciaux"]
         ("Comptes spéciaux", None) (Action.button D_infoComptes.action));
      (FIXSZ,
       button_desc [] ("Calculatrice", None)
         (Action.button D_calculat.action));
      (FIXSZ,
       button_desc [] ("Bloc-notes", None)
         (Action.button D_blocnotes.action))])
    Action.no_pack
;

value mise_a_jour_wdesc =
  pack_desc [NameAtt "Mise à jour popup"]
    (DIRy,
     [(FIXSZ,
       button_desc [NameAtt "MAJ Postes"] ("Postes", None)
         (Action.button D_majPostes.action));
      (FIXSZ,
       button_desc [NameAtt "MAJ Cartes"] ("Cartes", None)
         (Action.button D_majCartes.action));
      (FIXSZ,
       button_desc [NameAtt "MAJ Comptes spéciaux"]
         ("Comptes spéciaux", None) (Action.button D_majComptes.action));
      (FIXSZ,
       button_desc [] ("Virements automatiques", None)
         (Action.button D_majAuto.action))])
    Action.no_pack
;
