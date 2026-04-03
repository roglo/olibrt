(* $Id: a_entree.ml,v 1.2 2006/05/30 17:13:13 deraugla Exp $ *)

open State;
open RtN;

value entree_wdesc =
  pack_desc [NameAtt "entree"]
    (DIRy,
     [Action.filler;
      (INCSZ,
       pack_desc []
         (DIRx,
          [Action.filler;
           (FIXSZ,
            pack_desc [InterAtt 10]
              (DIRy,
               [(FIXSZ,
                 button_desc [] ("Compte bancaire", None)
                   (Action.button B_compte.action));
                (FIXSZ,
                 button_desc [] ("Pointage", None)
                   (Action.button B_pointage.action));
                (FIXSZ,
                 button_desc [] ("Initialisation", None)
                   (Action.button B_init.action));
                (FIXSZ,
                 button_desc [] ("Quitter", None)
                   (Action.button (fun _ -> state.quit := True)))])
              Action.no_pack);
           Action.filler])
         Action.no_pack);
      Action.filler])
    Action.no_pack
;

value wdesc =
  pack_desc [NameAtt "Main"]
    (DIRz,
     [(INCSZ, entree_wdesc); (INCSZ, B_compte.wdesc);
      (INCSZ, B_pointage.wdesc); (INCSZ, B_init.wdesc)])
    Action.no_pack
;
