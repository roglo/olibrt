(* $Id: d_blocnotes.ml,v 1.8 2006/06/02 00:22:00 deraugla Exp $ *)

open State;
open File;
open RtN;

value pages = [1; 2; 3; 4; 5; 6; 7; 8];

value bn_cour = ref 1;

value strip_line str =
  strip (String.length str) where rec strip len =
    if len == 0 then ""
    else if str.[len - 1] == ' ' then strip (pred len)
    else String.sub str 0 len
;

value strip_array arr =
  strip (Array.length arr) where rec strip len =
    if len == 0 then [| |]
    else if arr.(len - 1) = "" then strip (pred len)
    else Array.sub arr 0 len
;

value strip_list list =
  strip (List.rev list) where rec strip =
    fun
    [ [] -> []
    | [x :: l] -> if x = "" then strip l else List.rev [x :: l] ]
;

value bloc_notes xd =
  let bn =
    List.map
      (fun x -> do {
         let wid = rt_widget_named xd ("BN term " ^ string_of_int x) in
         let (nlin, _) = term_get_params wid in
         let arr = Array.make nlin "" in
         for i = 0 to nlin - 1 do {
           arr.(i) := strip_line (term_line wid i);
         };
         fst
           (List.fold_right (fun s (str, sep) -> (s ^ sep ^ str, "\n"))
              (Array.to_list (strip_array arr)) ("", ""))
       })
      pages
  in
  strip_list bn
;

value action_X wid = do {
  let xd = rt_xdata_of_widget wid in
  let bn = bloc_notes xd in
  rt_unmap_widget (rt_widget_named xd "BLOC NOTES");
  if budget.blocNotes <> bn then do {
    budget.blocNotes := bloc_notes xd;
    modif xd
  }
  else ()
};

value afficher_bn wid = do {
  let xd = rt_xdata_of_widget wid in
  rt_map_widget
    (rt_widget_named xd ("BN term " ^ string_of_int bn_cour.val));
  Show.texte_centre (rt_widget_named xd "BLOC NOTES numero")
    ("- " ^ string_of_int bn_cour.val ^ " -")
};

value action_up wid =
  fun
  [ ArrowEvPress ->
      if bn_cour.val > 1 then do {
        bn_cour.val := bn_cour.val - 1;
        afficher_bn wid
      }
      else ()
  | _ -> () ]
;

value action_down wid =
  fun
  [ ArrowEvPress ->
      if bn_cour.val < List.length pages then do {
        bn_cour.val := bn_cour.val + 1;
        afficher_bn wid
      }
      else ()
  | _ -> () ]
;

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  Show.texte_centre (rt_widget_named xd "BLOC NOTES titre") "BLOC NOTES";
  rt_map_widget (rt_widget_named xd "BLOC NOTES");
  let _ =
    List.fold_left
      (fun i txt -> do {
         let wid = rt_widget_named xd ("BN term " ^ string_of_int i) in
         term_send wid "\027[H\027[2J";
         term_send wid txt;
         term_send wid "\027[H";
         succ i
       })
      1 budget.blocNotes
  in
  ();
  afficher_bn wid
};

value accent = ref None;

value est_accent =
  fun
  [ ''' | '`' | '^' | ',' | '"' -> True
  | _ -> False ]
;

value echo wid kmod ksym =
  let xd = rt_xdata_of_widget wid in
  let twid = rt_widget_named xd ("BN term " ^ string_of_int bn_cour.val) in
  match ksym with
  [ Rt.K_Return ->
      term_send twid "\n"
  | Rt.K_BackSpace ->
      term_send twid "\b"
  | Rt.K_Left ->
      term_send twid "\027[D"
  | Rt.K_Up ->
      term_send twid "\027[A"
  | Rt.K_Right ->
      term_send twid "\027[C"
  | Rt.K_Down ->
      term_send twid "\027[B"
  | Rt.K_Delete ->
      if kmod land 0x8 != 0 then term_send twid "\027[2J\027[H"
      else term_send twid "\027[K"
  | Rt.K_Ascii c ->
      if kmod land 0x8 != 0 && est_accent c then do {
        term_send twid (String.make 1 c);
        term_send twid "\b";
        accent.val := Some c
      }
      else
        let c =
          match accent.val with
          [ Some a -> do { accent.val := None; Action.mettre_accent a c }
          | None -> c ]
        in
        term_send twid (String.make 1 c)
  | _ ->
      () ]
;

value blin = ref 0
and bcol = ref 0;
value inside_term wid lin col =
  let (nlin, ncol) = term_get_params wid in
  (max 0 (min (nlin - 1) lin), max 0 (min ncol col))
;

value act_term wid =
  fun
  [ TermEvKeyPress kmod ksym ->
      echo wid kmod ksym
  | TermEvButtonPress ({tEvButton = 1} as b) _ -> do {
      let (lin, col) = inside_term wid b.tEvRow b.tEvCol in
      blin.val := lin;
      bcol.val := col;
      term_emph_from wid lin col
    }
  | TermEvButtonMotion ({tEvButton = 1} as b) ->
      let (lin, col) = inside_term wid b.tEvRow b.tEvCol in
      term_emph_to wid lin col
  | TermEvButtonRelease b -> do {
      Jmage.term_goto wid b.tEvRow b.tEvCol;
      match b.tEvButton with
      [ 1 -> do {
          let (lin, col) = inside_term wid b.tEvRow b.tEvCol in
          term_emph_to wid lin col;
          let (blin, bcol, elin, ecol) =
            if lin < blin.val then (lin, col, blin.val, bcol.val)
            else if lin > blin.val then (blin.val, bcol.val, lin, col)
            else if col < bcol.val then (lin, col, blin.val, bcol.val)
            else (blin.val, bcol.val, lin, col)
          in
          let (_, ncol) = term_get_params wid in
          let str =
            get blin bcol where rec get lin col =
              let str = term_line wid lin in
              if lin == elin then
                strip_line (String.sub str col (ecol - col))
              else
                strip_line (String.sub str col (ncol - col)) ^ "\n" ^
                get (succ lin) 0
          in
          rt_set_cut_buffer wid str
        }
      | 2 -> rt_get_cut_buffer wid
      | _ -> () ]
    }
  | TermEvClearCutBuffer ->
      term_emph_from wid 0 0
  | TermEvCutBufferGot str ->
      term_send wid str
  | _ ->
      () ]
;

value act_term_titre wid =
  fun
  [ TermEvKeyPress kmod ksym -> echo wid kmod ksym
  | TermEvSizeChanged -> Show.texte_centre wid "BLOC NOTES"
  | _ -> () ]
;

value act_numero wid =
  fun
  [ TermEvKeyPress kmod ksym -> echo wid kmod ksym
  | TermEvSizeChanged ->
      Show.texte_centre wid ("- " ^ string_of_int bn_cour.val ^ " -")
  | _ -> () ]
;

value action_no_pack wid =
  fun [ PackEvKeyPress kmod ksym -> echo wid kmod ksym ]
;

value filler = (INCSZ, pack_desc [BandAtt 0] (DIRx, []) action_no_pack);

value wdesc =
  pack_desc [NameAtt "BLOC NOTES"]
    (DIRy,
     [(FIXSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc [] ("X", None) (Action.button action_X));
           (INCSZ,
            term_desc [NameAtt "BLOC NOTES titre"] (1, 10) act_term_titre)])
         action_no_pack);
      (FIXSZ, line_desc [] () (fun _ _ -> ()));
      (INCSZ,
       pack_desc []
         (DIRx,
          [(INCSZ,
            pack_desc [BandAtt 0; InterAtt 0]
              (DIRy,
               [(INCSZ,
                 pack_desc [BandAtt 0]
                   (DIRz,
                    List.map
                      (fun x ->
                         let n = "BN term " ^ string_of_int x in
                         (INCSZ,
                          term_desc [NameAtt n; BandAtt 0] (20, 25)
                            act_term))
                      pages)
                   action_no_pack);
                (FIXSZ,
                 term_desc [NameAtt "BLOC NOTES numero"; BandAtt 0] (1, 1)
                   act_numero)])
              action_no_pack);
           (FIXSZ,
            pack_desc []
              (DIRy,
               [(FIXSZ, arrow_desc [] ORup action_up); filler;
                (FIXSZ, arrow_desc [] ORdown action_down)])
              action_no_pack)])
         action_no_pack)])
    action_no_pack
;
