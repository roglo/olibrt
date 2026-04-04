(* $Id: action.ml,v 1.9 2006/06/02 00:22:00 deraugla Exp $ *)

open State;
open Jmage;
open RtN;

value button a wid =
  fun
  [ ButtonEvRelease -> a wid
  | ButtonEvShortcut -> a wid
  | _ -> () ]
;

value popup name =
  let doit wid xll yll = do {
    let xd = rt_xdata_of_widget wid in
    let pwid = rt_widget_named xd name in
    rt_move_widget pwid xll yll;
    rt_map_widget pwid
  }
  in
  fun wid ->
    fun
    [ ButtonEvPress xll yll -> do {
        let xd = rt_xdata_of_widget wid in
        rt_map_widget (rt_widget_named xd "NoErr");
        doit wid xll yll
      }
    | ButtonEvEnter xll yll -> doit wid xll yll
    | _ -> () ]
;

value no_button wid _ = ();

value no_line wid _ = ();

value no_pack wid =
  fun [ PackEvKeyPress kmod ksym -> state.keyPressAct kmod ksym ]
;

value no_term wid =
  fun
  [ TermEvKeyPress kmod ksym -> state.keyPressAct kmod ksym
  | _ -> () ]
;

value butt_term wid =
  fun
  [ TermEvKeyPress kmod ksym -> state.keyPressAct kmod ksym
  | TermEvButtonPress b _ -> state.buttonAct True b.tEvRow
  | TermEvButtonMotion b -> state.buttonAct True b.tEvRow
  | TermEvButtonRelease b -> state.buttonAct False b.tEvRow
  | TermEvSizeChanged ->
      let xd = rt_xdata_of_widget wid in
      if rt_eq_widget wid (rt_widget_named xd "Libelle term") then do {
        let (nlin, _) = term_get_params wid in
        state.noPage := 0;
        state.nlin := nlin;
        Show.titres xd;
        Show.mois xd;
        Show.lignes xd state.noPage;
        Show.selection xd state.noPage;
        Show.total xd;
        Show.lignes_pt xd state.noPage;
        Show.selection_pt xd state.noPage;
        Show.total_pt xd;
        Show.solde_pt xd
      }
      else ()
  | _ -> () ]
;

value highlight xd row =
  List.iter
    (fun name -> do {
       let wid = rt_widget_named xd name in
       let (_, col) = term_get_params wid in
       term_emph_from wid row 0;
       term_emph_to wid row col
     })
;

value unhighlight xd row =
  List.iter
    (fun name ->
       let wid = rt_widget_named xd name in
       term_emph_from wid row 0)
;

value button_select xd terms liste sh f =
  let nrow = List.length liste in
  let highlighted = ref None in
  fun press row ->
    let sh = sh () in
    if press then
      if match highlighted.val with
         [ Some row1 ->
             if row1 <> row then do {
               unhighlight xd row1 terms;
               highlighted.val := None;
               True
             }
             else False
         | _ -> True ]
      then
        if row >= 0 && row < nrow then do {
          highlight xd row terms;
          highlighted.val := Some row
        }
        else ()
      else ()
    else if
      match highlighted.val with
      [ Some row1 -> do {
          unhighlight xd row1 terms;
          highlighted.val := None;
          row1 == row
        }
      | None -> False ]
    then
      match
        nth (row + sh) liste where rec nth _xxx1 _xxx2 =
          match (_xxx1, _xxx2) with
          [ (_, []) -> None
          | (0, [x :: _]) -> Some x
          | (n, [_ :: l]) -> nth (pred n) l ]
      with
      [ Some ligne -> f ligne
      | _ -> () ]
    else ()
;

value mettre_accent_aux accent car =
  match (accent, car) with
  [ ('A', '`') -> 'À'
  | ('a', 'a') -> 'å'
  | ('^', 'a') -> 'â'
  | ('`', 'a') -> 'à'
  | ('"', 'a') -> 'ä'
  | (',', 'C') -> 'Ç'
  | (',', 'c') -> 'ç'
  | (''', 'E') -> 'É'
  | ('^', 'e') -> 'ê'
  | ('`', 'e') -> 'è'
  | (''', 'e') -> 'é'
  | ('"', 'e') -> 'ë'
  | ('~', 'n') -> 'ñ'
  | ('^', 'o') -> 'ô'
  | ('s', 's') -> 'ß'
  | ('^', 'u') -> 'û'
  | ('`', 'u') -> 'ù'
  | ('"', 'u') -> 'ü'
  | ('1', '2') -> '½'
  | ('1', '4') -> '¼'
  | (a, c) -> raise Not_found ]
;

value mettre_accent acc car =
  try mettre_accent_aux acc car with
  [ Not_found -> try mettre_accent_aux car acc with [ Not_found -> car ] ]
;

value input ip hook_key kont_ret =
  let rec next_field i eof =
    let i = (i + 1) mod Array.length ip.iParr in
    let (wonly, _, _, _, _, _, _, _) = ip.iParr.(i) in
    if wonly then next_field i eof else goto_field ip i eof
  in
  let rec prev_field i eof =
    let i = if i == 0 then Array.length ip.iParr - 1 else i - 1 in
    let (wonly, _, _, _, _, _, _, _) = ip.iParr.(i) in
    if wonly then prev_field i eof else goto_field ip i eof
  in
  let accent = ref None in
  let multi_key = ref False in
  fun kmod ksym -> do {
    let last_was_multi_key = do {
      let v = multi_key.val in
      multi_key.val := False;
      v
    }
    in
    hook_key ksym;
    let twid = ip.iPwid in
    match ksym with
    [ Rt.K_Return ->
        kont_ret ip
    | Rt.K_Left | Rt.K_BackSpace ->
        if ip.iPind > 0 then do {
          ip.iPind := ip.iPind - 1;
          ip.iPcol := ip.iPcol - 1;
          term_send twid "\b"
        }
        else prev_field ip.iPcur True
    | Rt.K_Up ->
        prev_field ip.iPcur False
    | Rt.K_Right ->
        if ip.iPind < String.length ip.iPstr - 1 then do {
          ip.iPind := ip.iPind + 1;
          ip.iPcol := ip.iPcol + 1;
          term_send twid "\027[C"
        }
        else next_field ip.iPcur False
    | Rt.K_Down | Rt.K_Tab ->
        next_field ip.iPcur False
    | Rt.K_Delete -> do {
        term_send twid "\0277";
        for i = ip.iPind to String.length ip.iPstr - 1 do {
          ip.iPstr.[i] := ' ';
          term_send twid " ";
        };
        term_send twid "\0278"
      }
    | Rt.K_Shift_L | Rt.K_Shift_R | Rt.K_Control_L | Rt.K_Caps_Lock |
      Rt.K_Meta_L | Rt.K_Meta_R | Rt.K_Alt_L ->
        ()
    | Rt.K_Multi_key ->
        multi_key.val := True
    | Rt.K_Ascii c ->
        if kmod land 0x8 != 0 || last_was_multi_key then do {
          term_send twid (String.make 1 c);
          term_send twid "\b";
          accent.val := Some c
        }
        else
          let c =
            match accent.val with
            [ Some a -> do { accent.val := None; mettre_accent a c }
            | None -> c ]
          in
          if match ip.iPtyp with
             [ 's' -> True
             | 'd' -> c >= '0' && c <= '9' || c == ' '
             | 'f' -> c >= '0' && c <= '9' || c == ' ' || c == ','
             | _ -> False ]
          then do {
            ip.iPstr.[ip.iPind] := c;
            term_send twid (String.make 1 c);
            ip.iPind := ip.iPind + 1;
            ip.iPcol := ip.iPcol + 1;
            if ip.iPind == String.length ip.iPstr then
              next_field ip.iPcur False
            else ()
          }
          else ()
    | ksym -> do {
        Printf.printf "<0x%x>" (int_of_keysym ksym);
        flush Pervasives.stdout
      } ]
  }
;

value filler = (INCSZ, pack_desc [BandAtt 0] (DIRx, []) no_pack);

value place_transient wid =
  let xd = rt_xdata_of_widget wid in
  let mwid = rt_widget_named xd "Main" in
  let x = (rt_widget_width mwid - rt_widget_width wid) / 2 in
  let y = (rt_widget_height mwid - rt_widget_height wid) / 2 in
  rt_move_widget wid (rt_widget_x mwid + x) (rt_widget_y mwid + y)
;
