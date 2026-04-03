(* $Id: d_calculat.ml,v 1.7 2006/06/02 06:58:16 deraugla Exp $ *)

open RtN;

value _MAX = 100000000;

type accu = { neg : mutable bool; mant : mutable int; exp : mutable int };
type etat = [ Nouveau | PartieEntiere | PartieDecimale | Erreur of string ];

value accu = {neg = False; mant = 0; exp = 1};
value pile = {neg = False; mant = 0; exp = 1};
value oper = ref ' ';
value etat = ref PartieEntiere;

value afficher_accu aff = do {
  term_send aff "\027[H\027[2J\027[?35h";
  match etat.val with
  [ Erreur s -> term_send aff s
  | _ -> do {
      if accu.neg then term_send aff "-" else ();
      term_send aff (string_of_int (accu.mant / accu.exp));
      if accu.exp != 1 then do {
        term_send aff ",";
        let n = accu.mant mod accu.exp in
        let n =
          match accu.exp with
          [ 10 -> n * 10
          | 100 -> n
          | _ -> 0 ]
        in
        term_send aff (Printf.sprintf "%02d" n)
      }
      else ()
    } ]
};

value empiler () = do {
  pile.neg := accu.neg;
  pile.mant := accu.mant;
  pile.exp := accu.exp
};

value float_of_valeur a =
  if a.neg then -. float a.mant /. float a.exp
  else float a.mant /. float a.exp
;

value normaliser a =
  if a.exp == 0 then failwith ("normaliser " ^ string_of_int a.mant)
  else do {
    while a.exp > 1 && a.mant mod 10 == 0 do {
      a.exp := a.exp / 10;
      a.mant := a.mant / 10;
    };
    if a.mant == 0 then a.neg := False else ()
  }
;

value valeur_of_float f v =
  let neg = f < 0.0 in
  let f = abs_float f in
  if f >= float _MAX then etat.val := Erreur "Dépassement"
  else if f = 0.0 then do { v.neg := False; v.mant := 0; v.exp := 1 }
  else do {
    let (mant, exp) =
      glop f 1 where rec glop f exp =
        if f *. 10.0 >= float _MAX then (truncate (f +. 0.5), exp)
        else glop (f *. 10.0) (exp * 10)
    in
    v.neg := neg;
    v.mant := mant;
    v.exp := exp;
    normaliser v
  }
;

value appliquer_oper aff =
  match oper.val with
  [ '+' ->
      valeur_of_float (float_of_valeur pile +. float_of_valeur accu) accu
  | '-' ->
      valeur_of_float (float_of_valeur pile -. float_of_valeur accu) accu
  | '*' | 'x' | 'X' ->
      valeur_of_float (float_of_valeur pile *. float_of_valeur accu) accu
  | '/' ->
      if accu.mant == 0 then etat.val := Erreur "Div par 0"
      else
        valeur_of_float (float_of_valeur pile /. float_of_valeur accu) accu
  | _ ->
      () ]
;

value effacer_accu () = do {
  accu.neg := False; accu.mant := 0; accu.exp := 1
};

value action wid = do {
  let xd = rt_xdata_of_widget wid in
  let aff = rt_widget_named xd "CALC affichage" in
  rt_map_widget (rt_widget_named xd "CALCULATRICE");
  afficher_accu aff
};

value effacement_paresseux () =
  match etat.val with
  [ Nouveau -> do { etat.val := PartieEntiere; effacer_accu () }
  | _ -> () ]
;

value touche wid kmod ksym =
  let xd = rt_xdata_of_widget wid in
  let aff = rt_widget_named xd "CALC affichage" in
  match
    match ksym with
    [ Rt.K_Return -> '='
    | Rt.K_Left | Rt.K_BackSpace -> 'e'
    | Rt.K_Ascii x -> x
    | _ -> ' ' ]
  with
  [ 'C' | 'c' -> do {
      etat.val := PartieEntiere;
      effacer_accu ();
      oper.val := ' ';
      afficher_accu aff
    }
  | _
      when
        match etat.val with
        [ Erreur _ -> True
        | _ -> False ] ->
      ()
  | ' ' ->
      ()
  | 'E' | 'e' -> do {
      effacement_paresseux ();
      match etat.val with
      [ PartieDecimale ->
          if accu.exp == 1 then etat.val := PartieEntiere else ()
      | _ -> () ];
      accu.mant := accu.mant / 10;
      if accu.exp > 1 then accu.exp := accu.exp / 10 else ();
      afficher_accu aff
    }
  | '0'..'9' as c -> do {
      let n = Char.code c - Char.code '0' in
      effacement_paresseux ();
      if accu.mant < _MAX / 10 then do {
        accu.mant := 10 * accu.mant + n;
        match etat.val with
        [ PartieDecimale -> accu.exp := 10 * accu.exp
        | _ -> () ]
      }
      else ();
      afficher_accu aff
    }
  | ',' -> do {
      effacement_paresseux ();
      afficher_accu aff;
      if accu.exp == 1 then do {
        etat.val := PartieDecimale; term_send aff ","
      }
      else ()
    }
  | '+' | '-' | 'x' | 'X' | '*' | '/' | '=' as c -> do {
      etat.val := Nouveau;
      appliquer_oper aff;
      oper.val := c;
      empiler ();
      afficher_accu aff;
      match c with
      [ '=' -> ()
      | _ -> term_send aff (String.make 1 c) ]
    }
  | 's' | 'S' -> do {
      accu.neg := not accu.neg;
      afficher_accu aff
    }
  | _ ->
      () ]
;

value action_off wid =
  fun
  [ ButtonEvRelease | ButtonEvShortcut ->
      let xd = rt_xdata_of_widget wid in
      rt_unmap_widget (rt_widget_named xd "CALCULATRICE")
  | _ -> () ]
;

value action_affichage wid =
  fun
  [ TermEvKeyPress kmod ksym -> touche wid kmod ksym
  | _ -> () ]
;

value action_no_pack wid =
  fun [ PackEvKeyPress kmod ksym -> touche wid kmod ksym ]
;
value action_button a wid =
  fun
  [ ButtonEvPress _ _ -> touche wid 0 (Rt.K_Ascii a)
  | ButtonEvShortcut -> touche wid 0 (Rt.K_Ascii a)
  | _ -> () ]
;
value action_no_button _ _ = ();
  
value filler = (INCSZ, pack_desc [BandAtt 0] (DIRx, []) action_no_pack);

value taille = [WidthAtt 25; HeightAtt 25];

value wdesc =
  pack_desc [NameAtt "CALCULATRICE"]
    (DIRy,
     [(FIXSZ, term_desc [NameAtt "CALC affichage"] (1, 11) action_affichage);
      (INCSZ,
       pack_desc []
         (DIRx,
          [(FIXSZ, button_desc taille ("Off", None) action_off); filler])
         action_no_pack);
      (INCSZ,
       pack_desc []
         (DIRx,
          [(INCSZ, button_desc taille ("C", None) (action_button 'c'));
           (INCSZ, button_desc taille ("E", None) (action_button 'e'));
           (INCSZ, button_desc taille (" ", None) action_no_button);
           (INCSZ, button_desc taille ("/", None) (action_button '/'))])
         action_no_pack);
      (INCSZ,
       pack_desc []
         (DIRx,
          [(INCSZ, button_desc taille ("7", None) (action_button '7'));
           (INCSZ, button_desc taille ("8", None) (action_button '8'));
           (INCSZ, button_desc taille ("9", None) (action_button '9'));
           (INCSZ, button_desc taille ("x", None) (action_button 'x'))])
         action_no_pack);
      (INCSZ,
       pack_desc []
         (DIRx,
          [(INCSZ, button_desc taille ("4", None) (action_button '4'));
           (INCSZ, button_desc taille ("5", None) (action_button '5'));
           (INCSZ, button_desc taille ("6", None) (action_button '6'));
           (INCSZ, button_desc taille ("-", None) (action_button '-'))])
         action_no_pack);
      (INCSZ,
       pack_desc []
         (DIRx,
          [(INCSZ, button_desc taille ("1", None) (action_button '1'));
           (INCSZ, button_desc taille ("2", None) (action_button '2'));
           (INCSZ, button_desc taille ("3", None) (action_button '3'));
           (INCSZ, button_desc taille ("+", None) (action_button '+'))])
         action_no_pack);
      (INCSZ,
       pack_desc []
         (DIRx,
          [(INCSZ, button_desc taille ("0", None) (action_button '0'));
           (INCSZ, button_desc taille (",", None) (action_button ','));
           (INCSZ, button_desc taille ("S", None) (action_button 'S'));
           (INCSZ, button_desc taille ("=", None) (action_button '='))])
         action_no_pack)])
    action_no_pack
;
