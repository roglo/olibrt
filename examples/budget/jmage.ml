(* $Id: jmage.ml,v 1.4 2006/06/02 06:58:16 deraugla Exp $ *)

open RtN;

type inputPic =
  { iParr : array (bool * string * int * int * char * int * String.t * widget);
    iPcur : mutable int;
    iPlin : mutable int;
    iPcol : mutable int;
    iPtyp : mutable char;
    iPdec : mutable int;
    iPstr : mutable String.t;
    iPind : mutable int;
    iPwid : mutable widget }
;

type field =
  [ Fint of int
  | Fdecimal of (int * int * int)
  | Fstring of string
  | Fempty ]
;

(* $Id: jmage.ml,v 1.4 2006/06/02 06:58:16 deraugla Exp $ *)

value rec get_format_len pic len i =
  if i >= String.length pic then invalid_arg "unexpected end of picture"
  else
    match pic.[i] with
    [ '0'..'9' ->
        get_format_len pic (10 * len + Char.code pic.[i] - Char.code '0')
          (succ i)
    | '.' ->
        let (dlen, i) = get_decimal_len pic 0 (succ i) in
        (len, dlen, i)
    | _ ->
        (len, 0, i) ]
and get_decimal_len pic len i =
  if i >= String.length pic then invalid_arg "unexpected end of picture"
  else
    match pic.[i] with
    [ '0'..'9' ->
        get_decimal_len pic (10 * len + Char.code pic.[i] - Char.code '0')
          (succ i)
    | _ -> (len, i) ]
;

value empty_pic pic =
  make "" 0 where rec make str i =
    if i >= String.length pic then str
    else if pic.[i] == '%' then
      let (len, _, i) = get_format_len pic 0 (succ i) in
      make (if len == 0 then str else str ^ String.make len ' ') (succ i)
    else make (str ^ String.make 1 pic.[i]) (succ i)
;

open RtN;

value term_goto twid lin col = do {
  term_send twid "\027[";
  term_send twid (string_of_int (succ lin));
  term_send twid ";";
  term_send twid (string_of_int (succ col));
  term_send twid "H"
};

value input_of_pic wid pic =
  let rec input_list lin col i =
    if i >= String.length pic then []
    else if pic.[i] == '%' then
      let (len, dlen, i2) = get_format_len pic 0 (succ i) in
      let fmt = String.sub pic i (i2 - i) in
      let x =
        (False, fmt, lin, col, pic.[i2], dlen, String.make len ' ', wid)
      in
      [x :: input_list lin (col + len) (succ i2)]
    else if pic.[i] == '\n' then input_list (succ lin) 0 (succ i)
    else input_list lin (succ col) (succ i)
  in
  let l = input_list 0 0 0 in
  let a = Array.of_list l in
  let (_, _, lin, col, typ, dec, str, _) = a.(0) in
  {iParr = a; iPcur = 0; iPlin = lin; iPcol = col; iPtyp = typ; iPdec = dec;
   iPstr = str; iPind = 0; iPwid = wid}
;

value strip_trailing_spaces (str : String.t) =
  let end_ =
    start_spaces (String.length str) where rec start_spaces n =
      if n == 0 then 0
      else if str.[n - 1] == ' ' then start_spaces (pred n)
      else n
  in
  String.sub str 0 end_
;

value goto_field ip i eof = do {
  ip.iPcur := i;
  let (_, _, lin, col, typ, dec, str, wid) = ip.iParr.(ip.iPcur) in
  term_send ip.iPwid "\027[?35h";
  ip.iPstr := str;
  ip.iPind :=
    if eof then max 0 (String.length (strip_trailing_spaces ip.iPstr) - 1)
    else 0;
  ip.iPlin := lin;
  ip.iPcol := col + ip.iPind;
  ip.iPtyp := typ;
  ip.iPdec := dec;
  ip.iPwid := wid;
  term_send ip.iPwid "\027[?35l";
  term_goto ip.iPwid ip.iPlin ip.iPcol
};

value lock_field ip n =
  let (_, fmt, lin, col, typ, dec, str, wid) = ip.iParr.(n) in
  ip.iParr.(n) := (True, fmt, lin, col, typ, dec, str, wid)
;

value int_of_field str =
  int 0 (Stream.of_string str) where rec int mant =
    parser
    [ [: `' '; a = int mant :] -> a
    | [: `('0'..'9' as c);
         a = int (10 * mant + Char.code c - Char.code '0') :] -> a
    | [: :] -> mant ]
;

value decimal_of_field str =
  let rec exp d e =
    parser
    [ [: `' '; a = exp d e :] -> a
    | [: `('0'..'9' as c);
         a = exp (10 * d + Char.code c - Char.code '0') (succ e) :] -> a
    | [: :] -> (d, e) ]
  in
  decimal 0 (Stream.of_string str) where rec decimal mant =
    parser
    [ [: `' '; a = decimal mant :] -> a
    | [: `('0'..'9' as c);
         a = decimal (10 * mant + Char.code c - Char.code '0') :] -> a
    | [: `','; (r, e) = exp 0 0 :] -> (mant, r, e)
    | [: :] -> (mant, 0, 0) ]
;

value get_field ip n =
  let (_, _, _, _, typ, dec, str, _) = ip.iParr.(n) in
  match strip_trailing_spaces str with
  [ "" -> Fempty
  | str ->
      match typ with
      [ 's' -> Fstring (strip_trailing_spaces str)
      | 'd' -> Fint (int_of_field str)
      | 'f' -> Fdecimal (decimal_of_field str)
      | _ -> invalid_arg "bad field type" ] ]
;

value saved_blit_string src bsrc dst bdst len =
  try String.blit src bsrc dst bdst len with _ ->
    String.blit (String.make (String.length dst) '*') 0
      dst 0 (String.length dst)
;

value format_field fmt typ dec f dst =
  match typ with
  [ 's' ->
      match f with
      [ Fstring src -> saved_blit_string src 0 dst 0 (String.length src)
      | _ -> () ]
  | 'd' ->
      match f with
      [ Fint n -> do {
          let gap =
            if String.length fmt > 1 && fmt.[1] == '0' then '0' else ' '
          in
          let src = string_of_int n in
          let start = String.length dst - String.length src in
          for i = 0 to start - 1 do { dst.[i] := gap };
          saved_blit_string src 0 dst start (String.length src)
        }
      | _ -> () ]
  | 'f' ->
      match f with
      [ Fdecimal (m, d, e) -> do {
          let d =
            normalize (dec - e) d where rec normalize sh d =
              if sh < 0 then normalize (succ sh) (d / 10)
              else if sh > 0 then normalize (pred sh) (d * 10)
              else d
          in
          let src = string_of_int m ^ "," ^ Printf.sprintf "%02d" d in
          let start = String.length dst - String.length src in
          for i = 0 to start - 1 do { dst.[i] := ' ' };
          saved_blit_string src 0 dst start (String.length src)
        }
      | _ -> () ]
  | _ ->
      failwith "internal error in format_field" ]
;

value set_field ip n f = do {
  let (_, fmt, lin, col, typ, dec, dst, twid) = ip.iParr.(n) in
  format_field fmt typ dec f dst;
  term_goto twid lin col;
  term_send twid dst
};

value get_string =
  fun
  [ Fstring str -> str
  | _ -> "" ]
;
value get_int =
  fun
  [ Fint i -> i
  | _ -> 0 ]
;

value string_of_somme len s =
  let str =
    string_of_int (abs s / 100) ^ "," ^ Printf.sprintf "%02d" (abs s mod 100)
  in
  let str = if s < 0 then "-" ^ str else str in
  if String.length str > len then String.make len '#'
  else if String.length str == len then str
  else String.make (len - String.length str) ' ' ^ str
;

value string_of_mois =
  fun
  [ 1 -> "janvier"
  | 2 -> "février"
  | 3 -> "mars"
  | 4 -> "avril"
  | 5 -> "mai"
  | 6 -> "juin"
  | 7 -> "juillet"
  | 8 -> "août"
  | 9 -> "septembre"
  | 10 -> "octobre"
  | 11 -> "novembre"
  | 12 -> "décembre"
  | _ -> invalid_arg "string_of_mois" ]
;

value string_of_date j m a = do {
  let strj = String.make 2 ' ' in
  format_field "%02d" 'd' 0 (Fint j) strj;
  let strm = String.make 2 ' ' in
  format_field "%02d" 'd' 0 (Fint m) strm;
  let stra = String.make 4 ' ' in
  format_field "%4d" 'd' 0 (Fint a) stra;
  strj ^ "/" ^ strm ^ "/" ^ stra
};

value mois_annee_large m a = do {
  let m = string_of_mois m in
  let a = Printf.sprintf "%04d" a in
  let s = Bytes.create (2 * (String.length m + 1 + String.length a)) in
  for i = 0 to String.length m - 1 do {
    s.[2 * i] := Char.chr (Char.code m.[i] + Char.code 'A' - Char.code 'a');
    s.[2 * i + 1] := ' ';
  };
  s.[2 * String.length m] := ' ';
  s.[2 * String.length m + 1] := ' ';
  for i = 0 to String.length a - 1 do {
    s.[2 * String.length m + 2 + 2 * i] := a.[i];
    s.[2 * String.length m + 2 + 2 * i + 1] := ' ';
  };
  "- " ^ s ^ "-"
};

value capitalize str =
  match str.[0] with
  [ 'a'..'z' as c -> do {
      let str2 = Bytes.create (String.length str) in
      String.blit str 0 str2 0 (String.length str);
      str2.[0] := Char.chr (Char.code c - Char.code 'a' + Char.code 'A');
      Bytes.to_string str2
    }
  | _ -> str ]
;

value large str = do {
  let str2 = String.make (2 * String.length str - 1) ' ' in
  for i = 0 to String.length str - 1 do { str2.[2 * i] := str.[i] };
  Bytes.to_string str2
};
