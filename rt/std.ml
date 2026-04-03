(* $Id: std.ml,v 1.5 2008/01/23 10:41:08 deraugla Exp $ *)

value hash = Hashtbl.hash;

value except_assoc e =
  except_e where rec except_e =
    fun
    [ [] -> []
    | [((x, _) as y) :: l] -> if x = e then l else [y :: except_e l] ]
;
value hash_add_assoc ((key, val0) as pair) v =
  let i = hash key mod Array.length v in v.(i) := [pair :: v.(i)]
and hash_remove_assoc key v =
  let i = hash key mod Array.length v in v.(i) := except_assoc key v.(i)
and hash_assoc key v =
  match v.(hash key mod Array.length v) with
  [ [] -> failwith "find"
  | [(key1, val1)] -> if key = key1 then val1 else failwith "find"
  | [(key1, val1); (key2, val2)] ->
      if key = key1 then val1
      else if key = key2 then val2
      else failwith "find"
  | l -> List.assoc key l ]
;

value add_setq x s = if List.memq x s then s else [x :: s];
exception Identity;
value share f x = try f x with [ Identity -> x ];
value filter_neg p =
  let rec filter_aux =
    fun
    [ [] -> raise Identity
    | [x :: l] -> if p x then share filter_aux l else [x :: filter_aux l] ]
  in
  share filter_aux
;
value rec item p0 p1 =
  match (p0, p1) with
  [ ([a :: l], i) -> if i = 0 then a else item l (i - 1)
  | (_, _) -> failwith "item" ]
;
value do_list_i f =
  do_list_f where rec do_list_f i =
    fun
    [ [] -> ()
    | [x :: l] -> do { f i x; do_list_f (succ i) l } ]
;
