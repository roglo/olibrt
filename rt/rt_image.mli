(* $Id: rt_image.mli,v 1.1 2008/12/29 10:46:57 deraugla Exp $ *)

open Rtdecl;

type image = 'aa';
value rt_create_image : xdata -> string -> int -> int -> int -> image;
value rt_put_image :
  drawable -> image -> (int * int * int * int) -> (int * int) -> unit;
value rt_get_image :
  drawable -> image -> (int * int * int * int) -> (int * int) -> unit;
value rt_get_pixel : image -> (int * int) -> int;
value rt_put_pixel : image -> (int * int * int) -> unit;

value rt_image_data_len : xdata -> int -> int -> int -> int;
