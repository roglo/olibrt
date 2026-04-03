(* $Id: motif.ml,v 1.8 2017/05/14 01:11:49 deraugla Exp $ *)

open Rtdecl;
open Xlib;

type motif_border = [ MBpush | MBflat | MBpop ];
type motif_dir = [ MDup | MDdown | MDleft | MDright ];

value make_array len f = do {
  let v = Array.make len (Obj.magic 0) in
  for i = 0 to len - 1 do { v.(i) := f i };
  v
};

value pts = alloc_XPoint 6;
value cpts = make_array 6 (fun _ -> (0, 0));

value motif_border xd win x y width height mb = do {
  let lwid = xd.motif_border in cpts.(0) := (x, y);
  cpts.(1) := (x + width, y);
  cpts.(2) := (x + width - lwid, y + lwid);
  cpts.(3) := (x + lwid, y + lwid);
  cpts.(4) := (x + lwid, y + height - lwid);
  cpts.(5) := (x, y + height);
  for i = 0 to 5 do {
    let (x, y) = cpts.(i) in set_XPoint_x (x, pts, i);
    set_XPoint_y (y, pts, i);
  };
  xSetForeground
    (xd.dpy, xd.gc,
     match mb with
     [ MBpush -> xd.bord_pix.(1)
     | MBflat -> xd.backg_pix
     | MBpop -> xd.bord_pix.(0) ]);
  xFillPolygon (xd.dpy, win, xd.gc, pts, 6, nonconvex, coordModeOrigin);
  for i = 0 to 5 do {
    let (px, py) = cpts.(i) in
    let px = 2 * x + width - px in
    let py = 2 * y + height - py in set_XPoint_x (px, pts, i);
    set_XPoint_y (py, pts, i);
  };
  xSetForeground
    (xd.dpy, xd.gc,
     match mb with
     [ MBpush -> xd.bord_pix.(0)
     | MBflat -> xd.backg_pix
     | MBpop -> xd.bord_pix.(1) ]);
  xFillPolygon (xd.dpy, win, xd.gc, pts, 6, nonconvex, coordModeOrigin);
  xSetForeground (xd.dpy, xd.gc, xd.black)
};

value motif_backg xd win =
  if List.mem (visual_class xd.vis) [pseudoColor; trueColor] then
    xSetWindowBackground (xd.dpy, win, xd.backg_pix)
  else xSetWindowBackgroundPixmap (xd.dpy, win, xd.gray_pixm)
;

value motif_arrow xd win x y width height adir mb = do {
  let lwid = xd.motif_border in
  cpts.(0) := (x + lwid / 2, y + height - 1 - lwid / 2);
  cpts.(1) := (x + (width - 1) / 2, y + lwid / 2);
  cpts.(2) := (x + width - 1 - lwid / 2, y + height - 1 - lwid / 2);
  let inx =
    match adir with
    [ MDup -> do {
        cpts.(0) := (x + lwid / 2, y + height - 1 - lwid / 2);
        cpts.(1) := (x + (width - 1) / 2, y + lwid / 2);
        cpts.(2) := (x + width - 1 - lwid / 2, y + height - 1 - lwid / 2);
        0
      }
    | MDdown -> do {
        cpts.(0) := (x + width - 1 - lwid / 2, y + lwid / 2);
        cpts.(1) := (x + (width - 1) / 2, y + height - 1 - lwid / 2);
        cpts.(2) := (x + lwid / 2, y + lwid / 2);
        1
      }
    | MDleft -> do {
        cpts.(0) := (x + width - 1 - lwid / 2, y + lwid / 2);
        cpts.(1) := (x + lwid / 2, y + (height - 1) / 2);
        cpts.(2) := (x + width - 1 - lwid / 2, y + height - 1 - lwid / 2);
        0
      }
    | MDright -> do {
        cpts.(0) := (x + lwid / 2, y + height - 1 - lwid / 2);
        cpts.(1) := (x + width - 1 - lwid / 2, y + (height - 1) / 2);
        cpts.(2) := (x + lwid / 2, y + lwid / 2);
        1
      } ]
  in
  xSetLineAttributes (xd.dpy, xd.gc, lwid, lineSolid, capRound, joinRound);
  for i = 0 to 2 do {
    let (lx, ly) = cpts.((i + 1) mod 3) in set_XPoint_x (lx, pts, i);
    set_XPoint_y (ly, pts, i);
  };
  xSetForeground
    (xd.dpy, xd.gc,
     match mb with
     [ MBpush -> xd.bord_pix.(inx)
     | MBflat -> xd.backg_pix
     | MBpop -> xd.bord_pix.(1-inx) ]);
  xDrawLines (xd.dpy, win, xd.gc, pts, 3, coordModeOrigin);
  for i = 0 to 1 do {
    let (lx, ly) = cpts.(i) in set_XPoint_x (lx, pts, i);
    set_XPoint_y (ly, pts, i);
  };
  xSetForeground
    (xd.dpy, xd.gc,
     match mb with
     [ MBpush -> xd.bord_pix.(1-inx)
     | MBflat -> xd.backg_pix
     | MBpop -> xd.bord_pix.(inx) ]);
  xDrawLines (xd.dpy, win, xd.gc, pts, 2, coordModeOrigin);
  xSetLineAttributes (xd.dpy, xd.gc, 1, lineSolid, capButt, joinMiter);
  xSetForeground (xd.dpy, xd.gc, xd.black)
};

