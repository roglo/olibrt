open Rt.
open Rt_main.

type rtf_data = {xdata : xdata};

value rtf_xdata xdf = xdf.xdata;

value rtf_initialize name =
  let xd = Rt.rt_initialize name in
  {xdata = xd};

