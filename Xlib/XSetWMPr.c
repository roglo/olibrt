/* $Id: XSetWMPr.c,v 1.1 2006/05/08 19:48:38 deraugla Exp $ */

#include "stub.h"

value ML_XSetWMProtocols(v)
value *v;
{
  Status r = XSetWMProtocols((Display*) aarv(0),
                             (Window) iarv(1),
                             (Atom*) aarv(2),
                             (int) iarv(3));
  return Val_long(r);
}
