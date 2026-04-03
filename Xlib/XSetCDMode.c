#include "stub.h"

value ML_XSetCloseDownMode(v)
value *v;
{
  int r = XSetCloseDownMode((Display*) aarv(0),
                            (int) iarv(1));
  return Val_long(r);
}
