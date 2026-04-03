#include "stub.h"

value ML_XInternAtom(v)
value *v;
{
  Atom r = XInternAtom((Display*) aarv(0),
                       (const char *) sarv(1),
                       (Bool) iarv(2));
  return Val_long(r);
}
