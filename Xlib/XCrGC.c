/* text generated automatically by mkstub */

#include "stub.h"

value ML_XCreateGC(v)
value *v;
{
	GC r = XCreateGC(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(unsigned long) iarv(2),
		(XGCValues*) aarv(3)
	);
	return Val_addr(r);
}

value ML_XGContextFromGC(v)
value v;
{
	GContext r = XGContextFromGC(
		(GC) aar()
	);
	return Val_long(r);
}
