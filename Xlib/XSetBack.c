/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetBackground(v)
value *v;
{
	XSetBackground(
		(Display*) aarv(0),
		(GC) aarv(1),
		(unsigned long) iarv(2)
	);
	return unit;
}
