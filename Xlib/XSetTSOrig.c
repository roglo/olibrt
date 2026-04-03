/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetTSOrigin(v)
value *v;
{
	XSetTSOrigin(
		(Display*) aarv(0),
		(GC) aarv(1),
		(int) iarv(2),
		(int) iarv(3)
	);
	return unit;
}
