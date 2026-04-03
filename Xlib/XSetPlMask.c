/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetPlaneMask(v)
value *v;
{
	XSetPlaneMask(
		(Display*) aarv(0),
		(GC) aarv(1),
		(unsigned long) iarv(2)
	);
	return unit;
}
