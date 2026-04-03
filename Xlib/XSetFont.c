/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetFont(v)
value *v;
{
	XSetFont(
		(Display*) aarv(0),
		(GC) aarv(1),
		(Font) iarv(2)
	);
	return unit;
}
