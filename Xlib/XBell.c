/* text generated automatically by mkstub */

#include "stub.h"

value ML_XBell(v)
value *v;
{
	XBell(
		(Display*) aarv(0),
		(int) iarv(1)
	);
	return unit;
}
