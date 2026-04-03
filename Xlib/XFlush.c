/* text generated automatically by mkstub */

#include "stub.h"

value ML_XFlush(v)
value v;
{
	XFlush(
		(Display*) aar()
	);
	return unit;
}
