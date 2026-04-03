/* text generated automatically by mkstub */

#include "stub.h"

value ML_XAllocColor(v)
value *v;
{
	Status r = XAllocColor(
		(Display*) aarv(0),
		(Colormap) iarv(1),
		(XColor*) aarv(2)
	);
	return Val_long(r);
}
