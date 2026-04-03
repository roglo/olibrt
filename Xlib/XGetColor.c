/* text generated automatically by mkstub */

#include "stub.h"

value ML_XAllocNamedColor(v)
value *v;
{
	Status r = XAllocNamedColor(
		(Display*) aarv(0),
		(Colormap) iarv(1),
		(const char*) sarv(2),
		(XColor*) aarv(3),
		(XColor*) aarv(4)
	);
	return Val_long(r);
}
