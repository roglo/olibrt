/* text generated automatically by mkstub */

#include "stub.h"

value ML_XQueryColor(v)
value *v;
{
	XQueryColor(
		(Display*) aarv(0),
		(Colormap) iarv(1),
		(XColor*) aarv(2)
	);
	return unit;
}
