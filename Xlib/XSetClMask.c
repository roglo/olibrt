/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetClipMask(v)
value *v;
{
	XSetClipMask(
		(Display*) aarv(0),
		(GC) aarv(1),
		(Pixmap) iarv(2)
	);
	return unit;
}
