/* text generated automatically by mkstub */

#include "stub.h"

value ML_XDrawArc(v)
value *v;
{
	XDrawArc(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(GC) aarv(2),
		(int) iarv(3),
		(int) iarv(4),
		(unsigned int) iarv(5),
		(unsigned int) iarv(6),
		(int) iarv(7),
		(int) iarv(8)
	);
	return unit;
}
