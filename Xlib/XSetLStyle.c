/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetLineAttributes(v)
value *v;
{
	XSetLineAttributes(
		(Display*) aarv(0),
		(GC) aarv(1),
		(unsigned int) iarv(2),
		(int) iarv(3),
		(int) iarv(4),
		(int) iarv(5)
	);
	return unit;
}

value ML_XSetDashes(v)
value *v;
{
        XSetDashes(
		(Display*) aarv(0),
		(GC) aarv(1),
		(int) iarv(2),
		(char *) sarv(3),
		(int) iarv(4)
	);
        return unit;
}
