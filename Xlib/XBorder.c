/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetWindowBorder(v)
value *v;
{
	XSetWindowBorder(
		(Display*) aarv(0),
		(Window) iarv(1),
		(unsigned long) iarv(2)
	);
	return unit;
}
