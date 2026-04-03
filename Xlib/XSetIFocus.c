/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetInputFocus(v)
value *v;
{
	XSetInputFocus(
		(Display*) aarv(0),
		(Window) iarv(1),
		(int) iarv(2),
		(Time) iarv(3)
	);
	return unit;
}
