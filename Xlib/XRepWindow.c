/* text generated automatically by mkstub */

#include "stub.h"

value ML_XReparentWindow(v)
value *v;
{
	XReparentWindow(
		(Display*) aarv(0),
		(Window) iarv(1),
		(Window) iarv(2),
		(int) iarv(3),
		(int) iarv(4)
	);
	return unit;
}
