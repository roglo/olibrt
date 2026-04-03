/* text generated automatically by mkstub */

#include "stub.h"

value ML_XChangeKeyboardControl(v)
value *v;
{
	XChangeKeyboardControl(
		(Display*) aarv(0),
		(unsigned long) iarv(1),
		(XKeyboardControl*) aarv(2)
	);
	return unit;
}
