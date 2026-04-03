/* text generated automatically by mkstub */

#include "stub.h"

value ML_XGetKeyboardControl(v)
value *v;
{
	XGetKeyboardControl(
		(Display*) aarv(0),
		(XKeyboardState*) aarv(1)
	);
	return unit;
}
