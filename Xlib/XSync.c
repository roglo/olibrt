#include "stub.h"

value ML_XSync(v)
value *v;
{
	XSync(
		(Display*) aarv(0),
		(Bool) iarv(1)
	);
	return unit;
}
