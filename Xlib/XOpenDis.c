#include "stub.h"

value ML_XOpenDisplay(v)
value v;
{
	Display * r = XOpenDisplay(
		(const char*) sar()
	);
	return MLADDR(r);
}
