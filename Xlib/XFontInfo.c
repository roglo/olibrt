/* text generated automatically by mkstub */

#include "stub.h"

value ML_XFreeFontInfoSpec(v)
value v;
{
	XFreeFontInfo(
		(char**) 0,
		(XFontStruct*) aar(),
		(int) 1
	);
	return unit;
}
