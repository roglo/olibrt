/* $Id: XClear.c,v 1.1 1998/05/20 17:49:03 ddr Exp $ */

#include "stub.h"

value ML_XClearWindow(v)
value *v;
{
	XClearWindow((Display *)aarv(0), (Window)iarv(1));
	return unit;
}
