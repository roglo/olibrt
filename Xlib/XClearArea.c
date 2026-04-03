/* $Id: XClearArea.c,v 1.1 1998/05/20 17:49:04 ddr Exp $ */

#include "stub.h"

value ML_XClearArea(v)
value *v;
{
	XClearArea((Display *)aarv(0), (Window)iarv(1),
		   (int)iarv(2), (int)iarv(3),
		   (int)iarv(4), (int)iarv(5), (Bool)iarv(6));
	return unit;
}
