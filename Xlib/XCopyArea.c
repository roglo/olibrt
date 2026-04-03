/* $Id: XCopyArea.c,v 1.1 1998/05/20 17:49:04 ddr Exp $ */

#include "stub.h"

value ML_XCopyArea(v)
value *v;
{
	XCopyArea((Display *)aarv(0),
		  (Drawable)iarv(1), (Drawable)iarv(2),
		  (GC)aarv(3), (int)iarv(4), (int)iarv(5),
		  (int)iarv(6), (int)iarv(7),
		  (int)iarv(8), (int)iarv(9));
	return unit;
}
