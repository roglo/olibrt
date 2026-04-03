/* $Id: XCrWindow.c,v 1.1 1998/05/20 17:49:08 ddr Exp $ */

#include "stub.h"

value ML_XCreateSimpleWindow(v)
value *v;
{
	Window r = XCreateSimpleWindow((Display *)aarv(0),
				       (Window)iarv(1),
				       (int)iarv(2), (int)iarv(3),
				       (unsigned int)iarv(4),
				       (unsigned int)iarv(5),
				       (unsigned int)iarv(6),
				       (unsigned long)iarv(7),
				       (unsigned long)iarv(8));
	return MLINT(r);
}
