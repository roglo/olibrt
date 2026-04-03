/* $Id: XConfWind.c,v 1.1 1998/05/20 17:49:04 ddr Exp $ */

#include "stub.h"

value ML_XMoveResizeWindow(v)
value *v;
{
	XMoveResizeWindow((Display *)aarv(0), (Window)iarv(1),
			  (int)iarv(2), (int)iarv(3),
			  (int)iarv(4), (int)iarv(5));
	return unit;
}
