/* $Id: XMoveWin.c,v 1.1 1998/05/20 17:49:23 ddr Exp $ */

#include "stub.h"

value ML_XMoveWindow(v)
value *v;
{
	XMoveWindow((Display *)aarv(0), (Window)iarv(1),
		    (int)iarv(2), (int)iarv(3));
	return unit;
}
