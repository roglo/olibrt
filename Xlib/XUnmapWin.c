/* $Id: XUnmapWin.c,v 1.1 1998/05/20 17:49:39 ddr Exp $ */

#include "stub.h"

value ML_XUnmapWindow(v)
value *v;
{
	XUnmapWindow((Display *)aarv(0), (Window)iarv(1));
	return unit;
}
