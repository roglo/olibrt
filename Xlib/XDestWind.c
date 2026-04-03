/* $Id: XDestWind.c,v 1.1 1998/05/20 17:49:09 ddr Exp $ */

#include "stub.h"

value ML_XDestroyWindow(v)
value *v;
{
	XDestroyWindow((Display *)aarv(0), (Window)iarv(1));
	return unit;
}
