/* $Id: XMapWindow.c,v 1.1 1998/05/20 17:49:22 ddr Exp $ */

#include "stub.h"

value ML_XMapWindow(v)
value *v;
{
	XMapWindow((Display *)aarv(0), (Window)iarv(1));
	return unit;
}
