/* $Id: XClDisplay.c,v 1.1 1998/05/20 17:49:03 ddr Exp $ */

#include "stub.h"

value ML_XCloseDisplay(v)
value v;
{
	XCloseDisplay((Display *)aar());
	return unit;
}
