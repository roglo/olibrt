/* $Id: XRaiseWin.c,v 1.1 1998/05/20 17:49:28 ddr Exp $ */

#include "stub.h"

value ML_XRaiseWindow(v)
value *v;
{
	XRaiseWindow((Display *)aarv(0), (Window)iarv(1));
	return unit;
}
