/* $Id: XUndefCurs.c,v 1.1 1998/05/20 17:49:39 ddr Exp $ */

#include "stub.h"

value ML_XUndefineCursor(v)
value *v;
{
	XUndefineCursor((Display *)aarv(0), (Window)iarv(1));
	return unit;
}
