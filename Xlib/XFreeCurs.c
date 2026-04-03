/* $Id: XFreeCurs.c,v 1.1 1998/05/20 17:49:15 ddr Exp $ */

#include "stub.h"

value ML_XFreeCursor(v)
value *v;
{
	XFreeCursor((Display *)aarv(0), (Cursor)iarv(1));
	return unit;
}
