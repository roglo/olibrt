/* $Id: XDefCursor.c,v 1.1 1998/05/20 17:49:09 ddr Exp $ */

#include "stub.h"

value ML_XDefineCursor(v)
value *v;
{
	XDefineCursor((Display *)aarv(0), (Window)iarv(1), (Cursor)iarv(2));
	return unit;
}
