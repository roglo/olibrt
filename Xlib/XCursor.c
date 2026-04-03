/* $Id: XCursor.c,v 1.1 1998/05/20 17:49:09 ddr Exp $ */

#include "stub.h"

value ML_XCreateFontCursor(v)
value *v;
{
	Cursor r = XCreateFontCursor((Display *)aarv(0),
				     (unsigned int)iarv(1));
	return MLINT(r);
}
