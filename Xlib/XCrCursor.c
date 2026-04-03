/* $Id: XCrCursor.c,v 1.2 2008/12/11 14:11:33 deraugla Exp $ */

#include "stub.h"

value ML_XCreatePixmapCursor(v)
value *v;
{
	Cursor r = XCreatePixmapCursor((Display *)aarv(0),
				       (Pixmap)iarv(1), (Pixmap)iarv(2),
				       (XColor *)aarv(3), (XColor *)aarv(4),
				       (unsigned int)iarv(5),
				       (unsigned int)iarv(6));	
	return MLINT(r);
}
