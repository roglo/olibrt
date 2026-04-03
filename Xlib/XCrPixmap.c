/* $Id: XCrPixmap.c,v 1.1 1998/05/20 17:49:08 ddr Exp $ */

#include "stub.h"

value ML_XCreatePixmap(v)
value *v;
{
	Pixmap r = XCreatePixmap((Display *)aarv(0), (Drawable)iarv(1),
				 (int)iarv(2), (int)iarv(3), (int)iarv(4));
	return MLINT(r);
}
