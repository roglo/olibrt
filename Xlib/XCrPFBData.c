/* $Id: XCrPFBData.c,v 1.1 1998/05/20 17:49:07 ddr Exp $ */

#include "stub.h"

value ML_XCreatePixmapFromBitmapData(v)
value *v;
{
	Pixmap r = XCreatePixmapFromBitmapData(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(char*) sarv(2),
		(unsigned int) iarv(3),
		(unsigned int) iarv(4),
		(unsigned long) iarv(5),
		(unsigned long) iarv(6),
		(unsigned int) iarv(7)
	);
	return MLINT(r);
}
