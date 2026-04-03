/* $Id: XCrBFData.c,v 1.1 1998/05/20 17:49:05 ddr Exp $ */

#include "stub.h"

value ML_XCreateBitmapFromData(v)
value *v;
{
	Pixmap r = XCreateBitmapFromData(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(const char*) sarv(2),
		(unsigned int) iarv(3),
		(unsigned int) iarv(4)
	);
	return MLINT(r);
}
