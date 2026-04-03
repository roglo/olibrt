/* $Id: XPutImage.c,v 1.1 1998/05/20 17:49:26 ddr Exp $ */

#include "stub.h"

value ML_XPutImageWithData(v)
value *v;
{
	((XImage *)aarv(3))->data = (char *) CSTRING(v[4]);
	XPutImage((Display *)aarv(0), (Drawable)iarv(1),
		  (GC)aarv(2), (XImage *)aarv(3),
		  (int)iarv(5), (int)iarv(6), (int)iarv(7), (int)iarv(8),
		  (int)iarv(9), (int)iarv(10));
	((XImage *)aarv(3))->data = 0;
	return unit;
}

value ML_XPutImage(v)
value *v;
{
	XPutImage(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(GC) aarv(2),
		(XImage*) aarv(3),
		(int) iarv(4),
		(int) iarv(5),
		(int) iarv(6),
		(int) iarv(7),
		(unsigned int) iarv(8),
		(unsigned int) iarv(9)
	);
	return unit;
}
