/* $Id: XFillPoly.c,v 1.1 1998/05/20 17:49:13 ddr Exp $ */

#include "stub.h"

value ML_XFillPolygon(v)
value *v;
{
	XFillPolygon((Display *)aarv(0), (Drawable)iarv(1), (GC)aarv(2),
		     (XPoint *)aarv(3), (int)iarv(4), (int)iarv(5),
		     (int)iarv(6));
	return unit;
}
