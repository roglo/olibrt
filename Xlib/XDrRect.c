/* $Id: XDrRect.c,v 1.1 1998/05/20 17:49:12 ddr Exp $ */

#include "stub.h"

value ML_XDrawRectangle(v)
value *v;
{
	XDrawRectangle((Display *)aarv(0), (Drawable)iarv(1), (GC)aarv(2),
		       (int)iarv(3), (int)iarv(4), (int)iarv(5), (int)iarv(6));
	return unit;
}
