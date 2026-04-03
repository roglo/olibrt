/* $Id: XDrPoint.c,v 1.1 1998/05/20 17:49:12 ddr Exp $ */

#include "stub.h"

value ML_XDrawPoint(v)
value *v;
{
	XDrawPoint((Display *)aarv(0), (Drawable)iarv(1), (GC)aarv(2),
		   (int)iarv(3), (int)iarv(4));
	return unit;
}
