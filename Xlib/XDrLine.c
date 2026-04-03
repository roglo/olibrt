/* $Id: XDrLine.c,v 1.1 1998/05/20 17:49:11 ddr Exp $ */

#include "stub.h"

value ML_XDrawLine(v)
value *v;
{
	XDrawLine((Display *)aarv(0), (Drawable)iarv(1), (GC)aarv(2),
		  (int)iarv(3), (int)iarv(4), (int)iarv(5), (int)iarv(6));
	return unit;
}
