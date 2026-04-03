/* $Id: XGCMisc.c,v 1.1 1998/05/20 17:49:17 ddr Exp $ */

#include "stub.h"

value ML_XSetFillStyle(v)
value *v;
{
	XSetFillStyle((Display *)aarv(0), (GC)aarv(1), (int)iarv(2));
	return unit;
}
