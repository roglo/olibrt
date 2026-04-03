/* $Id: XFreeGC.c,v 1.1 1998/05/20 17:49:16 ddr Exp $ */

#include "stub.h"

value ML_XFreeGC(v)
value *v;
{
	XFreeGC((Display *)aarv(0), (GC)aarv(1));
	return unit;
}
