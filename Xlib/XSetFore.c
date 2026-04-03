/* $Id: XSetFore.c,v 1.1 1998/05/20 17:49:31 ddr Exp $ */

#include "stub.h"

value ML_XSetForeground(v)
value *v;
{
	XSetForeground((Display *)aarv(0), (GC)aarv(1), (long)iarv(2));
	return unit;
}
