/* $Id: XSetStip.c,v 1.1 1998/05/20 17:49:34 ddr Exp $ */

#include "stub.h"

value ML_XSetStipple(v)
value *v;
{
	XSetStipple((Display *)aarv(0), (GC)aarv(1), (Pixmap)iarv(2));
	return unit;
}
