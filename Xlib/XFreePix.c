/* $Id: XFreePix.c,v 1.1 1998/05/20 17:49:16 ddr Exp $ */

#include "stub.h"

value ML_XFreePixmap(v)
value *v;
{
	XFreePixmap((Display *)aarv(0), (Pixmap)iarv(1));
	return unit;
}
