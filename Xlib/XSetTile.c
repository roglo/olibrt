/* $Id: XSetTile.c,v 1.1 1998/05/20 17:49:35 ddr Exp $ */

#include "stub.h"

value ML_XSetTile(v)
value *v;
{
	XSetTile((Display *)aarv(0), (GC)aarv(1), (Pixmap)iarv(2));
	return unit;
}
