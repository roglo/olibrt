/* $Id: XPmapBgnd.c,v 1.1 1998/05/20 17:49:25 ddr Exp $ */

#include "stub.h"

value ML_XSetWindowBackgroundPixmap(v)
value *v;
{
	XSetWindowBackgroundPixmap((Display *)aarv(0), (Window)iarv(1),
				   (Pixmap)iarv(2));
	return unit;
}
