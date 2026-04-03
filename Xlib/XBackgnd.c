/* $Id: XBackgnd.c,v 1.1 1998/05/20 17:48:59 ddr Exp $ */

#include "stub.h"

value ML_XSetWindowBackground(v)
value *v;
{
	XSetWindowBackground((Display *)aarv(0), (Window)iarv(1),
			     (unsigned long)iarv(2));
	return unit;
}
