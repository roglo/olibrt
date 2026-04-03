/* $Id: XMapRaised.c,v 1.1 1998/05/20 17:49:21 ddr Exp $ */

#include "stub.h"

value ML_XMapRaised(v)
value *v;
{
	XMapRaised((Display *)aarv(0), (Window)iarv(1));
	return unit;
}
