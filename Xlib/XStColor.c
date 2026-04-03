/* $Id: XStColor.c,v 1.1 1998/05/20 17:49:36 ddr Exp $ */

#include "stub.h"

value ML_XStoreColor(v)
value *v;
{
	XStoreColor(
		(Display*) aarv(0),
		(Colormap) iarv(1),
		(XColor*) aarv(2)
	);
	return unit;
}
