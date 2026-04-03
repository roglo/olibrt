/* $Id: XSetSOwner.c,v 1.1 1998/05/20 17:49:34 ddr Exp $ */

#include "stub.h"

value ML_XSetSelectionOwner(v)
value *v;
{
	XSetSelectionOwner((Display *)aarv(0), (Atom)iarv(1),
			   (Window)iarv(2), (Time)iarv(3));
	return unit;
}
