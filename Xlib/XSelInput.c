/* $Id: XSelInput.c,v 1.1 1998/05/20 17:49:29 ddr Exp $ */

#include "stub.h"

value ML_XSelectInput(v)
value *v;
{
	XSelectInput((Display *)aarv(0), (Window)iarv(1),
		     (unsigned long)iarv(2));
	return unit;
}
