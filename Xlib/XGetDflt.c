/* $Id: XGetDflt.c,v 1.1 1998/05/20 17:49:18 ddr Exp $ */

#include "stub.h"

value ML_XGetDefault(v)
value *v;
{
	char * r = XGetDefault(
		(Display*) aarv(0),
		(const char*) sarv(1),
		(const char*) sarv(2)
	);
	return MLADDR(r);
}
