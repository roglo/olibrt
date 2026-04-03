/* $Id: XDisName.c,v 1.1 1998/05/20 17:49:10 ddr Exp $ */

#include "stub.h"

value ML_XDisplayName(v)
value v;
{
	char * r = XDisplayName(
		(const char*) sar()
	);
	return MLADDR(r);
}
