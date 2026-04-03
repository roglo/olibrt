/* $Id: XStBytes.c,v 1.1 1998/05/20 17:49:35 ddr Exp $ */

#include "stub.h"

value ML_XStoreBytes(v)
value *v;
{
	XStoreBytes(
		(Display*) aarv(0),
		(const char*) sarv(1),
		(int) iarv(2)
	);
	return unit;
}

value ML_XFetchBytes (v)
value *v;
{
	int nbytes_return;
	char * r = XFetchBytes (
		(Display*) aarv (0),
		&nbytes_return
	);
	Field (v[1], 0) = Val_long (nbytes_return);
	return Val_addr (r);
}
