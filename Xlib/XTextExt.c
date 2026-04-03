/* $Id: XTextExt.c,v 1.1 1998/05/20 17:49:38 ddr Exp $ */

#include "stub.h"

value ML_XTextWidth(v)
value *v;
{
	int r = XTextWidth(
		(XFontStruct*) aarv(0),
		(const char*) sarv(1),
		(int) iarv(2)
	);
	return MLINT(r);
}
