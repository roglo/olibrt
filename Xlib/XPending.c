/* $Id: XPending.c,v 1.1 1998/05/20 17:49:24 ddr Exp $ */

#include "stub.h"

value ML_XPending(v)
value v;
{
	return MLINT(XPending((Display *)aar()));
}
