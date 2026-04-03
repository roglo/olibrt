/* $Id: e_any.c,v 1.1 1998/05/20 17:49:41 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xany(v)
value v;
{
	return MLADDR(&((XEvent *)aar())->xany);
}

value ML_XAnyEvent_window(v)
value v;
{
	return MLINT(((XAnyEvent *)aar())->window);
}
