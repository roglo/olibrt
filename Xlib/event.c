/* $Id: event.c,v 1.1 1998/05/20 17:49:46 ddr Exp $ */

#include "stub.h"

value ML_alloc_XEvent(v)
value v;
{
	return new(XEvent);
}

value ML_XEvent_type(v)
value v;
{
	return MLINT(((XEvent *)aar())->type);
}
