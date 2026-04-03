/* $Id: e_mapping.c,v 1.1 1998/05/20 17:49:45 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xmapping(v)
value v;
{
	return Val_addr(&((XEvent *)aar())->xmapping);
}
