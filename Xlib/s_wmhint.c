/* $Id: s_wmhint.c,v 1.1 1998/05/20 17:49:50 ddr Exp $ */

#include "stub.h"
#include <X11/Xutil.h>

value ML_alloc_XWMHints(v)
value v;
{
	return new(XWMHints);
}

value ML_set_XWMHints_input(v)
value *v;
{
	((XWMHints *)aarv(1))->input = (int)iarv(0);
	return unit;
}

value ML_set_XWMHints_flags(v)
value *v;
{
	((XWMHints *)aarv(1))->flags = (long)iarv(0);
	return unit;
}
