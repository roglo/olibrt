/* $Id: e_focus.c,v 1.1 1998/05/20 17:49:43 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xfocus(v)
value v;
{
	return Val_addr(&((XEvent *)aar())->xfocus);
}

value ML_XFocusChangeEvent_detail(v)
value v;
{
	return Val_long(((XFocusChangeEvent *)aar())->detail);
}

value ML_XFocusChangeEvent_mode(v)
value v;
{
	return Val_long(((XFocusChangeEvent *)aar())->mode);
}
