/* $Id: e_config.c,v 1.1 1998/05/20 17:49:42 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xconfigure(v)
value v;
{
	return MLADDR(&((XEvent *)aar())->xconfigure);
}

value ML_XConfigureEvent_x(v)
value v;
{
	return MLINT(((XConfigureEvent *)aar())->x);
}

value ML_XConfigureEvent_y(v)
value v;
{
	return MLINT(((XConfigureEvent *)aar())->y);
}

value ML_XConfigureEvent_width(v)
value v;
{
	return MLINT(((XConfigureEvent *)aar())->width);
}

value ML_XConfigureEvent_height(v)
value v;
{
	return MLINT(((XConfigureEvent *)aar())->height);
}

value ML_XConfigureEvent_border_width(v)
value v;
{
	return MLINT(((XConfigureEvent *)aar())->border_width);
}
