/* $Id: e_expose.c,v 1.2 2008/03/14 10:13:50 deraugla Exp $ */

#include "stub.h"

value ML_XEvent_xexpose(v)
value v;
{
	return MLADDR(&((XEvent *)aar())->xexpose);
}

value ML_XExposeEvent_x(v)
value v;
{
	return MLINT(((XExposeEvent *)aar())->x);
}

value ML_XExposeEvent_y(v)
value v;
{
	return MLINT(((XExposeEvent *)aar())->y);
}

value ML_XExposeEvent_width(v)
value v;
{
	return MLINT(((XExposeEvent *)aar())->width);
}

value ML_XExposeEvent_height(v)
value v;
{
	return MLINT(((XExposeEvent *)aar())->height);
}

value ML_XExposeEvent_count(v)
value v;
{
	return MLINT(((XExposeEvent *)aar())->count);
}
