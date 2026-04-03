/* $Id: e_graphics.c,v 1.1 1998/05/20 17:49:44 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xgraphicsexpose(v)
value v;
{
	return MLADDR(&((XEvent *)aar())->xgraphicsexpose);
}

value ML_XGraphicsExposeEvent_x(v)
value v;
{
	return MLINT(((XGraphicsExposeEvent *)aar())->x);
}

value ML_XGraphicsExposeEvent_y(v)
value v;
{
	return MLINT(((XGraphicsExposeEvent *)aar())->y);
}

value ML_XGraphicsExposeEvent_width(v)
value v;
{
	return MLINT(((XGraphicsExposeEvent *)aar())->width);
}

value ML_XGraphicsExposeEvent_height(v)
value v;
{
	return MLINT(((XGraphicsExposeEvent *)aar())->height);
}

value ML_XGraphicsExposeEvent_count(v)
value v;
{
	return MLINT(((XGraphicsExposeEvent *)aar())->count);
}
