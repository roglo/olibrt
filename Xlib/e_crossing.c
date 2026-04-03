/* $Id: e_crossing.c,v 1.1 1998/05/20 17:49:42 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xcrossing(v)
value v;
{
	return Val_addr(&((XEvent *)aar())->xcrossing);
}

value ML_XCrossingEvent_x(v)
value v;
{
	return Val_long(((XCrossingEvent *)aar())->x);
}

value ML_XCrossingEvent_y(v)
value v;
{
	return Val_long(((XCrossingEvent *)aar())->y);
}

value ML_XCrossingEvent_x_root(v)
value v;
{
	return Val_long(((XCrossingEvent *)aar())->x_root);
}

value ML_XCrossingEvent_y_root(v)
value v;
{
	return Val_long(((XCrossingEvent *)aar())->y_root);
}

value ML_XCrossingEvent_detail(v)
value v;
{
	return Val_long(((XCrossingEvent *)aar())->detail);
}

value ML_XCrossingEvent_focus(v)
value v;
{
	return Val_long(((XCrossingEvent *)aar())->focus);
}
