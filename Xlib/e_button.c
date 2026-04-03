/* $Id: e_button.c,v 1.1 1998/05/20 17:49:41 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xbutton(v)
value v;
{
	return Val_addr(&((XEvent *)aar())->xbutton);
}

value ML_XButtonEvent_x(v)
value v;
{
	return Val_long(((XButtonEvent *)aar())->x);
}

value ML_XButtonEvent_y(v)
value v;
{
	return Val_long(((XButtonEvent *)aar())->y);
}

value ML_XButtonEvent_x_root(v)
value v;
{
	return Val_long(((XButtonEvent *)aar())->x_root);
}

value ML_XButtonEvent_y_root(v)
value v;
{
	return Val_long(((XButtonEvent *)aar())->y_root);
}

value ML_XButtonEvent_button(v)
value v;
{
	return Val_long(((XButtonEvent *)aar())->button);
}

value ML_XButtonEvent_state(v)
value v;
{
	return Val_long(((XButtonEvent *)aar())->state);
}
