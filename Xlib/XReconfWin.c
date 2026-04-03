/* $Id: XReconfWin.c,v 1.1 1998/05/20 17:49:28 ddr Exp $ */

#include "stub.h"

value ML_alloc_XWindowChanges(v)
value v;
{
	return new(XWindowChanges);
}

value ML_set_XWindowChanges_x(v)
value *v;
{
	((XWindowChanges *)aarv(1))->x = (int)iarv(0);
	return unit;
}

value ML_set_XWindowChanges_y(v)
value *v;
{
	((XWindowChanges *)aarv(1))->y = (int)iarv(0);
	return unit;
}

value ML_set_XWindowChanges_stack_mode(v)
value *v;
{
	((XWindowChanges *)aarv(1))->stack_mode = (int)iarv(0);
	return unit;
}

value ML_XConfigureWindow(v)
value *v;
{
	XConfigureWindow((Display *)aarv(0), (Window)iarv(1),
			 (int)iarv(2), (XWindowChanges *)aarv(3));
	return unit;
}
