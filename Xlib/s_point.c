/* $Id: s_point.c,v 1.1 1998/05/20 17:49:48 ddr Exp $ */

#include "stub.h"

value ML_alloc_XPoint(v)
value v;
{
	return new_tab(XPoint, (int)iar());
}

value ML_set_XPoint_x(v)
value *v;
{
	((XPoint *)aarv(1))[(int)iarv(2)].x = (long)iarv(0);
	return unit;
}

value ML_set_XPoint_y(v)
value *v;
{
	((XPoint *)aarv(1))[(int)iarv(2)].y = (long)iarv(0);
	return unit;
}
