/* $Id: s_szhint.c,v 1.1 1998/05/20 17:49:49 ddr Exp $ */

#include "stub.h"
#include <X11/Xutil.h>

value ML_alloc_XSizeHints(v)
value v;
{
	return new(XSizeHints);
}

value ML_set_XSizeHints_x(v)
value *v;
{
	((XSizeHints *)aarv(1))->x = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_y(v)
value *v;
{
	((XSizeHints *)aarv(1))->y = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_width(v)
value *v;
{
	((XSizeHints *)aarv(1))->width = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_height(v)
value *v;
{
	((XSizeHints *)aarv(1))->height = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_base_width(v)
value *v;
{
	((XSizeHints *)aarv(1))->base_width = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_base_height(v)
value *v;
{
	((XSizeHints *)aarv(1))->base_height = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_min_width(v)
value *v;
{
	((XSizeHints *)aarv(1))->min_width = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_min_height(v)
value *v;
{
	((XSizeHints *)aarv(1))->min_height = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_width_inc(v)
value *v;
{
	((XSizeHints *)aarv(1))->width_inc = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_height_inc(v)
value *v;
{
	((XSizeHints *)aarv(1))->height_inc = (int)iarv(0);
	return unit;
}

value ML_set_XSizeHints_flags(v)
value *v;
{
	((XSizeHints *)aarv(1))->flags = (long)iarv(0);
	return unit;
}
