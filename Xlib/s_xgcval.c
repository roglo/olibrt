/* $Id: s_xgcval.c,v 1.1 1998/05/20 17:49:50 ddr Exp $ */

#include "stub.h"

value ML_alloc_XGCValues(v)
value v;
{
	return new(XGCValues);
}

value ML_set_XGCValues_background(v)
value *v;
{
	((XGCValues *)aarv(1))->background = (long)iarv(0);
	return unit;
}

value ML_set_XGCValues_cap_style(v)
value *v;
{
	((XGCValues *)aarv(1))->cap_style = (int)iarv(0);
	return unit;
}

value ML_set_XGCValues_fill_style(v)
value *v;
{
	((XGCValues *)aarv(1))->fill_style = (int)iarv(0);
	return unit;
}

value ML_set_XGCValues_font(v)
value *v;
{
	((XGCValues *)aarv(1))->font = (Font)iarv(0);
	return unit;
}

value ML_set_XGCValues_foreground(v)
value *v;
{
	((XGCValues *)aarv(1))->foreground = (long)iarv(0);
	return unit;
}

value ML_set_XGCValues_line_width(v)
value *v;
{
	((XGCValues *)aarv(1))->line_width = (int)iarv(0);
	return unit;
}

value ML_set_XGCValues_stipple(v)
value *v;
{
	((XGCValues *)aarv(1))->stipple = (int)iarv(0);
	return unit;
}

value ML_set_XGCValues_tile(v)
value *v;
{
	((XGCValues *)aarv(1))->tile = (int)iarv(0);
	return unit;
}
