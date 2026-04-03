/* $Id: s_color.c,v 1.1 1998/05/20 17:49:46 ddr Exp $ */

#include "stub.h"

value ML_alloc_XColor(v)
value v;
{
	return new(XColor);
}

value ML_XColor_pixel(v)
value v;
{
	return MLINT(((XColor *)aar())->pixel);
}

value ML_XColor_red(v)
value v;
{
	return MLINT(((XColor *)aar())->red);
}

value ML_XColor_green(v)
value v;
{
	return MLINT(((XColor *)aar())->green);
}

value ML_XColor_blue(v)
value v;
{
	return MLINT(((XColor *)aar())->blue);
}

value ML_set_XColor_pixel(v)
value *v;
{
	((XColor *)aarv(1))->pixel = (long)iarv(0);
	return unit;
}

value ML_set_XColor_red(v)
value *v;
{
	((XColor *)aarv(1))->red = (unsigned short)iarv(0);
	return unit;
}

value ML_set_XColor_green(v)
value *v;
{
	((XColor *)aarv(1))->green = (unsigned short)iarv(0);
	return unit;
}

value ML_set_XColor_blue(v)
value *v;
{
	((XColor *)aarv(1))->blue = (unsigned short)iarv(0);
	return unit;
}

value ML_set_XColor_flags(v)
value *v;
{
	((XColor *)aarv(1))->flags = (char)iarv(0);
	return unit;
}
