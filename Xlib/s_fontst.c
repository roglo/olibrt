/* $Id: s_fontst.c,v 1.1 1998/05/20 17:49:47 ddr Exp $ */

#include "stub.h"

value ML_XFontStruct_ascent(v)
value v;
{
	return MLINT(((XFontStruct *)aar())->ascent);
}

value ML_XFontStruct_descent(v)
value v;
{
	return MLINT(((XFontStruct *)aar())->descent);
}

value ML_XFontStruct_fid(v)
value v;
{
	return MLINT(((XFontStruct *)aar())->fid);
}

value ML_XFontStruct_max_bounds_ascent(v)
value v;
{
	return MLINT(((XFontStruct *)aar())->max_bounds.ascent);
}

value ML_XFontStruct_max_bounds_descent(v)
value v;
{
	return MLINT(((XFontStruct *)aar())->max_bounds.descent);
}
