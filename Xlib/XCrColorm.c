/* $Id: XCrColorm.c,v 1.1 2008/12/11 14:11:33 deraugla Exp $ */

#include "stub.h"

value ML_XCreateColormap(v)
value *v;
{
	Colormap r = XCreateColormap((Display *)aarv(0),
                                     (Window)iarv(1),
                                     (Visual *)aarv(2),
                                     (int)iarv(3));
	return MLINT(r);
}
