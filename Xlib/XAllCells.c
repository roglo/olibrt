/* $Id: XAllCells.c,v 1.2 2008/11/07 18:16:18 deraugla Exp $ */

#include "stub.h"

value ML_XAllocColorCells(v)
value *v;
{
	int r = XAllocColorCells((Display *)aarv(0),
				 (Colormap)iarv(1),
				 (int)iarv(2), (long *)aarv(3),
				 (int)iarv(4), (long *)aarv(5),
				 (int)iarv(6));
	return MLINT(r);
}

value ML_XFreeColors(v)
value *v;
{
        int r = XFreeColors((Display *)aarv(0),
                            (Colormap)iarv(1),
                            (unsigned long *)aarv(2),
                            (int)iarv(3),
                            (unsigned long)iarv(4));
        return MLINT(r);
}
