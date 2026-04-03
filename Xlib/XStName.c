/* $Id: XStName.c,v 1.1 1998/05/20 17:49:36 ddr Exp $ */

#include "stub.h"

value ML_XStoreName(v)
value *v;
{
	XStoreName(
		(Display*) aarv(0),
		(Window) iarv(1),
		(const char*) sarv(2)
	);
	return unit;
}
value ML_XSetIconName(v)
value *v;
{
        XSetIconName(
                (Display*) aarv(0),
                (Window) iarv(1),
                (const char*) sarv(2)
        );
        return unit;
}
