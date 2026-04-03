/* $Id: XChkMaskEv.c,v 1.3 2008/12/10 05:55:34 deraugla Exp $ */

#include "stub.h"

value ML_XCheckMaskEvent(v)
value *v;
{
	Bool r = XCheckMaskEvent((Display *)aarv(0), (long)iarv(1),
				 (XEvent *)aarv(2));
	return MLINT(r);
}
