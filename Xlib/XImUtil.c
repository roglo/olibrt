/* $Id: XImUtil.c,v 1.1 1998/05/20 17:49:20 ddr Exp $ */

#include "stub.h"

value ML_XCreateImageNullData(v)
value *v;
{
	XImage * r = XCreateImage(
		(Display*) aarv(0),
		(Visual*) aarv(1),
		(unsigned int) iarv(2),
		(int) iarv(3),
		(int) iarv(4),
		(char*) 0,
		(unsigned int) iarv(5),
		(unsigned int) iarv(6),
		(int) iarv(7),
		(int) iarv(8)
	);
	return Val_addr(r);
}

value ML_XDestroyImage(v)
value v;
{
	XDestroyImage((XImage *)aar());
	return unit;
}
