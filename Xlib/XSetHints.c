/* text generated automatically by mkstub */

#include "stub.h"
#include <X11/Xutil.h>

value ML_XSetWMHints(v)
value *v;
{
	XSetWMHints(
		(Display*) aarv(0),
		(Window) iarv(1),
		(XWMHints*) aarv(2)
	);
	return unit;
}

value ML_XSetNormalHints(v)
value *v;
{
	XSetNormalHints(
		(Display*) aarv(0),
		(Window) iarv(1),
		(XSizeHints*) aarv(2)
	);
	return unit;
}

value ML_XSetStandardProperties(v)
value *v;
{
	char *a5 = (char *) aarv(5);
	XSetStandardProperties(
		(Display*) aarv(0),
		(Window) iarv(1),
		(const char*) sarv(2),
		(const char*) sarv(3),
		(Pixmap) iarv(4),
		&a5,
		1,
		(XSizeHints*) aarv(6)
	);
	return unit;
}

value ML_XSetTransientForHint(v)
value *v;
{
	XSetTransientForHint(
		(Display*) aarv(0),
		(Window) iarv(1),
		(Window) iarv(2)
	);
	return unit;
}

value ML_XSetClassHint(v)
value *v;
{
	XClassHint xch;
	xch.res_name = sarv(2);
	xch.res_class = sarv(3);
	XSetClassHint(
		(Display*) aarv(0),
		(Window) iarv(1),
		&xch
	);
	return unit;
}
