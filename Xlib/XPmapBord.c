/* text generated automatically by mkstub */

#include "stub.h"

value ML_XSetWindowBorderPixmap(v)
value *v;
{
	XSetWindowBorderPixmap(
		(Display*) aarv(0),
		(Window) iarv(1),
		(Pixmap) iarv(2)
	);
	return unit;
}
