#include "../Xlib/stub.h"
#include <X11/Xft/Xft.h>

value ML_XftFontOpenName(v)
value *v;
{
	XftFont * r = XftFontOpenName(
		(Display*) aarv(0),
		(int) iarv(1),
		(const char*) sarv(2)
	);
	return Val_addr(r);
}
