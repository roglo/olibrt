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

value ML_XftDrawString8(v)
value *v;
{
	XftDrawString8((XftDraw*) aarv(0),
		       (_Xconst XftColor*) aarv(1),
		       (XftFont*) aarv(2),
		       (int) iarv(3),
		       (int) iarv(4),
		       (_Xconst FcChar8*) sarv(5),
		       (int) iarv(6));
	return unit;
}
