/* text generated automatically by mkstub */

#include "stub.h"

value ML_XGetImage(v)
value *v;
{
	XImage * r = XGetImage(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(int) iarv(2),
		(int) iarv(3),
		(unsigned int) iarv(4),
		(unsigned int) iarv(5),
		(unsigned long) iarv(6),
		(int) iarv(7)
	);
	return Val_addr(r);
}

value ML_XGetSubImageWithData(v)
value *v;
{
	XImage *r;
	((XImage *)aarv(8))->data = (char *) String_val(v[9]);
	r = XGetSubImage(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(int) iarv(2),
		(int) iarv(3),
		(unsigned int) iarv(4),
		(unsigned int) iarv(5),
		(unsigned long) iarv(6),
		(int) iarv(7),
		(XImage*) aarv(8),
		(int) iarv(10),
		(int) iarv(11)
	);
	((XImage *)aarv(8))->data = 0;
	return Val_addr(r);
}

value ML_XGetSubImage(v)
value *v;
{
	XImage * r = XGetSubImage(
		(Display*) aarv(0),
		(Drawable) iarv(1),
		(int) iarv(2),
		(int) iarv(3),
		(unsigned int) iarv(4),
		(unsigned int) iarv(5),
		(unsigned long) iarv(6),
		(int) iarv(7),
		(XImage*) aarv(8),
		(int) iarv(9),
		(int) iarv(10)
	);
	return Val_addr(r);
}
