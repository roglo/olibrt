/* $Id: XChWAttrs.c,v 1.2 2008/12/11 03:24:48 deraugla Exp $ */

#include "stub.h"

value ML_alloc_XSetWindowAttributes(v)
value v;
{
	return new(XSetWindowAttributes);
}

value ML_set_XSetWindowAttributes_save_under(v)
value *v;
{
	((XSetWindowAttributes *)aarv(1))->save_under = (int)iarv(0);
	return unit;
}

value ML_set_XSetWindowAttributes_override_redirect(v)
value *v;
{
	((XSetWindowAttributes *)aarv(1))->override_redirect = (int)iarv(0);
	return unit;
}

value ML_set_XSetWindowAttributes_backing_store(v)
value *v;
{
	((XSetWindowAttributes *)aarv(1))->backing_store = (int)iarv(0);
	return unit;
}

value ML_set_XSetWindowAttributes_bit_gravity(v)
value *v;
{
	((XSetWindowAttributes *)aarv(1))->bit_gravity = (int)iarv(0);
	return unit;
}

value ML_XChangeWindowAttributes(v)
value *v;
{
	XChangeWindowAttributes((Display *)aarv(0),
				(Window)iarv(1),
				(long)iarv(2),
				(XSetWindowAttributes *)aarv(3));
	return unit;
}

value ML_alloc_XWindowAttributes(v)
value v;
{
	return new(XWindowAttributes);
}

value ML_XGetWindowAttributes(v)
value *v;
{
	Status r = XGetWindowAttributes((Display *)aarv(0),
					(Window)iarv(1),
					(XWindowAttributes *)aarv(2));
	return Val_long(r);
}

value ML_XWindowAttributes_colormap(v)
value v;
{
	Colormap r = ((XWindowAttributes *)aar())->colormap;
	return Val_long(r);
}

value ML_XWindowAttributes_visual(v)
value v;
{
	Visual *r = ((XWindowAttributes *)aar())->visual;
	return MLADDR(r);
}
