/* $Id: macros.c,v 1.1 1998/05/20 17:49:46 ddr Exp $ */

#include "stub.h"

value ML_XDefaultScreen(v)
value v;
{
	int r = DefaultScreen((Display *)aar());
	return MLINT(r);
}

value ML_XDefaultRootWindow(v)
value v;
{
	int r = DefaultRootWindow((Display *)aar());
	return MLINT(r);
}

value ML_XBlackPixel(v)
value *v;
{
	long r = BlackPixel((Display *)aarv(0), (int)iarv(1));
	return MLINT(r);
}

value ML_XWhitePixel(v)
value *v;
{
	long r = WhitePixel((Display *)aarv(0), (int)iarv(1));
	return MLINT(r);
}

value ML_XConnectionNumber(v)
value v;
{
	int r = ConnectionNumber((Display *)aar());
	return MLINT(r);
}

value ML_XDefaultColormap(v)
value *v;
{
	Colormap r = DefaultColormap((Display *)aarv(0), (int)iarv(1));
	return MLINT(r);
}
value ML_XDefaultDepth(v)
value *v;
{
	int r = DefaultDepth((Display *)aarv(0), (int)iarv(1));
	return MLINT(r);
}

value ML_XDisplayHeight(v)
value *v;
{
        int r = DisplayHeight(
                (Display*) aarv(0),
                (int) iarv(1)
        );
        return MLINT(r);
}

value ML_XDisplayWidth(v)
value *v;
{
	int r = DisplayWidth(
		(Display*) aarv(0),
		(int) iarv(1)
	);
	return MLINT(r);
}

value ML_XDefaultVisual(v)
value *v;
{
	Visual * r = DefaultVisual(
		(Display*) aarv(0),
		(int) iarv(1)
	);
	return MLADDR(r);
}

value ML_XGetPixel(v)
value *v;
{
	unsigned long r = XGetPixel(
		(XImage*) aarv(0),
		(int) iarv(1),
		(int) iarv(2)
	);
	return MLINT(r);
}

value ML_XGetPixelWithData(v)
value *v;
{
	unsigned long r;
	((XImage *)aarv(0))->data = (char *) CSTRING(v[1]);
	r = XGetPixel(
		(XImage*) aarv(0),
		(int) iarv(2),
		(int) iarv(3)
	);
	((XImage *)aarv(0))->data = 0;
	return MLINT(r);
}

value ML_XPutPixel(v)
value *v;
{
	XPutPixel(
		(XImage*) aarv(0),
		(int) iarv(1),
		(int) iarv(2),
		(unsigned long) iarv(3)
	);
	return unit;
}

value ML_XPutPixelWithData(v)
value *v;
{
	((XImage *)aarv(0))->data = (char *) CSTRING(v[1]);
	XPutPixel(
		(XImage*) aarv(0),
		(int) iarv(2),
		(int) iarv(3),
		(unsigned long) iarv(4)
	);
	((XImage *)aarv(0))->data = 0;
	return unit;
}

value ML_XDefaultGC (v)
value *v;
{
	GC r = DefaultGC (
		(Display*) aarv (0),
		(int) iarv (1)
	);
	return Val_addr (r);
}
