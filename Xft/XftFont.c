#include "../Xlib/stub.h"
#include <X11/XKBlib.h>
#include "caml/memory.h"
#include "caml/alloc.h"
#include <X11/Xft/Xft.h>
#include <X11/extensions/Xrandr.h>
#include <stdio.h>

value ML_alloc_XftColor(v)
value v;
{
        return new(XftColor);
}

value ML_alloc_XGlyphInfo(v)
value v;
{
        return new(XGlyphInfo);
}

value ML_XftFont_width(v)
value v;
{
        return MLINT(((XftFont *)aar())->max_advance_width);
}

value ML_XftFont_height(v)
value v;
{
        return MLINT(((XftFont *)aar())->height);
}

value ML_XGlyphInfo_width(v)
value v;
{
        return MLINT(((XGlyphInfo *)aar())->width);
}

value ML_XGlyphInfo_height(v)
value v;
{
        return MLINT(((XGlyphInfo *)aar())->height);
}

value ML_XGlyphInfo_yOff(v)
value v;
{
        return MLINT(((XGlyphInfo *)aar())->yOff);
}

value ML_XftColorAllocName(v)
value *v;
{
	Bool r;
	r = XftColorAllocName((Display*) aarv(0),
			      (_Xconst Visual*) aarv(1),
			      (Colormap) iarv(2),
			      (_Xconst char*) sarv(3),
			      (XftColor *) aarv(4));
	return MLINT(r);
}

value ML_XftDrawCreate(v)
value *v;
{
	XftDraw *d;
	d = XftDrawCreate((Display*) aarv(0),
			  (Drawable) iarv(1),
			  (Visual*) aarv(2),
			  (Colormap) iarv(3));
	return Val_addr(d);
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

value ML_XftFontOpenName(v)
value *v;
{
	XftFont * r =
	  XftFontOpenName((Display*) aarv(0),
			  (int) iarv(1),
			  (const char*) sarv(2));
	return Val_addr(r);
}

value ML_XftTextExtents8(v)
value *v;
{
	XftTextExtents8((Display *) aarv(0),
			(XftFont *) aarv(1),
			(const FcChar8 *) sarv(2),
			(int) iarv(3),
			(XGlyphInfo *) aarv(4));
	return unit;
}

value ML_get_screen_size_mm(v)
value v;
{
	CAMLparam1(v);
	CAMLlocal1(r);
	Display *dpy;
	int width_mm, height_mm;
	int event_base, error_base, x;
	XRROutputInfo *output_info;
	XRRScreenResources *resources;

	dpy = (Display *)aar();
	if (!XRRQueryExtension(dpy, &event_base, &error_base)) {
	  fprintf(stderr, "<W> XRandR not available.\n");
	  x = 0;
	}
	else {
	  resources = XRRGetScreenResources(dpy, RootWindow(dpy, 0));
	  if (!resources) {
	    fprintf(stderr, "<W> Impossible to get XRandR resources.\n");
	    x = 0;
	  }
	  else {
	    x = 0;
	    for (int i = 0; i < resources->noutput && x == 0; i++) {
	      output_info =
		XRRGetOutputInfo(dpy, resources, resources->outputs[i]);
	      if (output_info) {
		if (output_info->connection == RR_Connected &&
		    output_info->mm_width > 0) {
		  width_mm = output_info->mm_width;
		  height_mm = output_info->mm_height;
		  x = 1;
		}
		XRRFreeOutputInfo(output_info);
	      }
	    }
	    XRRFreeScreenResources(resources);
	  }
	}
	if (x == 0) {
	  width_mm = DisplayWidthMM(dpy, DefaultScreen(dpy));
	  height_mm = DisplayHeightMM(dpy, DefaultScreen(dpy));
	}
	r = alloc_small(2, 0);
        Store_field(r, 0, Val_int(width_mm));
        Store_field(r, 1, Val_int(height_mm));
        CAMLreturn(r);
}
