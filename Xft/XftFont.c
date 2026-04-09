#include "../Xlib/stub.h"
#include <X11/XKBlib.h>
#include "caml/memory.h"
#include "caml/alloc.h"
#include <X11/Xft/Xft.h>
#include <X11/extensions/Xrandr.h>
#include <stdio.h>

value ML_XftColorAllocName(v)
value *v;
{
	Bool r;
	r = XftColorAllocName((Display*) aarv(0),
			      (_Xconst Visual*) aarv(1),
			      (Colormap) aarv(2),
			      (_Xconst char*) sarv(3),
			      (XftColor *) aarv(4));
	return MLINT(r);
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
	XftFont * r = XftFontOpenName(
		(Display*) aarv(0),
		(int) iarv(1),
		(const char*) sarv(2)
	);
	return Val_addr(r);
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
