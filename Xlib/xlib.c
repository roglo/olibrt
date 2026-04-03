/* $Id: xlib.c,v 1.7 2017/05/14 01:11:49 deraugla Exp $ */

#include <unistd.h>
#include "stub.h"
#include "caml/alloc.h"
#include <sys/ioctl.h>
#include "config.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include <errno.h>
#include <string.h>

#ifdef OLDX
typedef int (*XErrorHandler) ();
typedef int (*XIOErrorHandler) ();
#endif

static value x_error_exn_tag, xio_error_exn_tag;

static int error_handler (dpy, err)
Display* dpy;
XErrorEvent* err;
{
	char msg[80];
	XGetErrorText (dpy, err->error_code, msg, 80);
	raise_with_string (x_error_exn_tag, msg);
}

static int io_error_handler (dpy, xerrev)
Display* dpy;
XErrorEvent *xerrev;
{
	CAMLparam0();
	CAMLlocal1(r);
	int err = errno;

	r = alloc_small (3, 0);
	Field(r, 0) = xio_error_exn_tag;
	Field(r, 1) = copy_string(XDisplayString (dpy));
	Field(r, 2) = copy_string(strerror(err));
	mlraise (r);
}

value ML_set_xerror_exn (v)
value *v;
{
	x_error_exn_tag = Field(v[0], 0);
	xio_error_exn_tag = Field(v[1], 0);
	register_global_root(&x_error_exn_tag);
	register_global_root(&xio_error_exn_tag);
	XSetErrorHandler (error_handler);
	XSetIOErrorHandler (io_error_handler);
	return unit;
}

value ML_null_pt(v)
value v;
{
	return Val_addr(0);
}

value ML_is_null_pt(v)
value v;
{
	return Val_bool((char *)aar()==0);
}

value ML_alloc_LongRef(v)
value v;
{
	return new(long);
}

value ML_LongRef_value(v)
value v;
{
	return Val_long(*(long *)aar());
}

value ML_set_LongRef_value(v)
value *v;
{
	*(long *)aarv(1) = (long)iarv(0);
	return unit;
}

value ML_read(fd, buff, len)
value fd, buff, len;
{
	return Val_int(read(Int_val(fd), String_val(buff), Int_val(len)));
}

value ML_ioctl_winsz(fd, row, col, xpixel, ypixel)
value fd, row, col, xpixel, ypixel;
{
#ifdef TIOCSWINSZ
	struct winsize ws;
	ws.ws_row = Int_val(row);
	ws.ws_col = Int_val(col);
	ws.ws_xpixel = Int_val(xpixel);
	ws.ws_ypixel = Int_val(ypixel);
	ioctl(Int_val(fd), TIOCSWINSZ, (char *)&ws);
#endif
	return unit;
}
