/* $Id: XKeyBind.c,v 1.4 2017/05/14 01:11:49 deraugla Exp $ */

#include "stub.h"
#include <X11/XKBlib.h>
#include "caml/memory.h"
#include "caml/alloc.h"

value ML_XKeycodeToKeysym(v)
value *v;
{
	KeySym r = XkbKeycodeToKeysym((Display *)aarv(0), (KeyCode)iarv(1),
				       0, (int)iarv(2));
	return MLINT(r);
}

value ML_XLookupString(v)
value v;
{
	CAMLparam1(v);
	CAMLlocal1(r);
	int len;
	KeySym keysym_return;

	len = XLookupString((XKeyEvent *)((char *)Field (v, 0) - 1),
                            (char *)String_val (Field (v, 1)),
                            (int)Int_val (Field (v, 2)), &keysym_return,
                            NULL);
	r = alloc_small (2, 0);
	Store_field (r, 0, Val_int (keysym_return));
	Store_field (r, 1, Val_int (len));
	CAMLreturn (r);
}

value ML_XRefreshKeyboardMapping(v)
value v;
{
	XRefreshKeyboardMapping(
		(XMappingEvent*) aar()
	);
	return unit;
}
