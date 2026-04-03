/* $Id: e_key.c,v 1.1 1998/05/20 17:49:44 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xkey(v)
value v;
{
	return MLADDR(&((XEvent *)aar())->xkey);
}

value ML_XKeyEvent_state(v)
value v;
{
	return MLINT(((XKeyEvent *)aar())->state);
}

value ML_XKeyEvent_keycode(v)
value v;
{
	return MLINT(((XKeyEvent *)aar())->keycode);
}
