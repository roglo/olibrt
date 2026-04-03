/* $Id: s_kbstate.c,v 1.1 1998/05/20 17:49:48 ddr Exp $ */

#include "stub.h"

value ML_alloc_XKeyboardState (v)
value v;
{
	return new (XKeyboardState);
}

value ML_XKeyboardState_bell_percent (v)
value v;
{
	return Val_long (((XKeyboardState *) aar ())->bell_percent);
}

value ML_XKeyboardState_bell_pitch (v)
value v;
{
	return Val_long (((XKeyboardState *) aar ())->bell_pitch);
}

value ML_XKeyboardState_bell_duration (v)
value v;
{
	return Val_long (((XKeyboardState *) aar ())->bell_duration);
}
