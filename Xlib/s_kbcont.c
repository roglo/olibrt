/* $Id: s_kbcont.c,v 1.1 1998/05/20 17:49:47 ddr Exp $ */

#include "stub.h"

value ML_alloc_XKeyboardControl (v)
value v;
{
	return new (XKeyboardControl);
}

value ML_set_XKeyboardControl_bell_percent (v)
value *v;
{
	((XKeyboardControl *) aarv (1))->bell_percent = (int) iarv(0);
	return unit;
}

value ML_set_XKeyboardControl_bell_pitch (v)
value *v;
{
	((XKeyboardControl *) aarv (1))->bell_pitch = (int) iarv(0);
	return unit;
}

value ML_set_XKeyboardControl_bell_duration (v)
value *v;
{
	((XKeyboardControl *) aarv (1))->bell_duration = (int) iarv(0);
	return unit;
}
