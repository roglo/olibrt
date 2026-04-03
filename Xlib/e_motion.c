/* $Id: e_motion.c,v 1.1 1998/05/20 17:49:45 ddr Exp $ */

#include "stub.h"

value ML_XEvent_xmotion(v)
value v;
{
	return MLADDR(&((XEvent *)aar())->xmotion);
}

value ML_XMotionEvent_x(v)
value v;
{
	return MLINT(((XMotionEvent *)aar())->x);
}

value ML_XMotionEvent_y(v)
value v;
{
	return MLINT(((XMotionEvent *)aar())->y);
}

value ML_XMotionEvent_state(v)
value v;
{
	return MLINT(((XMotionEvent *)aar())->state);
}
