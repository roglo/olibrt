/* $Id: s_visual.c,v 1.1 1998/05/20 17:49:49 ddr Exp $ */

#include "stub.h"

value ML_Visual_class(v)
value v;
{
	return MLINT(((Visual *)aar())->class);
}
