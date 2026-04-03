/* $Id: s_atom.c,v 1.1 2006/05/08 19:48:38 deraugla Exp $ */

#include "stub.h"

value ML_alloc_XAtom(v)
value v;
{
	return new_tab(Atom, (int)iar());
}

value ML_set_XAtom(v)
value *v;
{
	((Atom *)aarv(1))[(int)iarv(2)] = (long)iarv(0);
	return unit;
}
