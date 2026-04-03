/* text generated automatically by mkstub */

#include "stub.h"

value ML_XKeysymToString(v)
value v;
{
	char * r = XKeysymToString(
		(KeySym) iar()
	);
	return Val_addr(r);
}
