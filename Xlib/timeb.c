/* $Id: timeb.c,v 1.3 1999/01/08 16:35:15 roglo Exp $ */

#include <sys/time.h>
#include <unistd.h>
#include "stub.h"

static struct timeval tv;

value ML_ctime(v)
value v;
{
	(void) gettimeofday(&tv, NULL);
	return MLINT(tv.tv_sec);
}

value ML_ctime_ms(v)
value v;
{
	return MLINT(tv.tv_usec / 1000);
}
