/* $Id: select.c,v 1.2 2007/07/05 20:10:47 deraugla Exp $ */

#include <sys/types.h>
#include <sys/time.h>
#include "stub.h"

#ifndef FD_ZERO

#define NFDBITS 32
#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

#endif

value ML_alloc_fd_set(v)
value v;
{
	return new(fd_set);
}

value ML_FD_ZERO(v)
value v;
{
	FD_ZERO((fd_set *)aar());
	return unit;
}

value ML_FD_SET(v)
value *v;
{
	FD_SET((int)iarv(0), (fd_set *)aarv(1));
	return unit;
}

value ML_FD_ISSET(v)
value *v;
{
	return MLINT(FD_ISSET((int)iarv(0), (fd_set *)aarv(1)));
}

value ML_fselect(v)
value *v;
{
	struct timeval timeout;

	timeout.tv_sec = (long)iarv(2) / 1000;
	timeout.tv_usec = (long)iarv(2) % 1000 * 1000;
	return MLINT(select((int)iarv(0), (fd_set *)aarv(1), 0, 0,
			    ((long)iarv(2) < 0) ? 0 : &timeout));
}
