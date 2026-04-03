/* $Id: convert.c,v 1.4 2007/07/05 20:10:47 deraugla Exp $ */

#include "stub.h"
#include <strings.h>
#include <stdio.h>
#include "caml/memory.h"
#include <string.h>

value alloc_uninitialized_string();
#if 0
static char stbuff[8192];
static char stbuff2[1024];
#endif

value ML_string_of_C_String(v)
value *v;
{
	int len;
	value r;
	len = (int)iarv(1);
	r = alloc_uninitialized_string(len);
	bcopy((char *)aarv(0), (char *)r, len);
	return r;
}

value ML_C_String_length(v)
value v;
{
	return MLINT(strlen((char *)aar()));
}

value ML_alloc_C_String(v)
value v;
{
	return new_tab(char, iar());
}

value ML_free(v)
value v;
{
	free((char *)aar());
	return unit;
}
