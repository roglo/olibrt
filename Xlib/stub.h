/* $Id: stub.h,v 1.2 2017/05/14 01:11:49 deraugla Exp $ */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#undef True
#undef False
#include "caml/mlvalues.h"
#undef True
#undef False

#define iarv(x)		(Long_val(v[(x)]))
#define iar()		(Long_val(v))
#define aarv(x)		(v[x]-1)
#define aar()		(v-1)
#define sarv(x)		(String_val(v[x]))
#define sar()		(String_val(v))
#define new(t)		((value)(((char *)malloc(sizeof(t)))+1))
#define new_tab(t,l)	((value)(((char *)malloc(sizeof(t)*(l)))+1))
#define unit		Val_unit
#define const

/* hope that -1 is *not* a valid address... */
#define Val_addr(x)	((long)(x)+1)

/* backward compatibility */
#define CINT				Long_val
#define CSTRING				String_val
#define MLINT				Val_long
#define GET_STRING_LENGTH(s,n)		{n=string_length(s);}
#define MLADDR				Val_addr
#define alloc_uninitialized_string	alloc_string
#define atom(tag) (Val_hp (&(first_atoms [tag])))
#undef Atom
/* end */
