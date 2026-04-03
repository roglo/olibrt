/* $Id: s_ximage.c,v 1.2 2001/06/08 15:43:23 ddr Exp $ */

#include "stub.h"

value ML_XImage_data(v)
value v;
{
	return MLADDR(((XImage *)aar())->data);
}

value ML_set_XImage_bitmap_bit_order(v)
value *v;
{
	((XImage *)aarv(1))->bitmap_bit_order = (int)iarv(0);
	return unit;
}

value ML_set_XImage_byte_order(v)
value *v;
{
	((XImage *)aarv(1))->byte_order = (int)iarv(0);
	return unit;
}

#include "caml/custom.h"
#include "caml/memory.h"

static void my_func(value v)
{
	XImage *xim = (XImage *) Field (v, 1);

	if (xim->data) free ((char *) xim->data);
	free ((char *) xim);
}

#ifdef CL06
extern unsigned long allocated_words;
extern unsigned free_mem_percent_max;
#endif

value ML_MLimage_of_XImage(v)
value v;
{
	XImage *xim;
	value result;
	int size;
	struct custom_operations custop;	
	CAMLparam1(v);

	xim = (XImage *) aar();
	custop.identifier = "ximage";
	custop.finalize = my_func;
	custop.compare = custom_compare_default;
	custop.hash = custom_hash_default;
	custop.serialize = custom_serialize_default;
	custop.deserialize = custom_deserialize_default;
	result = alloc_custom(&custop, 0, xim->width * xim->height, 1000000);
#if 0
	size = 1 + (sizeof (*xim) + sizeof (long) - 1) / sizeof (long) +
		1 + (xim->data ?
			xim->width * xim->height * xim->depth
				/ sizeof (char) / sizeof (long)
		: 0);

	Field (result, 0) = (value) my_func;
	Field (result, 1) = (value) xim;
#endif
#ifdef CL06
	allocated_words += size * free_mem_percent_max / 100;
#endif
	CAMLreturn(result);
}
