/* $Id: fvie.c,v 1.2 1998/07/22 16:29:21 ddr Exp $ */

#include "caml/mlvalues.h"

/* backward compatibility */
#define CINT				Long_val
#define CSTRING				String_val
#define MLINT				Val_long
#define GET_STRING_LENGTH(s,n)		{n=string_length(s);}
#define FIELD				Field
#define alloc_uninitialized_string	alloc_string
#define atom(tag) (Val_hp (&(first_atoms [tag])))
#undef Atom
/* end */

#define MLAND(x,y)	((x)&(y))
#define MLOR(x,y)	((x)|(y))
#define MLADD(x,y)	((x)+(y)-1)
#define MLINCR(x)	(MLINT(x)-1)

#define tag_mask	(MLINT(((1<<8)-1)<<16))
#define genc_mask	(MLINT(((1<<8)-1)<<8))
#define sum_mask	(MLINT(((1<<8)-1)<<0))

#define elem_tag(e)	(MLAND((e),tag_mask))
#define elem_genc(e)	(MLAND((e),genc_mask))
#define elem_sum(e)	(MLAND((e),sum_mask))
#define elem_no_tag(e)	(MLAND((e),genc_mask|sum_mask))
#define elem_no_genc(e)	(MLAND((e),tag_mask|sum_mask))

#define make_tag(t)	(MLINT((t)<<16))
#define make_genc(r)	(MLINT((r)<<8))
#define make_sum(s)	(MLINT((s)<<0))

#define NoElem		(make_tag(0))
#define BornElem	(make_tag(1))
#define Elem		(make_tag(2))
#define DeadElem	(make_tag(3))

static value random_genc(e)
value e;
{
	int g = (CINT(elem_genc(e))>>8);
	return (random()%3) < g;
}

static void choose_genc(e, rp)
value e, *rp;
{
	if (elem_tag(e) >= Elem && random_genc(e)) (*rp)++;
}

static value baby_genc(b, i, j, width, height)
value b;
int i, j, width, height;
{
	value v, vm1, vp1, e, r;
	int jm1, jp1;
	v = FIELD(b, i);
	vm1 = FIELD(b, i>0 ? i-1 : width-1);
	vp1 = FIELD(b, i<width-1 ? i+1 : 0);
	jm1 = j > 0 ? j-1 : height-1;
	jp1 = j < height-1 ? j+1 : 0;
	r = 0;
	choose_genc(FIELD(vm1, jm1), &r);
	choose_genc(FIELD(vm1, j  ), &r);
	choose_genc(FIELD(vm1, jp1), &r);
	choose_genc(FIELD(v  , jm1), &r);
	choose_genc(FIELD(v  , jp1), &r);
	choose_genc(FIELD(vp1, jm1), &r);
	choose_genc(FIELD(vp1, j  ), &r);
	choose_genc(FIELD(vp1, jp1), &r);
	return make_genc(r);
}

value ML_c_one_step(b, w, h)
value b, w, h;
{
	int width, height, i, j, modif;
	value v, e, r;
	width = CINT(w);
	height = CINT(h);
	modif = 0;
	for (i = 0; i < width; i++) {
		v = FIELD(b, i);
		for (j = 0; j < height; j++) {
			e = FIELD(v, j);
			switch (elem_sum(e)) {
			case make_sum(3):
				if (elem_tag(e) == NoElem) {
					r = baby_genc(b,i,j,width,height);
					r = MLOR(r, elem_sum(e));
					FIELD(v, j) = MLOR(BornElem,r);
					modif = 1;
				}
				break;
			case make_sum(0):
			case make_sum(1):
			case make_sum(4):
			case make_sum(5):
			case make_sum(6):
			case make_sum(7):
			case make_sum(8):
				if (elem_tag(e) == Elem) {
					r = elem_no_tag(e);
					FIELD(v, j) = MLOR(DeadElem,r);
					modif = 1;
				}
				break;
			}
		}
	}
#if 0
	for (i = 0; i < width; i++) {
		v = FIELD(b, i);
		for (j = 0; j < height; j++) {
			e = FIELD(v, j);
			if (elem_tag(e) == Elem && random()%1000 == 0) {
				r = make_genc(random()%4);
				FIELD(v, j) = MLOR(elem_no_genc(e), r);
				modif = 1;
			}
		}
	}
#endif
	return Val_int(modif);
}

#if 1
value ML_c_update(b, w, h)
value b, w, h;
{
	int width, height, i, j, jm1, jp1, modif;
	value vm1, v, vp1, r;
	width = CINT(w);
	height = CINT(h);
	modif = 0;
	vm1 = FIELD(b, width-1);
	v = FIELD(b, 0);
	for (i = 0; i < width; i++) {
		vp1 = FIELD(b, (i == width-1) ? 0 : i+1);
		jm1 = height-1;
		for (j = 0; j < height; j++) {
			jp1 = (j == height-1) ? 0 : j+1;
			switch (elem_tag(FIELD(v, j))) {
			case BornElem:
				r = elem_no_tag(FIELD(v, j));
				FIELD(vm1, jm1) += MLINCR(1);
				FIELD(vm1, j  ) += MLINCR(1);
				FIELD(vm1, jp1) += MLINCR(1);
				FIELD(v  , jm1) += MLINCR(1);
				FIELD(v  , j  )  = MLOR(Elem, r);
				FIELD(v  , jp1) += MLINCR(1);
				FIELD(vp1, jm1) += MLINCR(1);
				FIELD(vp1, j  ) += MLINCR(1);
				FIELD(vp1, jp1) += MLINCR(1);
				modif = 1;
				break;
			case DeadElem:
				r = elem_no_tag(FIELD(v, j));
				FIELD(vm1, jm1) -= MLINCR(1);
				FIELD(vm1, j  ) -= MLINCR(1);
				FIELD(vm1, jp1) -= MLINCR(1);
				FIELD(v  , jm1) -= MLINCR(1);
				FIELD(v  , j  )  = MLOR(NoElem, r);
				FIELD(v  , jp1) -= MLINCR(1);
				FIELD(vp1, jm1) -= MLINCR(1);
				FIELD(vp1, j  ) -= MLINCR(1);
				FIELD(vp1, jp1) -= MLINCR(1);
				modif = 1;
				break;
			}
			jm1 = j;
		}
		vm1 = v;
		v = vp1;
	}
	return Val_int(modif);
}
#else
value ML_c_update(b, w, h)
value b, w, h;
{
	int width, height, i, j, jm1, jp1, modif;
	value vm1, v, vp1, r;
	width = CINT(w);
	height = CINT(h);
	modif = 0;
	vm1 = FIELD(b, width-1);
	v = FIELD(b, 0);
	for (i = 0; i < width; i++) {
		vp1 = FIELD(b, (i == width-1) ? 0 : i+1);
		jm1 = height-1;
		for (j = 0; j < height; j++) {
			jp1 = (j == height-1) ? 0 : j+1;
			switch (elem_tag(FIELD(v, j))) {
			case BornElem:
				r = elem_no_tag(FIELD(v, j));
				if (i > 0) {
				  if (j > 0)
				    FIELD(vm1, jm1) += MLINCR(1);
				  FIELD(vm1, j  ) += MLINCR(1);
				  if (j < height - 1)
				    FIELD(vm1, jp1) += MLINCR(1);
				}
				if (j > 0)
				  FIELD(v  , jm1) += MLINCR(1);
				FIELD(v  , j  )  = MLOR(Elem, r);
				if (j < height - 1)
				  FIELD(v  , jp1) += MLINCR(1);
				if (i < width - 1) {
				  if (j > 0)
				    FIELD(vp1, jm1) += MLINCR(1);
				  FIELD(vp1, j  ) += MLINCR(1);
				  if (j < height - 1)
				    FIELD(vp1, jp1) += MLINCR(1);
				}
				modif = 1;
				break;
			case DeadElem:
				r = elem_no_tag(FIELD(v, j));
				if (i > 0) {
				  if (j > 0)
				    FIELD(vm1, jm1) -= MLINCR(1);
				  FIELD(vm1, j  ) -= MLINCR(1);
				  if (j < height - 1)
				    FIELD(vm1, jp1) -= MLINCR(1);
				}
				if (j > 0)
				  FIELD(v  , jm1) -= MLINCR(1);
				FIELD(v  , j  )  = MLOR(NoElem, r);
				if (j < height - 1)
				  FIELD(v  , jp1) -= MLINCR(1);
				if (i < width - 1) {
				  if (j > 0)
				    FIELD(vp1, jm1) -= MLINCR(1);
				  FIELD(vp1, j  ) -= MLINCR(1);
				  if (j < height - 1)
				    FIELD(vp1, jp1) -= MLINCR(1);
				}
				modif = 1;
				break;
			}
			jm1 = j;
		}
		vm1 = v;
		v = vp1;
	}
	return Val_int(modif);
}
#endif

value ML_c_fill(b, w, h, e)
value b, w, h, e;
{
	int i, j, width, height;
	value v;
	width = CINT(w);
	height = CINT(h);
	for (i = 0; i < width; i++) {
		v = FIELD(b, i);
		for (j = 0; j < height; j++) FIELD(v, j) = e;
	}
	return Val_unit;
}
