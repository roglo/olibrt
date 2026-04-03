/* text generated automatically by mkstub */

#include "stub.h"

value ML_XQueryPointer(v)
value *v;
{
	Window root_return;
	Window child_return;
	int root_x_return;
	int root_y_return;
	int win_x_return;
	int win_y_return;
	unsigned int mask_return;
	Bool r = XQueryPointer(
		(Display*) aarv(0),
		(Window) iarv(1),
		&root_return,
		&child_return,
		&root_x_return,
		&root_y_return,
		&win_x_return,
		&win_y_return,
		&mask_return
	);
	Field(v[2], 0) = Val_long(root_return);
	Field(v[3], 0) = Val_long(child_return);
	Field(v[4], 0) = Val_long(root_x_return);
	Field(v[5], 0) = Val_long(root_y_return);
	Field(v[6], 0) = Val_long(win_x_return);
	Field(v[7], 0) = Val_long(win_y_return);
	Field(v[8], 0) = Val_long(mask_return);
	return Val_long(r);
}
