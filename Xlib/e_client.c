/* $Id: e_client.c,v 1.1 2006/05/08 19:48:38 deraugla Exp $ */

#include "stub.h"

value ML_XEvent_xclient(v)
value v;
{
	return Val_addr(&((XEvent *)aar())->xclient);
}

value ML_XClientMessageEvent_data_l_0(v)
value v;
{
	return Val_long(((XClientMessageEvent *)aar())->data.l[0]);
}

value ML_XClientMessageEvent_message_type(v)
value v;
{
	return Val_long(((XClientMessageEvent *)aar())->message_type);
}
