/* $Id: XNextEvent.c,v 1.3 2009/01/27 09:40:01 deraugla Exp $ */

#include "stub.h"
#include <string.h>
#include <stdio.h>

value ML_XNextEvent(v)
value *v;
{
  Display *dpy = (Display *)aarv(0);
  XEvent *xev, xev2;
  Window win;
  xev = (XEvent *)aarv(1);
  XNextEvent(dpy, xev);
  if (xev->xany.type == ConfigureNotify) {
    win = xev->xany.window;
    while (XCheckWindowEvent(dpy, win, StructureNotifyMask, &xev2)) {
      memcpy(xev, &xev2, sizeof(XEvent));
    }
  }
  return unit;
}
