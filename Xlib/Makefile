# $Id: Makefile,v 1.10 2010/08/03 13:40:10 deraugla Exp $

OBJS=XAllCells.o XBackgnd.o XBell.o XChWAttrs.o XChWindow.o XChKeyCon.o XChkMaskEv.o XClDisplay.o XClear.o XClearArea.o XConfWind.o XCopyArea.o XCrBFData.o XCrColorm.o XCrCursor.o XCrGC.o XCrPFBData.o XCrPixmap.o XCrWindow.o XCursor.o XDefCursor.o XDestWind.o XDisName.o XDrArc.o XDrLine.o XDrLines.o XDrPoint.o XDrRect.o XFillArc.o XFillPoly.o XFillRect.o XFlush.o XFont.o XFontInfo.o XFreeCurs.o XFreeGC.o XFreePix.o XGCMisc.o XGetColor.o XGetDflt.o XGetHColor.o XGetImage.o XGetKCnt.o XImText.o XImUtil.o XIntAtom.o XKeyBind.o XMapRaised.o XMapSubs.o XMapWindow.o XMoveWin.o XNextEvent.o XOpenDis.o XPending.o XPmapBgnd.o XPmapBord.o XPutImage.o XQuColor.o XQuPntr.o XRaiseWin.o XReconfWin.o XRepWindow.o XSelInput.o XSetBack.o XBorder.o XSetCDMode.o XSetClMask.o XSetClOrig.o XSetFont.o XSetFore.o XSetHints.o XSetIFocus.o XSetLStyle.o XSetPlMask.o XSetSOwner.o XSetStip.o XSetTile.o XSetTSOrig.o XSetWMPr.o XStBytes.o XStColor.o XStName.o XStrKeysym.o XSync.o XText.o XTextExt.o XUndefCurs.o XUnmapWin.o convert.o e_any.o e_button.o e_client.o e_config.o e_crossing.o e_expose.o e_focus.o e_graphics.o e_key.o e_mapping.o e_motion.o event.o macros.o s_atom.o s_color.o s_fontst.o s_kbcont.o s_kbstate.o s_point.o s_szhint.o s_visual.o s_wmhint.o s_xgcval.o s_ximage.o select.o timeb.o xlib.o
CFLAGS=-I $(OLIBDIR)

all: libx.a

clean:
	rm -f *.[ao] config.h

libx.a: config.h $(OBJS)
	rm -f $@
	ar qc $@ $(OBJS)

config.h:
	cat < /dev/null > config.h

include ../config/Makefile.cnf
