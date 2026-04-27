# $Id: Makefile,v 1.10 2010/08/03 13:40:10 deraugla Exp $

OBJS=XftFont.o
CFLAGS=-I $(OLIBDIR) -I/usr/include/freetype2

all: libxft.a

clean:
	rm -f *.[ao] config.h

test.out: test.c
	gcc test.c -o test.out -lX11 -lXft -lXrandr -lfontconfig -I/usr/include/freetype2

libxft.a: config.h $(OBJS)
	rm -f $@
	ar qc $@ $(OBJS)

config.h:
	cat < /dev/null > config.h

include ../config/Makefile.cnf
