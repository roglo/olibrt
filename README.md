VERSION 1.12-exp

TO COMPILE

You need ocaml and camlp5.

   ./configure
   make

GAMES

   cd examples; make

ISSUES

On Mac, when linking an application which uses olibrt, if you get the
message "file was built for archive with is not the architecture...",
edit the Xlib/Makefile and change the line:
   CFLAGS=-I $(OLIBDIR)
into:
   CFLAGS=-I $(OLIBDIR) -m32
then do:
   make clean
   make
