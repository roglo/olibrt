all:
	cd Xlib; $(MAKE)
	cd rt; $(MAKE)

clean:
	cd Xlib; $(MAKE) clean
	cd rt; $(MAKE) clean

include config/Makefile.cnf
