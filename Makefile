all:
	cd Xft; $(MAKE)
	cd Xlib; $(MAKE)
	cd rt; $(MAKE)

clean:
	cd Xft; $(MAKE) clean
	cd Xlib; $(MAKE) clean
	cd rt; $(MAKE) clean

include config/Makefile.cnf
