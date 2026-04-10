# $Id: Makefile,v 1.10 2017/03/11 10:27:23 deraugla Exp $

OBJS=std.cmo xlib.cmo time.cmo mousefont.cmo font.cmo motif.cmo util.cmo keysym.cmo rt_main.cmo misc.cmo rt_color.cmo c_arrow.cmo c_button.cmo c_check.cmo c_line.cmo c_pack.cmo c_raw.cmo c_scroll.cmo c_table.cmo termdef.cmo c_term.cmo term.cmo c_title.cmo mouse.cmo draw.cmo rt_image.cmo pattern.cmo pixmap.cmo resource.cmo uinfo.cmo rt.cmo rtdef.cmo
MLTARG=rtdecl.mli keysym.ml rt.mli rtdef.mli
TEST=rtest.cmo run.cmo
RTLIB=librt.cma
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
CAMLP5=camlp5r

all: $(RTLIB) $(RTLIB:.cma=.cmxa)

clean:
	rm -rf *.[ao] *.cm[aiox] *.cmxa *.out
	rm -f $(MLTARG)

depend:
	export LC_ALL=C; for i in $$(ls *.mli *.ml); do camlp5r pr_depend.cmo $$i; done > .depend.new
	mv .depend.new .depend

test_xlib.out: test_xlib.ml
	ocamlc -custom -pp camlp5r -o $@ librt.cma $< ../Xlib/libx.a ../Xft/libxft.a -cclib -lX11 -cclib -lXft -cclib -lXrandr

test_rt.out: rtf.cmo test_rt.ml
	ocamlc -custom -pp camlp5r -o $@ librt.cma rtf.cmo test_rt.ml ../Xlib/libx.a ../Xft/libxft.a -cclib -lX11 -cclib -lXft -cclib -lXrandr

$(RTLIB): $(MLTARG) $(OBJS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(RTLIB:.cma=.cmxa): $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $(OBJS:.cmo=.cmx)

rt.mli: rt.mlip rtdecl.mlip rt.mlih
	@/bin/rm -f $@
	cpp -P rt.mlip | sed -e "s/open Rtdecl;//" -e "s/open Keysym;//" | cat -s > $@
	@chmod -w $@

rtdef.mli: rtdef.mlip rtdecl.mlip rt.mlih
	@/bin/rm -f $@
	cpp -P rtdef.mlip | sed -e "s/open Rtdecl;//" -e "s/open Keysym;//" | cat -s > $@
	@chmod -w $@

rt.mli rtdef.mli: keysym.mli rt_main.mli c_arrow.mli c_button.mli c_check.mli c_pack.mli c_scroll.mli c_table.mli c_term.mli term.mli c_title.mli c_raw.mli pixmap.mli rt_color.mli pattern.mli draw.mli rt_image.mli mouse.mli font.mli resource.mli misc.mli uinfo.mli mousefont.mli time.mli

%.cmo: %.ml
	$(OCAMLC) -pp $(CAMLP5) -c $(OCFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -pp $(CAMLP5) -c $(OCFLAGS) $<

%.cmi: %.mli
	$(OCAMLC) -pp $(CAMLP5) -c $(OCFLAGS) $<

%.ml: %.mlp
	@/bin/rm -f $@
	cpp -I/usr/X11R6/include -P $< | cat -s > $@
	@chmod -w $@

%.mli: %.mlip
	@/bin/rm -f $@
	cpp -I/usr/X11R6/include -P $< | cat -s > $@
	@chmod -w $@

include ../config/Makefile.cnf
include .depend
