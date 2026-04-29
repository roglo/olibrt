TOP=../../.
CURRENT_DIR=./examples/budget
AR=ar
ARFLAGS=qc
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
CAMLLINK=ocamlc.opt
CAMLLIBR=ocamlc.opt -a
CAMLOPTLIBR=ocamlopt -a
CAMLMKTOP=ocamlmktop
CAMLP5=camlp5r
CC=gcc
ZDEP=ocamldep -pp $(CAMLP5)
RTDIR=$(TOP)/rt
RTLIB=$(RTDIR)/librt.cma
XLIB=$(TOP)/Xlib/$(ARCH)/libx.a
CONFIG=$(TOP)/config

ARCH=.

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib/ocaml
LIBX11=-cclib -L/usr/X11R6/lib -cclib -lX11 -cclib -lXft -cclib -lXrandr
OLIBDIR=/usr/local/lib/ocaml

OLIBDIR=/home/roglo/work/usr/lib/ocaml

ZDEPFLAGS=-I $(RTDIR)
ZFLAGS=-I $(RTDIR) -I `camlp5 -where`
CFLAGS=-O -I$(OLIBDIR) -I/usr/X11R6/include

# (begin Imakefile)

# $Id: Makefile,v 1.1 2010/11/17 17:22:09 deraugla Exp $

OCAMLC=ocamlc.opt -g
CAMLLINK=ocamlc.opt -g
TARGET=$(ARCH)/budget.out
ZOFILES=rtN.cmo jmage.cmo file.cmo const.cmo state.cmo show.cmo action.cmo ajoutModif.cmo d_listeMois.cmo d_imprimer.cmo d_ajout.cmo d_transf.cmo d_auto.cmo d_nouvMois.cmo d_modif.cmo d_suppr.cmo d_soldeRed.cmo d_repartMois.cmo d_repartPoste.cmo d_soldeGen.cmo d_bilan.cmo d_infoPostes.cmo d_infoCartes.cmo d_infoComptes.cmo d_calculat.cmo d_blocnotes.cmo d_majPostes.cmo d_majCartes.cmo d_majComptes.cmo d_ajoutAuto.cmo d_modifAuto.cmo d_supprAuto.cmo d_majAuto.cmo b_compte.cmo b_pointage.cmo b_init.cmo a_entree.cmo c_popup.cmo budget.cmo main.cmo
XLIB=$(TOP)/Xlib/$(ARCH)/libx.a $(TOP)/Xft/XftFont.o
EXT=pa_r.cmo ./pa_ext.cmo
EXT=pa_r.cmo
CAMLP5="camlp5 $(EXT) pa_rp.cmo pr_dump.cmo"

all:: $(TARGET)

$(TARGET): $(RTLIB) $(XLIB) $(ZOFILES)
	$(CAMLLINK) -custom $(TOP)/rt/xft.cmo $(RTLIB) "unix.cma" -I `camlp5 -where` "gramlib.cma" $(ZOFILES) $(XLIB) $(LIBX11) \
 -o $@

clean::
	rm -f $(TARGET)

depend::
	rm -f Imakefile.bak
	cp Imakefile Imakefile.bak
	sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Imakefile.bak \
		> Imakefile
	$(ZDEP) $(ZDEPFLAGS) $$(ls *.ml *.mli | LC_ALL=C sort) >> Imakefile

### DO NOT DELETE THIS LINE
a_entree.cmo: state.cmi rtN.cmi b_pointage.cmi b_init.cmi b_compte.cmi     action.cmi a_entree.cmi

a_entree.cmx: state.cmx rtN.cmx b_pointage.cmx b_init.cmx b_compte.cmx     action.cmx a_entree.cmi

a_entree.cmi: rtN.cmi
action.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi action.cmi
action.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx action.cmi
action.cmi: rtN.cmi jmage.cmi
ajoutModif.cmo: state.cmi show.cmi rtN.cmi jmage.cmi file.cmi ajoutModif.cmi
ajoutModif.cmx: state.cmx show.cmx rtN.cmx jmage.cmx file.cmx ajoutModif.cmi
ajoutModif.cmi: jmage.cmi
b_compte.cmo: state.cmi show.cmi rtN.cmi file.cmi d_transf.cmi d_suppr.cmi     d_soldeGen.cmi d_repartPoste.cmi d_modif.cmi d_majPostes.cmi     d_majComptes.cmi d_majCartes.cmi d_majAuto.cmi d_bilan.cmi d_auto.cmi     d_ajout.cmi action.cmi b_compte.cmi

b_compte.cmx: state.cmx show.cmx rtN.cmx file.cmx d_transf.cmx d_suppr.cmx     d_soldeGen.cmx d_repartPoste.cmx d_modif.cmx d_majPostes.cmx     d_majComptes.cmx d_majCartes.cmx d_majAuto.cmx d_bilan.cmx d_auto.cmx     d_ajout.cmx action.cmx b_compte.cmi

b_compte.cmi: rtN.cmi
b_init.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi file.cmi     action.cmi b_init.cmi

b_init.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx file.cmx     action.cmx b_init.cmi

b_init.cmi: rtN.cmi
b_pointage.cmo: state.cmi show.cmi rtN.cmi file.cmi action.cmi b_pointage.cmi
b_pointage.cmx: state.cmx show.cmx rtN.cmx file.cmx action.cmx b_pointage.cmi
b_pointage.cmi: rtN.cmi
budget.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi file.cmi     d_transf.cmi d_soldeRed.cmi d_repartPoste.cmi d_repartMois.cmi     d_nouvMois.cmi d_modifAuto.cmi d_modif.cmi d_majPostes.cmi     d_majComptes.cmi d_majCartes.cmi d_listeMois.cmi d_infoPostes.cmi     d_infoComptes.cmi d_infoCartes.cmi d_calculat.cmi d_blocnotes.cmi     d_ajoutAuto.cmi d_ajout.cmi c_popup.cmi b_pointage.cmi a_entree.cmi

budget.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx file.cmx     d_transf.cmx d_soldeRed.cmx d_repartPoste.cmx d_repartMois.cmx     d_nouvMois.cmx d_modifAuto.cmx d_modif.cmx d_majPostes.cmx     d_majComptes.cmx d_majCartes.cmx d_listeMois.cmx d_infoPostes.cmx     d_infoComptes.cmx d_infoCartes.cmx d_calculat.cmx d_blocnotes.cmx     d_ajoutAuto.cmx d_ajout.cmx c_popup.cmx b_pointage.cmx a_entree.cmx

c_popup.cmo: state.cmi rtN.cmi file.cmi d_transf.cmi d_supprAuto.cmi     d_suppr.cmi d_soldeRed.cmi d_soldeGen.cmi d_repartPoste.cmi     d_repartMois.cmi d_nouvMois.cmi d_modifAuto.cmi d_modif.cmi     d_majPostes.cmi d_majComptes.cmi d_majCartes.cmi d_majAuto.cmi     d_listeMois.cmi d_infoPostes.cmi d_infoComptes.cmi d_infoCartes.cmi     d_imprimer.cmi d_calculat.cmi d_blocnotes.cmi d_bilan.cmi d_auto.cmi     d_ajoutAuto.cmi d_ajout.cmi b_compte.cmi action.cmi c_popup.cmi

c_popup.cmx: state.cmx rtN.cmx file.cmx d_transf.cmx d_supprAuto.cmx     d_suppr.cmx d_soldeRed.cmx d_soldeGen.cmx d_repartPoste.cmx     d_repartMois.cmx d_nouvMois.cmx d_modifAuto.cmx d_modif.cmx     d_majPostes.cmx d_majComptes.cmx d_majCartes.cmx d_majAuto.cmx     d_listeMois.cmx d_infoPostes.cmx d_infoComptes.cmx d_infoCartes.cmx     d_imprimer.cmx d_calculat.cmx d_blocnotes.cmx d_bilan.cmx d_auto.cmx     d_ajoutAuto.cmx d_ajout.cmx b_compte.cmx action.cmx c_popup.cmi

c_popup.cmi: rtN.cmi
const.cmo: const.cmi
const.cmx: const.cmi
d_ajout.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi file.cmi     ajoutModif.cmi action.cmi d_ajout.cmi

d_ajout.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx file.cmx     ajoutModif.cmx action.cmx d_ajout.cmi

d_ajout.cmi: rtN.cmi
d_ajoutAuto.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi     file.cmi action.cmi d_ajoutAuto.cmi

d_ajoutAuto.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx     file.cmx action.cmx d_ajoutAuto.cmi

d_ajoutAuto.cmi: rtN.cmi
d_auto.cmo: state.cmi show.cmi rtN.cmi file.cmi action.cmi d_auto.cmi
d_auto.cmx: state.cmx show.cmx rtN.cmx file.cmx action.cmx d_auto.cmi
d_auto.cmi: rtN.cmi
d_bilan.cmo: state.cmi show.cmi rtN.cmi jmage.cmi file.cmi action.cmi     d_bilan.cmi

d_bilan.cmx: state.cmx show.cmx rtN.cmx jmage.cmx file.cmx action.cmx     d_bilan.cmi

d_bilan.cmi: rtN.cmi
d_blocnotes.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi     file.cmi action.cmi d_blocnotes.cmi

d_blocnotes.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx     file.cmx action.cmx d_blocnotes.cmi

d_blocnotes.cmi: rtN.cmi
d_calculat.cmo: rtN.cmi ../.././rt/rt.cmi d_calculat.cmi
d_calculat.cmx: rtN.cmx ../.././rt/rt.cmx d_calculat.cmi
d_calculat.cmi: rtN.cmi
d_comptesSpe.cmo: state.cmi rtN.cmi jmage.cmi file.cmi const.cmi action.cmi
d_comptesSpe.cmx: state.cmx rtN.cmx jmage.cmx file.cmx const.cmx action.cmx
d_imprimer.cmo: state.cmi jmage.cmi file.cmi const.cmi d_imprimer.cmi
d_imprimer.cmx: state.cmx jmage.cmx file.cmx const.cmx d_imprimer.cmi
d_imprimer.cmi: rtN.cmi
d_infoCartes.cmo: show.cmi rtN.cmi file.cmi action.cmi d_infoCartes.cmi
d_infoCartes.cmx: show.cmx rtN.cmx file.cmx action.cmx d_infoCartes.cmi
d_infoCartes.cmi: rtN.cmi
d_infoComptes.cmo: show.cmi rtN.cmi file.cmi const.cmi action.cmi     d_infoComptes.cmi

d_infoComptes.cmx: show.cmx rtN.cmx file.cmx const.cmx action.cmx     d_infoComptes.cmi

d_infoComptes.cmi: rtN.cmi
d_infoPostes.cmo: show.cmi rtN.cmi file.cmi action.cmi d_infoPostes.cmi
d_infoPostes.cmx: show.cmx rtN.cmx file.cmx action.cmx d_infoPostes.cmi
d_infoPostes.cmi: rtN.cmi
d_listeMois.cmo: state.cmi show.cmi rtN.cmi jmage.cmi file.cmi action.cmi     d_listeMois.cmi

d_listeMois.cmx: state.cmx show.cmx rtN.cmx jmage.cmx file.cmx action.cmx     d_listeMois.cmi

d_listeMois.cmi: rtN.cmi
d_majAuto.cmo: state.cmi show.cmi rtN.cmi d_supprAuto.cmi d_modifAuto.cmi     d_ajoutAuto.cmi action.cmi d_majAuto.cmi

d_majAuto.cmx: state.cmx show.cmx rtN.cmx d_supprAuto.cmx d_modifAuto.cmx     d_ajoutAuto.cmx action.cmx d_majAuto.cmi

d_majAuto.cmi: rtN.cmi
d_majCartes.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi     file.cmi action.cmi d_majCartes.cmi

d_majCartes.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx     file.cmx action.cmx d_majCartes.cmi

d_majCartes.cmi: rtN.cmi
d_majComptes.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi     file.cmi const.cmi action.cmi d_majComptes.cmi

d_majComptes.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx     file.cmx const.cmx action.cmx d_majComptes.cmi

d_majComptes.cmi: rtN.cmi
d_majPostes.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi     file.cmi action.cmi d_majPostes.cmi

d_majPostes.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx     file.cmx action.cmx d_majPostes.cmi

d_majPostes.cmi: rtN.cmi
d_modif.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi file.cmi     ajoutModif.cmi action.cmi d_modif.cmi

d_modif.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx file.cmx     ajoutModif.cmx action.cmx d_modif.cmi

d_modif.cmi: rtN.cmi
d_modifAuto.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi     file.cmi action.cmi d_modifAuto.cmi

d_modifAuto.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx     file.cmx action.cmx d_modifAuto.cmi

d_modifAuto.cmi: rtN.cmi
d_nouvMois.cmo: state.cmi show.cmi rtN.cmi jmage.cmi file.cmi d_imprimer.cmi     d_auto.cmi action.cmi d_nouvMois.cmi

d_nouvMois.cmx: state.cmx show.cmx rtN.cmx jmage.cmx file.cmx d_imprimer.cmx     d_auto.cmx action.cmx d_nouvMois.cmi

d_nouvMois.cmi: rtN.cmi
d_repartMois.cmo: rtN.cmi action.cmi d_repartMois.cmi
d_repartMois.cmx: rtN.cmx action.cmx d_repartMois.cmi
d_repartMois.cmi: rtN.cmi
d_repartPoste.cmo: state.cmi show.cmi rtN.cmi jmage.cmi file.cmi action.cmi     d_repartPoste.cmi

d_repartPoste.cmx: state.cmx show.cmx rtN.cmx jmage.cmx file.cmx action.cmx     d_repartPoste.cmi

d_repartPoste.cmi: rtN.cmi
d_soldeGen.cmo: state.cmi show.cmi rtN.cmi jmage.cmi file.cmi const.cmi     action.cmi d_soldeGen.cmi

d_soldeGen.cmx: state.cmx show.cmx rtN.cmx jmage.cmx file.cmx const.cmx     action.cmx d_soldeGen.cmi

d_soldeGen.cmi: rtN.cmi
d_soldeRed.cmo: rtN.cmi action.cmi d_soldeRed.cmi
d_soldeRed.cmx: rtN.cmx action.cmx d_soldeRed.cmi
d_soldeRed.cmi: rtN.cmi
d_suppr.cmo: state.cmi show.cmi rtN.cmi file.cmi action.cmi d_suppr.cmi
d_suppr.cmx: state.cmx show.cmx rtN.cmx file.cmx action.cmx d_suppr.cmi
d_suppr.cmi: rtN.cmi
d_supprAuto.cmo: state.cmi show.cmi rtN.cmi file.cmi action.cmi     d_supprAuto.cmi

d_supprAuto.cmx: state.cmx show.cmx rtN.cmx file.cmx action.cmx     d_supprAuto.cmi

d_supprAuto.cmi: rtN.cmi
d_transf.cmo: state.cmi show.cmi rtN.cmi ../.././rt/rt.cmi jmage.cmi file.cmi     action.cmi d_transf.cmi

d_transf.cmx: state.cmx show.cmx rtN.cmx ../.././rt/rt.cmx jmage.cmx file.cmx     action.cmx d_transf.cmi

d_transf.cmi: rtN.cmi
file.cmo: file.cmi
file.cmx: file.cmi
jmage.cmo: rtN.cmi jmage.cmi
jmage.cmx: rtN.cmx jmage.cmi
jmage.cmi: rtN.cmi
main.cmo: file.cmi budget.cmo
main.cmx: file.cmx budget.cmx
rtN.cmo: ../.././rt/rt.cmi rtN.cmi
rtN.cmx: ../.././rt/rt.cmx rtN.cmi
rtN.cmi: ../.././rt/rt.cmi
show.cmo: state.cmi rtN.cmi jmage.cmi file.cmi show.cmi
show.cmx: state.cmx rtN.cmx jmage.cmx file.cmx show.cmi
show.cmi: rtN.cmi
state.cmo: rtN.cmi ../.././rt/rt.cmi jmage.cmi file.cmi state.cmi
state.cmx: rtN.cmx ../.././rt/rt.cmx jmage.cmx file.cmx state.cmi
state.cmi: rtN.cmi ../.././rt/rt.cmi jmage.cmi file.cmi

# (end Imakefile)

clean scratch::
	rm -f *.run *.out *.cm[iox] *.o *.a *.bak *~ *.log

install::

rminstall::

depend:: Makefile

save::
	-ci -q -m"- sauvegarde" RCS/*,v
restore::
	-co -q RCS/*,v

rcs_release::
	-rcs -q -sRel RCS/*,v

Makefiles::

deleteMakefiles::

%.cmo: %.ml
	$(OCAMLC) -pp $(CAMLP5) -c $(ZFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -pp $(CAMLP5) -c $(ZFLAGS) $<

%.cmi: %.mli
	$(OCAMLC) -pp $(CAMLP5) -c $(ZFLAGS) $<

%.ml: %.mlp
	@/bin/rm -f $@
	cpp -I/usr/X11R6/include -P $< | cat -s > $@
	@chmod -w $@

%.mli: %.mlip
	@/bin/rm -f $@
	cpp -I/usr/X11R6/include -P $< | cat -s > $@
	@chmod -w $@

%.out: %.c
	$(CC) $(CFLAGS) $< -o $@

