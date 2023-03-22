
#
# Makefile 
#
prog_root = .
include Mkinclude
#
LIBSRCS = libsrc
SUBDIRS = $(LIBSRCS) gt2nc
# 
# 

BIN_GTTHETA	= gttheta_p
OBJ_GTTHETA	= util.o gttheta_p.o

BIN_GTTHETE	= gtthetae_p
OBJ_GTTHETE	= util.o gtthetae_p.o

BIN_GTRELH	= gtrelh_p
OBJ_GTRELH	= util.o gtrelh_p.o

BIN_INTP2TH	= gtintp2th
OBJ_INTP2TH	= util.o util_pvont.o gtintp2th.o

BIN_PVONT	= gtpvont
OBJ_PVONT	= util.o util_pvont.o gtpvont.o

BIN_PVSFC	= gtpvsfc
OBJ_PVSFC	= util.o util_steflux.o gtpvsfc.o

BIN_GTPS	= gtps
OBJ_GTPS	= util.o gtps.o

BIN_GTPS2P	= gtps2p
OBJ_GTPS2P	= util.o gtps2p.o

BIN_GTCOL	= gtcol
OBJ_GTCOL	= util.o gtcol.o

BIN_GTCOLV	= gtcolvar
OBJ_GTCOLV	= util.o gtcolvar.o

BIN_GTDOB	= gtdob
OBJ_GTDOB	= util.o gtdob.o

BIN_GTDOBL	= gtdobl
OBJ_GTDOBL	= util.o gtdobl.o

BIN_GTTROP	= gttrop
OBJ_GTTROP	= util.o util_steflux.o gttrop.o

BIN_GTSTAB	= gtstab
OBJ_GTSTAB	= util.o gtstab.o

BIN_GTMAGE	= gtmage
OBJ_GTMAGE	= util.o gtmage.o

BIN_GTPSC	= gtpsc
OBJ_GTPSC	= util.o gtpsc.o

BIN_GTPSCT	= gtpsct
OBJ_GTPSCT	= util.o gtpsct.o

BIN_GTMASS	= gtairmass
OBJ_GTMASS	= gtairmass.o

BIN_GTMASV	= gtairmassv
OBJ_GTMASV	= util.o gtairmassv.o

BIN_GTINFO	= gtinfo
OBJ_GTINFO	= util.o gtinfo.o

BIN_GTITEM	= gtitem
OBJ_GTITEM	= gtitem.o

BIN_GT2TXT	= gt2txt
OBJ_GT2TXT	= util.o gt2txt.o

BIN_HISTCHK	= histchk
OBJ_HISTCHK	= histchk.o

BIN_GTEPF	= gtepflux
OBJ_GTEPF	= util.o util_epf.o gtepflux.o

BIN_GTEPFW	= gtepfluxwn
OBJ_GTEPFW	= util.o util_epf.o gtepfluxwn.o

BIN_GTWSTA	= gtwsta
OBJ_GTWSTA	= util.o util_epf.o gtwsta.o

BIN_GTO2W	= gtomg2w
OBJ_GTO2W	= util.o gtomg2w.o

BIN_GTCUT	= gtcut
OBJ_GTCUT	= util.o gtcut.o

BIN_GTJOIN	= gtjoinz
OBJ_GTJOIN	= util.o gtjoinz.o

BIN_GTFACT	= gtfact
OBJ_GTFACT	= gtfact.o

BIN_GTRANG	= gtrange
OBJ_GTRANG	= util.o gtrange.o

BIN_GTMMON	= gtmmon
OBJ_GTMMON	= util.o gtmmon.o

BIN_GTMDAY      = gtmday
OBJ_GTMDAY      = util.o gtmday.o

BIN_GTRPLC	= gtreplace
OBJ_GTRPLC	= gtreplace.o

BIN_GTRPLI	= gtrepint
OBJ_GTRPLI	= util.o gtrepint.o

BIN_GTCNST	= gtconst
OBJ_GTCNST	= util.o gtconst.o

BIN_GTZONL	= gtzonal
OBJ_GTZONL	= gtzonal.o

BIN_GTCONV	= gtconv
OBJ_GTCONV	= util.o gtconv.o

BIN_GTTSM	= gttsm
OBJ_GTTSM	= util.o gttsm.o

BIN_GTSTE	= gtsteflux
OBJ_GTSTE	= util.o util_steflux.o gtsteflux.o

BIN_GTDELT	= gtdelt
OBJ_GTDELT	= util.o gtdelt.o

BIN_GTRUN	= gtrunmean
OBJ_GTRUN	= util.o util_runmean.o gtrunmean.o

BINS = $(BIN_GTTHETA) $(BIN_GTTHETE) $(BIN_GTRELH) $(BIN_INTP2TH) \
$(BIN_PVONT) $(BIN_PVSFC) $(BIN_GTPS) $(BIN_GTPS2P) \
$(BIN_GTCOL) $(BIN_GTCOLV) $(BIN_GTDOB) $(BIN_GTDOBL) $(BIN_GTTROP) $(BIN_GTSTAB) \
$(BIN_GTMAGE) $(BIN_GTPSC) $(BIN_GTPSCT) $(BIN_GTMASS) $(BIN_GTMASV) \
$(BIN_GTINFO) $(BIN_GTITEM) $(BIN_GT2TXT) $(BIN_HISTCHK) \
$(BIN_GTEPF) $(BIN_GTEPFW) $(BIN_GTWSTA) $(BIN_GTO2W) \
$(BIN_GTCUT) $(BIN_GTJOIN) $(BIN_GTFACT) $(BIN_GTRANG) $(BIN_GTMMON) $(BIN_GTMDAY) \
$(BIN_GTRPLC) $(BIN_GTRPLI) $(BIN_GTCNST) $(BIN_GTZONL) \
$(BIN_GTCONV) $(BIN_GTTSM) $(BIN_GTSTE) $(BIN_GTDELT) $(BIN_GTRUN)

SHELLS = day2mon.sh any2mon.sh histchk.sh gtfact.sh gtrepint.sh \
gtfmod.sh mon2clim.sh day0fix.sh
# 
# 
# 
all : lib $(BINS)

$(BIN_GTTHETA) : $(OBJ_GTTHETA)
	$(FC) $(FOPT) -o $(BIN_GTTHETA) $(OBJ_GTTHETA) $(FLIB_UTIL)

$(BIN_GTTHETE) : $(OBJ_GTTHETE)
	$(FC) $(FOPT) -o $(BIN_GTTHETE) $(OBJ_GTTHETE) $(FLIB_UTIL)

$(BIN_GTRELH) : $(OBJ_GTRELH)
	$(FC) $(FOPT) -o $(BIN_GTRELH) $(OBJ_GTRELH) $(FLIB_UTIL)

$(BIN_INTP2TH) : $(OBJ_INTP2TH)
	$(FC) $(FOPT) -o $(BIN_INTP2TH) $(OBJ_INTP2TH) $(FLIB_UTIL)

$(BIN_PVONT) : $(OBJ_PVONT)
	$(FC) $(FOPT) -o $(BIN_PVONT) $(OBJ_PVONT) $(FLIB_UTIL)

$(BIN_PVSFC) : $(OBJ_PVSFC)
	$(FC) $(FOPT) -o $(BIN_PVSFC) $(OBJ_PVSFC) $(FLIB_UTIL)
  
$(BIN_GTPS) : $(OBJ_GTPS)
	$(FC) $(FOPT) -o $(BIN_GTPS) $(OBJ_GTPS) $(FLIB_UTIL)

$(BIN_GTPS2P) : $(OBJ_GTPS2P)
	$(FC) $(FOPT) -o $(BIN_GTPS2P) $(OBJ_GTPS2P) $(FLIB_UTIL)

$(BIN_GTCOL) : $(OBJ_GTCOL)
	$(FC) $(FOPT) -o $(BIN_GTCOL) $(OBJ_GTCOL) $(FLIB_UTIL)

$(BIN_GTCOLV) : $(OBJ_GTCOLV)
	$(FC) $(FOPT) -o $(BIN_GTCOLV) $(OBJ_GTCOLV) $(FLIB_UTIL)

$(BIN_GTDOB) : $(OBJ_GTDOB)
	$(FC) $(FOPT) -o $(BIN_GTDOB) $(OBJ_GTDOB) $(FLIB_UTIL)

$(BIN_GTDOBL) : $(OBJ_GTDOBL)
	$(FC) $(FOPT) -o $(BIN_GTDOBL) $(OBJ_GTDOBL) $(FLIB_UTIL)

$(BIN_GTTROP) : $(OBJ_GTTROP)
	$(FC) $(FOPT) -o $(BIN_GTTROP) $(OBJ_GTTROP) $(FLIB_UTIL)

$(BIN_GTSTAB) : $(OBJ_GTSTAB)
	$(FC) $(FOPT) -o $(BIN_GTSTAB) $(OBJ_GTSTAB) $(FLIB_UTIL)

$(BIN_GTMAGE) : $(OBJ_GTMAGE)
	$(FC) $(FOPT) -o $(BIN_GTMAGE) $(OBJ_GTMAGE) $(FLIB_UTIL)

$(BIN_GTPSC) : $(OBJ_GTPSC)
	$(FC) $(FOPT) -o $(BIN_GTPSC) $(OBJ_GTPSC) $(FLIB_UTIL)

$(BIN_GTPSCT) : $(OBJ_GTPSCT)
	$(FC) $(FOPT) -o $(BIN_GTPSCT) $(OBJ_GTPSCT) $(FLIB_UTIL)

$(BIN_GTMASS) : $(OBJ_GTMASS)
	$(FC) $(FOPT) -o $(BIN_GTMASS) $(OBJ_GTMASS) $(FLIB_UTIL)

$(BIN_GTMASV) : $(OBJ_GTMASV)
	$(FC) $(FOPT) -o $(BIN_GTMASV) $(OBJ_GTMASV) $(FLIB_UTIL)

$(BIN_GTINFO) : $(OBJ_GTINFO)
	$(FC) $(FOPT) -o $(BIN_GTINFO) $(OBJ_GTINFO) $(FLIB_UTIL)

$(BIN_GTITEM) : $(OBJ_GTITEM)
	$(FC) $(FOPT) -o $(BIN_GTITEM) $(OBJ_GTITEM) $(FLIB_UTIL)

$(BIN_GT2TXT) : $(OBJ_GT2TXT)
	$(FC) $(FOPT) -o $(BIN_GT2TXT) $(OBJ_GT2TXT) $(FLIB_UTIL)

$(BIN_HISTCHK) : $(OBJ_HISTCHK)
	$(FC) $(FOPT) -o $(BIN_HISTCHK) $(OBJ_HISTCHK)

$(BIN_PARACHK) : $(OBJ_PARACHK)
	$(FC) $(FOPT) -o $(BIN_PARACHK) $(OBJ_PARACHK)

$(BIN_GTEPF) : $(OBJ_GTEPF)
	$(FC) $(FOPT) -o $(BIN_GTEPF) $(OBJ_GTEPF) $(FLIB_UTIL)

$(BIN_GTEPFW) : $(OBJ_GTEPFW)
	$(FC) $(FOPT) -o $(BIN_GTEPFW) $(OBJ_GTEPFW) $(FLIB_UTIL)

$(BIN_GTWSTA) : $(OBJ_GTWSTA)
	$(FC) $(FOPT) -o $(BIN_GTWSTA) $(OBJ_GTWSTA) $(FLIB_UTIL)

$(BIN_GTO2W) : $(OBJ_GTO2W)
	$(FC) $(FOPT) -o $(BIN_GTO2W) $(OBJ_GTO2W) $(FLIB_UTIL)

$(BIN_GTCUT) : $(OBJ_GTCUT)
	$(FC) $(FOPT) -o $(BIN_GTCUT) $(OBJ_GTCUT) $(FLIB_UTIL)

$(BIN_GTJOIN) : $(OBJ_GTJOIN)
	$(FC) $(FOPT) -o $(BIN_GTJOIN) $(OBJ_GTJOIN) $(FLIB_UTIL)

$(BIN_GTFACT) : $(OBJ_GTFACT)
	$(FC) $(FOPT) -o $(BIN_GTFACT) $(OBJ_GTFACT) $(FLIB_UTIL)

$(BIN_GTRANG) : $(OBJ_GTRANG)
	$(FC) $(FOPT) -o $(BIN_GTRANG) $(OBJ_GTRANG) $(FLIB_UTIL)

$(BIN_GTMMON) : $(OBJ_GTMMON)
	$(FC) $(FOPT) -o $(BIN_GTMMON) $(OBJ_GTMMON) $(FLIB_UTIL)

$(BIN_GTMDAY) : $(OBJ_GTMDAY)
	$(FC) $(FOPT) -o $(BIN_GTMDAY) $(OBJ_GTMDAY) $(FLIB_UTIL)

$(BIN_GTRPLC) : $(OBJ_GTRPLC)
	$(FC) $(FOPT) -o $(BIN_GTRPLC) $(OBJ_GTRPLC) $(FLIB_UTIL)

$(BIN_GTRPLI) : $(OBJ_GTRPLI)
	$(FC) $(FOPT) -o $(BIN_GTRPLI) $(OBJ_GTRPLI) $(FLIB_UTIL)

$(BIN_GTCNST) : $(OBJ_GTCNST)
	$(FC) $(FOPT) -o $(BIN_GTCNST) $(OBJ_GTCNST) $(FLIB_UTIL)

$(BIN_GTZONL) : $(OBJ_GTZONL)
	$(FC) $(FOPT) -o $(BIN_GTZONL) $(OBJ_GTZONL) $(FLIB_UTIL)

$(BIN_GTCONV) : $(OBJ_GTCONV)
	$(FC) $(FOPT) -o $(BIN_GTCONV) $(OBJ_GTCONV) $(FLIB_UTIL)

$(BIN_GTTSM) : $(OBJ_GTTSM)
	$(FC) $(FOPT) -o $(BIN_GTTSM) $(OBJ_GTTSM) $(FLIB_UTIL)

$(BIN_GTSTE) : $(OBJ_GTSTE)
	$(FC) $(FOPT) -o $(BIN_GTSTE) $(OBJ_GTSTE) $(FLIB_UTIL)

$(BIN_GTDELT) : $(OBJ_GTDELT)
	$(FC) $(FOPT) -o $(BIN_GTDELT) $(OBJ_GTDELT) $(FLIB_UTIL)

$(BIN_GTRUN) : $(OBJ_GTRUN)
	$(FC) $(FOPT) -o $(BIN_GTRUN) $(OBJ_GTRUN) $(FLIB_UTIL)

lib:  
	@for i in $(LIBSRCS) ; do \
	  cd $$i ; \
	  echo "entering $$i/" ; \
	  $(MAKE) ; \
	  cd .. ; \
        done

install : $(BINS) install-dir install-config install-shells install-subs
	@for i in $(BINS) ; do \
          $(CP) $$i $(prefix)/bin ; \
	done

install-dir : 
	test -d $(prefix)/bin || $(MD) $(prefix)/bin

install-config : 
	$(CP) /dev/null config.make
	echo "prefix:$(prefix)"  >> config.make
	echo "bindir:$(prefix)/bin"  >> config.make
	echo "libdir:$(prefix)/lib"  >> config.make
	echo "includedir:$(prefix)/include"  >> config.make
	./mkconfig.sh -i config.make -o config.sh
	$(CP) config.sh $(prefix)/bin

install-shells: 
	@for i in $(SHELLS) ; do \
          $(CP) $$i $(prefix)/bin ; \
	done

install-subs: 
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; \
	  echo "entering $$i/" ; \
	  $(MAKE) install; \
	  cd .. ; \
        done

clean-all : clean clean-bins

clean   :
	$(RM) *.o *.mod config.make *~ *__genmod.f90
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; \
	  echo "cleaning $$i/" ; \
	  $(MAKE) clean ; \
	  cd .. ; \
        done

clean-bins :
	$(RM) $(BINS) config.sh 
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; \
	  echo "cleaning $$i/" ; \
	  $(MAKE) clean-bins ; \
	  cd .. ; \
        done

