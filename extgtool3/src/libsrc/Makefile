
#
# Makefile for include
#
prog_root = ..
include $(prog_root)/Mkinclude

# sysdep
OBJS_SYS = ysys_misc.o
MODS_SYS = ysys.mod
ifeq ($(FC),gfortran)
  OBJS_SYS = ysys_gfort.o
endif
ifeq ($(FC),pgf90)
  OBJS_SYS = ysys_pgi.o
endif

OBJS = \
common_typedef.o \
common_args.o \
common_const.o \
$(OBJS_SYS) \
char2var.o \
error_handle.o \
convend.o \
calculate.o dcalculate.o diff.o diffp2z.o dtave.o eddy.o fourier.o \
ucaln.o uopts.o zmean.o dzmean.o zmean2d.o \
linear.o splines.o intplt.o \
rw.o rwgtool.o io.o
MODS = \
common_typedef.mod \
common_args.mod \
common_const.mod \
$(MODS_SYS) \
char2var.mod \
error_handle.mod \
calculate.mod dcalculate.mod diff.mod diffp2z.mod dtave.mod eddy.mod fourier.mod \
ucaln.mod uopts.mod zmean.mod dzmean.mod zmean2d.mod \
linear.mod splines.mod intplt.mod \
rw.mod rwgtool.mod io.mod
#
#
all: default

default: lib

lib:   $(OBJS)
	$(AR)  $(FLIB_UTIL) $(OBJS)
	$(RANLIB)  $(FLIB_UTIL)

install:

clean-all : clean clean-bins

clean:
	$(RM) *.o *.mod *~ *__genmod.f90 $(FLIB_UTIL)

clean-bins :


