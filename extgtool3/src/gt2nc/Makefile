
#
# makefile
#
prog_root = ..
include ../Mkinclude
include ../Mkinclude-netcdf


LIB = $(LIB_NETCDF)
INCLUDE=-I$(NETCDF)/include

FINCLUDE=$(INC_DIR) -I$(prog_root) $(INCLUDE)

BIN_GT2NC	= gt2nc
#OBJ_GT2NC	= $(prog_root)/common_args.o util_gt2nc.o
OBJ_GT2NC	= ../util.o wnc.o

BINS = $(BIN_GT2NC)


all: $(BINS)

$(BIN_GT2NC): $(OBJ_GT2NC)
	$(FC) $(FOPT) -c gt2nc.f90 $(FINCLUDE)
	$(FC) -o $(BIN_GT2NC) gt2nc.o $(OBJ_GT2NC) $(LIB) $(FLIB_UTIL)

install : $(BINS)
	@for i in $(BINS) ; do \
          $(CP) $$i $(prefix)/bin ; \
	done

clean:
	$(RM) *.o *.mod *~ *__genmod.f90

clean-bins :
	$(RM) $(BINS)

