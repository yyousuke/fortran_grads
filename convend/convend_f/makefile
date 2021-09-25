FC = ifort
CC = gcc
LD = ifort
#
FFLAGS = -assume byterecl
CFLAGS =
LDFLAGS =
#
INC=
LIB=
#
defaults:
	$(FC) $(FFLAGS) -c convend.f90
	$(FC) $(FFLAGS) -c main.f90
	$(LD) $(LDFLAGS) convend.o main.o $(LIB)

clean:
	rm *.o a.out
