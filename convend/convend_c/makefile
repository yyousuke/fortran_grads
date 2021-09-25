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
	$(CC) $(CFLAGS) -c convend.c
	$(FC) $(FFLAGS) -c main.f90
	$(LD) $(LDFLAGS) convend.o main.o $(LIB)

clean:
	rm *.o a.out
