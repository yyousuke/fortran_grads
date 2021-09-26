#!/bin/csh -f
#
#
# system
source /etc/csh.login 
#
#
if ( -x "/usr/bin/gawk" ) then
  set AWK = "/usr/bin/gawk"
else if ( -x "/opt/local/bin/gawk" ) then
  set AWK = "/opt/local/bin/gawk"
else
  set AWK = `which awk`
endif

# compiler and options
set FC = gfortran
set OPT = "-O -fconvert=big-endian -frecord-marker=4"
#set FC = ifort
#set OPT = "-O -assume byterecl -convert big_endian -mcmodel=medium -shared-intel -heap-arrays -fno-alias " # linux + ifort
#set OPT = "-O -assume byterecl -convert big_endian -mcmodel=medium -shared-intel -heap-arrays -fno-alias -check all -warn all -g -traceback " # linux + ifort
#-check all -warn all -g -traceback 

#
# NETCDF home directory (need to read NCEP/NCAR data)
set NETCDF = /opt/local

# data directory
set DATADIR_JRA55 = "path_to_JRA55_data"

# for ERA-interim
#set DATADIR_ERAINT =
# for NCEP
#set DATADIR_NCEP =


