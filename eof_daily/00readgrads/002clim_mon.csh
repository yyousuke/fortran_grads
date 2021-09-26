#!/bin/csh -f
#
source ../setpath.csh
#
### set input and output file names ###
set outd = `${AWK} '{print $2}' filedata.dat | head -2| tail -1`
set ext = `${AWK} '{print $2}' filedata.dat | head -3| tail -1`
set ifile = `${AWK} '{print $2}' filedata.dat | head -1| tail -1`
set ofile = ${outd}/${var}.clim.mon.mean${ext}
### set input and output file names ###

### set variables automatically ###
set var = `${AWK} '{print $2}' variable.dat | head -3| tail -1`
set nsyy = `${AWK} '{print $1}' variable.dat | head -1`
set neyy = `${AWK} '{print $1}' variable.dat | head -2| tail -1`
set nsmm = `${AWK} '{print $2}' variable.dat | head -1`
set nemm = `${AWK} '{print $2}' variable.dat | head -2| tail -1`
set ista = `${AWK} '{print $3}' variable.dat | head -1`
set iend = `${AWK} '{print $3}' variable.dat | head -2| tail -1`
set dsyy = `${AWK} '{print $4}' variable.dat | head -1`
set nx = `${AWK} '{print $1}' variable.dat | head -4| tail -1`
set ny = `${AWK} '{print $2}' variable.dat | head -4| tail -1`
set nz = `${AWK} '{print $3}' variable.dat | head -4| tail -1`
### set variables automatically ###

set irecsta = ${nsmm}
${FC} ${OPT} climat.f90 -o a.out
#
rm -f fort.*
ln -s ${ifile} fort.11
ln -s ${ofile} fort.51
#
#echo ${ista} ${iend} ${irecsta}
echo "${nx} ${ny} ${nz} 12 ${ista} ${iend} ${irecsta}" | ./a.out
rm -f a.out
rm -f fort.*

echo "input file: ${ifile}"
echo "output file: ${ofile}"
echo "dsyy: ${dsyy}"
echo "ista - iend: ${ista} - ${iend}"
echo "nsyy/nsmm - neyy/nemm: ${nsyy}/${nsmm} - ${neyy}/${nemm}"
echo "nx, ny, nz = ${nx} ${ny} ${nz}"
echo "nmm = 12"

exit 0
