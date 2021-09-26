#!/bin/csh -f
#
source ../setpath.csh
#
### set input and output file names ###
set outd = `${AWK} '{print $2}' filedata.dat | head -2| tail -1`
set ext = `${AWK} '{print $2}' filedata.dat | head -3| tail -1`
set timestep = `${AWK} '{print $3}' variable.dat | head -3| tail -1`
if ( "${timestep}" == 0 ) set flag = .mon.mean
if ( "${timestep}" == 1 ) set flag = .daily
if ( "${timestep}" == 2 ) set flag = .4times.daily
set ifile1 = "uwnd${flag}${ext}"
set ifile2 = "vwnd${flag}${ext}"
set ofile = "${outd}/vor${flag}${ext}"
set zmin = 7
set zmax = 16
### set input and output file names ###

### set variables automatically ###
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
${FC} ${OPT} vmean.f90 -o a.out
#
rm -f fort.*
if (! -e ${ifile1} ) then
  echo "NOT Found: ${ifile1}" ; exit 1
endif
if (! -e ${ifile2} ) then
  echo "NOT Found: ${ifile2}" ; exit 1
endif
ln -s ${ifile1} fort.11
ln -s ${ifile2} fort.12
ln -s ${ofile} fort.51
#
#echo ${ista} ${iend} ${irecsta}
echo "${nx} ${ny} ${nz} ${iend} ${zmin} ${zmax}" | ./a.out
rm -f a.out
rm -f fort.*

echo "input file: ${ifile1}, ${ifile2}"
echo "output file: ${ofile}"
echo "dsyy: ${dsyy}"
echo "ista - iend: ${ista} - ${iend}"
echo "nsyy/nsmm - neyy/nemm: ${nsyy}/${nsmm} - ${neyy}/${nemm}"
echo "nx, ny, nz = ${nx} ${ny} ${nz}"
echo "nmm = 12"

exit 0
