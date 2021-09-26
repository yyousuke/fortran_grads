#!/bin/csh -f
#
# input data-x.bin
# calc. variance covariance matrix & eigenvalue and eigen vector
#
source setpath.csh
#
# output directory
set outd = .
##### output files #####
set efile = ${outd}/eigenvalue.dat
set evfile = ${outd}/eigenvector.dat
##### output files #####

# set input file
set ifile = `${AWK} '{print $2}' filenames.dat | head -4|tail -1`
if (! -e ${ifile}) then
  echo 'Error: check previous 01mkdata-x.csh status' ; exit 1
endif


rm fort.*
${FC} ${OPT} -c common_args.f90
${FC} ${OPT} -c main_seigve.f90
${FC} ${OPT} -o a.out main_seigve.o common_args.o
#
ln -s ${ifile} fort.10
./a.out
time >! runtime.dat
mv fort.51 ${efile}
mv fort.61 ${evfile}
rm -f fort.* *.o *.mod a.out
#
cat filenames.dat | head -4 >! tmp.dat
mv -f tmp.dat filenames.dat
cat >> filenames.dat <<EOF
5eigenvalue	${efile}
6eigenvector	${evfile}
EOF
#
exit 0
