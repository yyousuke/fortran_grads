#!/bin/csh -f
#
source setpath.csh
#
# multiply -1 by score and vector (for npc = 5 in 01mkdata-x.csh)
set opt_neg = 'f t f f f' # negative signs for EOF-1,,,5

# output directory
set outd = .

# input files
set efile = `${AWK} '{print $2}' filenames.dat | head -5| tail -1`
set evfile = `${AWK} '{print $2}' filenames.dat | head -6| tail -1`
#
set gfile = `${AWK} '{print $2}' filenames.dat | head -1`
set tfile = `${AWK} '{print $3}' filenames.dat | head -1`
set cfile = `${AWK} '{print $2}' filenames.dat | head -2| tail -1`
set dfile = `${AWK} '{print $2}' filenames.dat | head -3| tail -1`
if (! -e ${efile}) then
  echo 'Error: check previous 02e-ev.csh status' ; exit 1
endif


# info-PCA
rm -f fort.* 
${FC} ${OPT} -c common_args.f90
${FC} ${OPT} -c mkinfo.f90
${FC} ${OPT} -o a.out mkinfo.o common_args.o
#
ln -s ${efile} fort.10
./a.out
mv -f fort.51 ${outd}/info-PCA.dat
rm -f fort.* a.out
#
cat filenames.dat | head -6 >! tmp.dat
mv -f tmp.dat filenames.dat
cat >> filenames.dat <<EOF
7infomation	${outd}/info-PCA.dat
EOF


# score timeseries
rm -f fort.* 
${FC} ${OPT} -c mkscore.f90
${FC} ${OPT} -o a.out mkscore.o common_args.o
#
ln -s  ${tfile}  fort.10
ln -s  ${gfile}  fort.11
if ( -e "${cfile}" ) ln -s ${cfile} fort.12
ln -s  ${dfile}  fort.21
ln -s  ${evfile} fort.22
echo "${opt_neg}" | ./a.out
#
mv -f fort.51 ${outd}/EOF-score.dat
mv -f fort.61 ${outd}/EOF-vector.dat
mv -f fort.71 ${outd}/EOF-norm_score.dat
rm -f fort.* *.o *.mod a.out
#
cat >> filenames.dat <<EOF
8scorefile	${outd}/EOF-score.dat
9vecrorfile	${outd}/EOF-vector.dat
10norm_score	${outd}/EOF-norm_score.dat
EOF
#
exit 0
