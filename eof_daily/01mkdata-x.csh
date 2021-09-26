#!/bin/csh -f
#
source setpath.csh
#
### variables ###
# data range
# data record most be started in January
set dsyy = 1981   # start year of data
set nsyy = 1981   # start year of analysis
set nsmm = 12     # start month
set neyy = 2020   # end year
set nemm = 02     # end month


set flag = 3      # flag = 0; whole months in nsyy/nsmm - neyy/nemm
                  # flag = 1; nsmm - nemm months in nsyy-neyy
                  #  (eg. if nsmm=12 nemm=2, DJF is selected)
                  #
                  # iflag = 2;  whole days in nsyy/nsmm - neyy/nemm
                  # iflag = 3;  nsmm - nemm days in nsyy-neyy

set stx  = 110.0  # minimum range of longitude
set enx  = 170.0  # maximum range of longitude
set sty  =  55.0  # minimum range of latitude (starting from north pole)
set eny  =  15.0  # maximum range of latitude (starting from north pole)
set level1  =  1  # minimum level
set level2  =  1  # maximum level

set mkgrid = 1    # flag = 1; when x, y grids are levels
                  # flag = 0; when x, y grids are linear

set weight = 0    # weight = 1; weighted by the square root of 
                  #             the pressure interval
                  # weight = 0; no weighted

set npc = 5       # the number of principal component for output

set dset = 1      # dset = 1; 00readgrads (do not change)

set timestep = 1  # 2; 4 times daily (year divided)
                  # 1; daily mean (year divided)
                  # 0; monthly mean (one file)

# configuration
set var = vor

set ext = .bin   #  extension character of input files
                 # (ex.  .grads  .bin  .dat)

# grid information for original data
set nx     = 288   # number of x-grid (For zonal mean data, this value is 1)
set ny     = 145   # number of y-grid
set nz     = 1     # number of p-grid (For surface level data, this value is 1)
set nmm    = 12    # months par year
                   # (For daily mean data, nmm in progs. is replaced 
                   #  by records par year, nrec)
### variables ###

### input and output ###
# input directory
if ( ${dset} == 1 ) then
  set cind = ./00readgrads           # directory of climatorogy file
  set dind = ./00readgrads           # directory of data file
else
  echo "Error: invalid input dset" ; exit 1
endif

set gind = .                       # grid id file (generate automatically)

# output directory
set outd = .

# input files
if ( ${timestep} == 0 ) then
  set cfile = "${cind}/${var}.clim.mon.mean${ext}"
  set dfile = "${dind}/${var}.mon.mean${ext}"
else if ( ${timestep} == 1 || ${timestep} == 2 ) then
  set cfile = 
  if ( "${timestep}" == 1 ) set dfile = "${dind}/${var}.daily${ext}"
  if ( "${timestep}" == 2 ) set dfile = "${dind}/${var}.4times.daily${ext}"
else
  echo "Error: invalid input timestep" ; exit 1
else
endif

# time and spatial grid information (generate automatically)
set tfile = ${gind}/time.dat
set gfile = ${gind}/grid.dat

# output file
set ofile = ${outd}/data-x.bin
### input and output ###


##### check #####
if ( ${level1} < 1 || ${level2} > ${nz} ) then
  echo "Error: incompatible input is found in np and level " ; exit 1
else if ( ${level1} > ${level2} ) then
  echo "Error: invalid input (level1 <= level2) " ; exit 1
endif
if (! -e ${dfile} ) then
  echo "Error: ${dfile} does not exist" ; exit 1
endif
set opt_clim = ".true."
if ( "x${cfile}" == "x" ) then
  set opt_clim = ".false."
else if (! -e ${cfile} ) then
  echo "Error: ${cfile} does not exist" ; exit 1
endif
if ( ${weight} == 0 || ${weight} == 1 ) then
else
  echo "Error: invalid input weight" ; exit 1
endif
##### check #####

##### make grid data #####
rm -f fort.*
(${FC} ${OPT} mkgrid${mkgrid}.f90 -o a.out) || exit 1
ln -s ${gfile} fort.10
echo ${nx} ${ny} ${nz} ${stx}  ${enx}  ${sty}  ${eny} ${level1} ${level2} | ./a.out
rm -f a.out fort.10
##### make grid data #####

##### make time grid information #####
${FC} ${OPT} mktgrd.f90 -o a.out
ln -s ${tfile} fort.10
echo ${dsyy}  ${nsmm}  ${neyy}  ${nemm}  ${nmm}  ${flag} | ./a.out
rm -f a.out fort.10

set nrec = `${AWK} '($3 == '${nsyy}'){print $0}' ${tfile} | wc -l`
${AWK} '{if ($3 == '${nsyy}' && $4 < '${nsmm}'){} else if ($3 >= '${nsyy}'){print $0} }' ${tfile}  >! tmp.dat
mv -f tmp.dat ${tfile}
##### make time grid information #####


##### set variables #####
set dimgrid = `cat ${gfile} | wc -l`
set dimtime = `cat ${tfile} | wc -l`
set stmm = `echo ${nsmm} | ${AWK} '{print $1*1}'`
set enmm = `echo ${nemm} | ${AWK} '{print $1*1}'`
##### set variables #####


### make data-X ###
cat >! common_args.f90 <<EOF
module common_args
  integer(4), parameter :: nx = ${nx} !! number of x-grids
  integer(4), parameter :: ny = ${ny} !! number of y-grids
  integer(4), parameter :: nz = ${nz} !! number of z-grids
  integer(4), parameter :: nmm = ${nrec} !! number of records (or month)

  integer(4), parameter :: nv = ${dimgrid} !! number of spatial grids
  integer(4), parameter :: nc = ${dimtime} !! number of time grids

  !c+++ start/end time
  integer(4), parameter :: nsyy = ${nsyy} !! start year
  integer(4), parameter :: nsmm = ${stmm} !! start month
  integer(4), parameter :: neyy = ${neyy} !! end year
  integer(4), parameter :: nemm = ${enmm} !! end month

  !c+++ vertical weight
  integer(4), parameter :: weight = ${weight}

  !c+++ info-PCA
  integer(4), parameter :: npc = ${npc} !! number of PCA for output

  !c+++ read climatorogy 
  logical, parameter    :: opt_clim = ${opt_clim}
end module common_args
EOF

${FC} ${OPT} -c common_args.f90
${FC} ${OPT} -c mkdata.f90
${FC} ${OPT} -o a.out mkdata.o common_args.o
rm -f fort.*
ln -s ${tfile} fort.10
ln -s ${gfile} fort.11
if ( -e "${cfile}" ) ln -s ${cfile} fort.12
ln -s ${dfile} fort.21
./a.out
mv -f fort.51 ${ofile}
rm -f fort.* *.o *.mod a.out
### make data-X ###
#
cat >! filenames.dat <<EOF
1grid_file	${gfile}  ${tfile}
2climatorogy	${cfile}
3monthly_data	${dfile}
4data-x_file	${ofile}
EOF
#
cat >! datainfo.dat <<EOF
fc  ${FC}
fflags  ${OPT}
grids  ${nx}  ${ny}  ${nz}
rec_per_year  ${nrec}
start_time  ${nsyy}  ${stmm}
end_time  ${neyy}  ${enmm}
dimtime  ${dimtime}
dimgrid  ${dimgrid}
var_name  ${var}
min_lev  ${level1}
max_lev  ${level2}
region  ${stx}  ${enx}  ${sty}  ${eny}
weight  ${weight}
EOF
#
exit 0
