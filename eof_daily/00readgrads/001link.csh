#!/bin/csh -f
#
source ../setpath.csh
#
### set variables ###
if ( "x$1" == "x" ) exit 1
# data range
set dsyy = 1958   # start yeat of the data (default value is 1948)

set nsyy = 1981   # start year of analysis
set nsmm = 01     # start month
set neyy = 2020   # end year
set nemm = 12     # end month

# configuration
set var = $1  # u v t w g uwnd vwnd air hgt

set ext = .bin   #  extension character of output files
                 # (ex.  .grads  .bin  .dat)
                 # (* do not edit)

set dataset = 4  # 5; JRA-55 surface level data
                 # 4; JRA-55 pressure level data
                 # 3; ERA-interim surface level data
                 # 2; ERA-interim pressure level data
                 # 1; NCEP/NCAR reanalysis surface level data
                 # 0; NCEP/NCAR reanalysis pressure level data

set timestep = 1 # 2; 4 times daily (year divided)
                 # 1; daily mean (year divided)
                 # 0; monthly mean (one file)
### set variables ###

##### set variables #####
# grid information
switch ( ${dataset} )      # dataset
  case 0:
    set nx = 144
    set ny = 73
    set flag = 
    switch ( ${var} )
      case omega:
        set nz = 12
      breaksw
      case shum:
      case rhum:
        set nz = 8
      breaksw
      default:
        set nz = 17
      breaksw
    endsw
  breaksw # case 0 (NCEP pressure)

  case 2:
    set nx = 144
    set ny = 73
    set nz = 23
    set flag = 
  breaksw # case 2 (ERA-40 pressure)

  case 1:
  case 3:
    set nz = 1
    switch ( ${var} )
      case prate:
      case pres:
        set flag = .sfc.gauss
        set nx = 192
        set ny = 94
      breaksw
      case dswrf:
        set flag = top
        set nx = 192
        set ny = 94
      breaksw
      default:
        set flag = 
        set nx = 144
        set ny = 73
      breaksw
    endsw # case 1, 3 (NCEP, ERA-40 surface)
  breaksw

  case 4:
    set nx = 288
    set ny = 145
    set nz = 37
    set flag = 
  breaksw # case 4 (JRA-55 pressure)

  case 5:
    set nx = 288
    set ny = 145
    set nz = 1
    set flag = 
  breaksw # case 5 (JRA-55 surface)

  default:
    echo 'invalid input: dataset' ; exit 1
  breaksw
endsw                      # dataset
##### set variables #####


##### set directory #####
# input directory
if ( "${dataset}" == 0 || "${dataset}" == 1 ) then
  if ( "${timestep}" == 0 ) set ind = ${DATADIR_NCEP}/grads/monthly/${var}
  if ( "${timestep}" == 1 ) set ind = ${DATADIR_NCEP}/grads/daily/${var}
  if ( "${timestep}" == 2 ) set ind = ${DATADIR_NCEP}/grads/6hr/${var}
endif
if ( "${dataset}" == 2 || "${dataset}" == 3 ) then
  if ( "${timestep}" == 0 ) set ind = ${DATADIR_ERAINT}/grads/monthly/${var}
  if ( "${timestep}" == 1 ) set ind = ${DATADIR_ERAINT}/grads/daily/${var}
  if ( "${timestep}" == 2 ) set ind = ${DATADIR_ERAINT}/grads/6hr/${var}
endif
if ( "${dataset}" == 4 || "${dataset}" == 5 ) then
  if ( "${timestep}" == 0 ) set ind = ${DATADIR_JRA55}/grads/monthly/${var}
  if ( "${timestep}" == 1 ) set ind = ${DATADIR_JRA55}/grads/daily/${var}
  if ( "${timestep}" == 2 ) set ind = ${DATADIR_JRA55}/grads/6hr/${var}
endif
#
# output directory
set outd = .
if (! -e ${outd} ) mkdir -p ${outd}
##### set directory #####

### input / output file ###
set stmm = `echo ${nsmm} | ${AWK} '{print $1*1}'`
set enmm = `echo ${nemm} | ${AWK} '{print $1*1}'`
if ( "${timestep}" == 0 ) then
  set ifile = ${ind}/${var}${flag}.mon.mean.grads
  set ofile = ${outd}/${var}${flag}.mon.mean${ext}
  set mkdata = 0 # 0; only link
  @ dimtime = ( ${neyy} - ${nsyy} ) * 12  + 1 + ${enmm} - ${stmm}
  @ ista = ( ${nsyy} - ${dsyy} ) * 12  + ${stmm}
  @ iend = ( ${neyy} - ${dsyy} ) * 12  + ${enmm}
else if ( "${timestep}" == 1 || "${timestep}" == 2 ) then
  set ifile = ${ind}/${var}${flag}
  set mkdata = 1 # 1; concatenate
  if ( "${timestep}" == 1 ) set ofile = ${outd}/${var}${flag}.daily${ext}
  if ( "${timestep}" == 2 ) set ofile = ${outd}/${var}${flag}.4times.daily${ext}
  # make time grid information
  ${FC} ${OPT} ../mktgrd.f90 -o a.out
  echo ${nsyy}  ${nsmm}  ${neyy}  ${nemm}  12  2 | ./a.out
  set dimtime = `cat fort.10 | wc -l`
  if ( "${timestep}" == 2 ) @ dimtime = ${dimtime} * 4
  set ista = 1
  set iend = ${dimtime}
else
  echo 'invalid input: timestep' ; exit 1
endif
### input / output file ###

echo "${nsyy}.${nsmm}-${neyy}.${nemm} start... (${dimtime} months) "
echo "start: ${ista}, end: ${iend}"


cat >! filedata.dat <<EOF
output_file	${ofile}
output_dir	${outd}
ext_cha		${ext}
EOF
cat >! variable.dat <<EOF
${nsyy}  ${stmm}  ${ista}  ${dsyy}
${neyy}  ${enmm}  ${iend}
var      ${var}    ${timestep}  ${mkdata}
${nx}  ${ny}  ${nz}
EOF


# mkdata
if ( "${mkdata}" == 0 ) then
  if (! -e ${ifile} ) then
    echo "Error: ${ifile} does not exist" ; exit 1
  endif
  echo "link ${ifile}"
  ln -sf ${ifile} ${ofile}
else if ( "${mkdata}" == 1 ) then
  # concatenate
  cp /dev/null ${ofile}
  set year = ${nsyy}
  while (${year} <= ${neyy})
    set fname = "${ifile}.${year}.grads"
    if ( -e ${fname} ) then
      cat ${fname} >> ${ofile}
    else
      echo "Error: ${fname} does not exist" ; exit 1
    endif
    @ year ++
  end
else
  echo "invalid input: mkdata" ; exit 1
endif
#
exit 0
