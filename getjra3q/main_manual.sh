#!/bin/bash
#
. /etc/bashrc 
# system dependency
if [ `uname` = "Darwin" ]; then
: ${DATE:=gdate}
else
: ${DATE:=date}
fi
export DATADIR_JRA3Q="path_to_JRA-3Q_dir"
export WGRIB2="wgrib2"
#
# 10-4 days ago
prv_yy=`${DATE} --date "10 days ago" +%Y`
prv_mm=`${DATE} --date "10 days ago" +%m`
prv_dd=`${DATE} --date "10 days ago" +%d`
cur_yy=`${DATE} --date "4 days ago" +%Y`
cur_mm=`${DATE} --date "4 days ago" +%m`
cur_dd=`${DATE} --date "4 days ago" +%d`
# manually
#prv_yy=2020
#prv_mm=12
#prv_dd=01
#cur_yy=2020
#cur_mm=12
#cur_dd=01
jra_progs="${DATADIR_JRA3Q}/progs"
log="${jra_progs}/log_main"
cd ${jra_progs}
#
# download grib files
cp /dev/null ${log}
${jra_progs}/wget_jra3q.sh ${prv_yy} ${prv_mm} ${prv_dd} ${cur_yy} ${cur_mm} ${cur_dd} >> ${log} 2>& 1
ESTAT=$?
[ ${ESTAT} -ne 0 ] && exit 1

for var in slp ps uwnd vwnd tmp hgt depr omega rh spfh strm t2 u10 v10
do
  case ${ivar} in
    slp|rain|t2|u10|v10) nz=1;;
    *) nz=45;;
  esac
  # check downloads
  ${jra_progs}/check.sh ${prv_yy} ${prv_mm} ${prv_dd} ${cur_yy} ${cur_mm} ${cur_dd} ${var} >> ${log} 2>& 1
  ESTAT=$?
  if [ ${ESTAT} -eq 0 ]; then 
    # grib => grads: 4 times daily 
    echo ${jra_progs}/grib2bin-jra3q.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -f -v ${var} >> ${log}
    (${jra_progs}/grib2bin-jra3q.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -f -v ${var} >> ${log} 2>& 1) || exit 1
    #
    # 6hr => daily
    echo ${jra_progs}/exec_convert.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -z ${nz} -d -f -v ${var} >> ${log}
    (${jra_progs}/exec_convert.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -z ${nz} -d -f -v ${var} >> ${log} 2>&1) || exit 1
  fi
done
#
exit 0
