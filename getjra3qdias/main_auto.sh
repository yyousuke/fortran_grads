#!/bin/bash
. /etc/bashrc 
: ${DATE:=date}
export DATADIR_JRA3Q="JRA-3Qデータへの絶対パス"
export WGRIB2="wgrib2"
#
# 4 days ago
prv_yy=`${DATE} --date "4 days ago" +%Y`
prv_mm=`${DATE} --date "4 days ago" +%m`
prv_dd=`${DATE} --date "4 days ago" +%d`
cur_yy=`${DATE} --date "4 days ago" +%Y`
cur_mm=`${DATE} --date "4 days ago" +%m`
cur_dd=`${DATE} --date "4 days ago" +%d`
#
jra_progs="${DATADIR_JRA3Q}/progs"
log="${jra_progs}/log_main"
cd ${jra_progs}
#
# download grib files
cp /dev/null ${log}
(./download_auto.py -p anl_surf125 -s ${prv_yy}${prv_mm}${prv_dd}00 -e ${cur_yy}${cur_mm}${cur_dd}18 >> ${log} 2>& 1) || exit 1
(./download_auto.py -p anl_p125 -s ${prv_yy}${prv_mm}${prv_dd}00 -e ${cur_yy}${cur_mm}${cur_dd}18 >> ${log} 2>& 1) || exit 1
(./download_auto.py -p fcst_p125 -s ${prv_yy}${prv_mm}${prv_dd}00 -e ${cur_yy}${cur_mm}${cur_dd}18 >> ${log} 2>& 1) || exit 1
(./download_auto.py -p fcst_phy2m125 -s ${prv_yy}${prv_mm}${prv_dd}00 -e ${cur_yy}${cur_mm}${cur_dd}18 >> ${log} 2>& 1) || exit 1

for var in slp ps uwnd vwnd tmp hgt depr omega rh spfh strm t2 u10 v10
do
  case ${ivar} in
    slp|rain|t2|u10|v10) nz=1;;
    *) nz=45;;
  esac
  # check downloads
  #${jra_progs}/check.sh ${prv_yy} ${prv_mm} ${prv_dd} ${cur_yy} ${cur_mm} ${cur_dd} ${var} >> ${log} 2>& 1
  #ESTAT=$?
  if [ ${ESTAT} = 0 ]; then 
    # grib => grads: 4 times daily 
    echo ${jra_progs}/grib2bin-jra3q.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -f -v ${var} >> ${log}
    (${jra_progs}/grib2bin-jra3q.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -f -v ${var} >> ${log} 2>& 1) || exit 1
    #
    # 6hr => daily
    echo ${jra_progs}/exec_convert.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -z ${nz} -d -f -v ${var} >> ${log}
    (${jra_progs}/exec_convert.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -z ${nz} -d -f -v ${var} >> ${log} 2>&1) || exit 1
  fi
done
echo
#
exit 0
