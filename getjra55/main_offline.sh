#!/bin/sh
#
. /etc/bashrc 
export DATADIR_JRA55='path_to_jra55_directory'
#
# auto
cur_yy=`date --date "2 days ago" +%Y`
cur_mm=`date --date "2 days ago" +%m`
# manually
#cur_yy=2020
#cur_mm=12
jra_progs="${DATADIR_JRA55}/progs"
log="${jra_progs}/log_main"
cd ${jra_progs}
#
# download grib files
cp /dev/null ${log}
${jra_progs}/wget_jra55.sh >> ${log} 2>& 1

#for var in slp rain uwnd vwnd tmp hgt depr omega rh spfh t2 u10 v10
for var in uwnd vwnd tmp hgt depr omega rh spfh
do
  case ${ivar} in
    slp|rain|t2|u10|v10) nz=1;;
    rh|spfh|depr) nz=27;;
    *) nz=37;;
  esac
  # grib => grads: 4 times daily 
  echo ${jra_progs}/grib2bin-jra_prs_day.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -f -v ${var} >> ${log}
  (${jra_progs}/grib2bin-jra_prs_day.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -f -v ${var} >> ${log} 2>& 1) || exit 1
  #
  # 6hr => daily
  echo ${jra_progs}/exec_convert.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -z ${nz} -d -f -v ${var} >> ${log}
  (${jra_progs}/exec_convert.sh -ys ${cur_yy} -ye ${cur_yy} -me ${cur_mm} -z ${nz} -d -f -v ${var} >> ${log} 2>&1) || exit 1
done
#
exit 0
