#!/bin/bash
. /etc/bashrc 
export DATADIR_JRA3Q="JRA-3Qデータへの絶対パス"
export WGRIB2="wgrib2"
#
prv_yy=2025
prv_mm=01
prv_dd=01
cur_yy=2025
cur_mm=06
cur_dd=30
jra_progs="${DATADIR_JRA3Q}/progs"
log="${jra_progs}/tinfoin.txt"
cd ${jra_progs}
#
# download grib files
cp /dev/null ${log}

for var in surf ugrd vgrd tmp depr hgt rh spfh vvel relv reld vpot strm o3mr cdca cwat phy2m
do
  ${jra_progs}/check.sh ${prv_yy} ${prv_mm} ${prv_dd} ${cur_yy} ${cur_mm} ${cur_dd} ${var} >> ${log} 2>& 1
done
#
exit 0
