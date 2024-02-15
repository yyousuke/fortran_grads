#!/bin/bash
# NCARからJRA-3Qデータを取得する
######################################################################
# initialization
######################################################################
. /etc/bashrc 
#set -eu
# system dependency
if [ `uname` = "Darwin" ]; then
: ${DATE:=gdate}
: ${WGET:=/opt/local/bin/wget}
else
: ${DATE:=date}
: ${WGET:=/usr/bin/wget}
fi
: ${option:="-np -c"}
#: ${option:="-np -c -q"}
#: ${option:="-N -r -np --level=0"}
#
# manually (取得するデータの期間を指定する場合は引数として与える)
nsyy=${1:-auto}
nsmm=${2:-auto}
nsdd=${3:-auto}
neyy=${4:-auto}
nemm=${5:-auto}
nedd=${6:-auto}
#
# auto (自動の場合は、10日前から4日前まで）
prv_yy=`${DATE} --date "10 days ago" +%Y`
prv_mm=`${DATE} --date "10 days ago" +%m`
prv_dd=`${DATE} --date "10 days ago" +%d`
cur_yy=`${DATE} --date "4 days ago" +%Y`
cur_mm=`${DATE} --date "4 days ago" +%m`
cur_dd=`${DATE} --date "4 days ago" +%d`
# 
if [ "${nsyy}" = "auto" ]; then
  nsyy=${prv_yy}
  nsmm=${prv_mm}
  nsdd=${prv_dd}
  neyy=${cur_yy}
  nemm=${cur_mm}
  nedd=${cur_dd}
else
  # 引数が足りていない場合に止める
  [ "${nedd}" = "auto" ] && exit 1
fi
#
# ダウンロードしたい変数をここに記述する
: ${vars:=surf ugrd vgrd tmp depr hgt rh spfh vvel relv reld vpot strm o3mr cdca cwat phy2m}
#: ${vars:=ugrd vgrd tmp depr hgt rh spfh vvel relv reld vpot strm phy2m ozone pvor_th tcdc cwat clwc ciwc surf land snow phyland ice pres_th ugrd_th vgrd_th hgt_th mntsf_th vvel_th spfh_th bvf2_th}
#
: ${URL:=https://data.rda.ucar.edu/ds640.1}
: ${output:=${DATADIR_JRA3Q}/grib/Hist/Daily}
: ${sleep:=15}
: ${max_err:=5}
: ${lsleep:=3600}
: ${start:=${nsyy}${nsmm}${nsdd}}
: ${end:=${neyy}${nemm}${nedd}}


######################################################################
# get JRA-3Q/JCDAS data
######################################################################
for ivar in ${vars}
do
#
var=${ivar}
case ${ivar} in
  o3mr|cdca|cwat)  prefix='fcst_p125';;
  ugrd|vgrd|tmp|depr|hgt|rh|spfh|vvel|relv|reld|vpot|strm)   prefix='anl_p125';;
  snow)   prefix='anl_snow125';;
  surf)   prefix='anl_surf125';;
  land)   prefix='anl_land125';;
  phyland)   prefix='fcst_phyland125';;
  phy2m)   prefix='fcst_phy2m125';;
  ice)   prefix='ice125';;
  pres_th|pvor_th|ugrd_th|vgrd_th|hgt_th|mntsf_th|vvel_th|spfh_th|bvf2_th) prefix='anl_isentrop125' var=`echo ${var} | sed -e 's/_th//' `;;
  pres_thm|pvor_thm|ugrd_thm|vgrd_thm|hgt_thm|mntsf_thm|vvel_thm|spfh_thm|bvf2_thm) prefix='anl_isentrop' var=`echo ${var} | sed -e 's/_thm//' `;;
  ugrd_mdl|vgrd_mdl|tmp_mdl|hgt_mdl|vvel_mdl|spfh_mdl)  prefix='anl_mdl' var=`echo ${var} | sed -e 's/_mdl//' `;;
  snow_m)   prefix='anl_snow';;
  surf_m)   prefix='anl_surf';;
  land_m)   prefix='anl_land';;
  phyland_m)   prefix='fcst_phyland';;
  phy2m_m)   prefix='fcst_phy2m';;
  ice_m)   prefix='ice';;
  minmaxsurf_m)   prefix='minmax_surf' var=`echo ${var} | sed -e 's/_m//' `;;
  *) exit 1;;
esac
#
timeo=`${DATE} +%s`
ofirst=1
YMD=${start}
while [ ${YMD} -le ${end} ]
do
  year=`echo ${YMD} | cut -c 1-4`
  month=`echo ${YMD} | cut -c 5-6`
  day=`echo ${YMD} | cut -c 7-8`
  if [ ${ofirst} -eq 1 ]; then
    ofirst=0
    if [ ! -e  ${output}/${prefix}/${year}${month} ]; then
      mkdir -p ${output}/${prefix}/${year}${month}
    fi
  fi
  #
  ierr=1
  err_cnt=0
  while [ ${ierr} -eq 1 ]
  do
    case ${var} in
      snow|surf|land|phyland|phy2m|ice|snow_m|surf_m|land_m|phyland_m|phy2m_m|ice_m|minmaxsurf_m) 
        basename="${prefix}" ;;
     *)
        basename="${prefix}_${var}" ;;
    esac
    #
    for hh in 00 06 12 18
    do
      (${WGET} \
      ${option} \
      -O ${output}/${prefix}/${year}${month}/${basename}.${year}${month}${day}${hh} \
      ${URL}/${prefix}/${year}${month}/${basename}.${year}${month}${day}${hh} ) && ierr=0
    done
    #
    if [ ${ierr} -ne 0 ]; then
      err_cnt=`expr ${err_cnt} + 1`
      [ ${err_cnt} -ge ${max_err} ] && exit 1
      echo "sleep ${lsleep}"
      sleep ${lsleep}
    fi
  done
  echo "sleep ${sleep}"
  sleep ${sleep}
  #
  # next day
  cur_month=`echo ${YMD} | cut -c 5-6`
  YMD=`${DATE} --date "${YMD} 1 days" +%Y%m%d`
  next_month=`echo ${YMD} | cut -c 5-6`
  if [ "${cur_month}" != "${next_month}" ]; then
    ofirst=1
  fi
  # 
  timen=`${DATE} +%s`
  tdur=`expr ${timen} - ${timeo}`
  if [ ${tdur} -ge ${lsleep} ]; then
    echo "sleep ${lsleep}"
    sleep ${lsleep}
    timeo=`${DATE} +%s`
  fi
done # year month day
done # var
#
exit 0
