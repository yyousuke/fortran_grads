#!/bin/sh
# 筑波大学　計算科学研究センターからJRA-55データを取得する
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
: ${option:="-N -r -np --level=0"}
#
prv_yy=`${DATE} --date "10 days ago" +%Y`
prv_mm=`${DATE} --date "10 days ago" +%m`
cur_yy=`${DATE} --date "2 days ago" +%Y`
cur_mm=`${DATE} --date "2 days ago" +%m`
cur_dd=`${DATE} --date "2 days ago" +%d`
# auto
: ${nsyy:=${prv_yy}}
: ${nsmm:=${prv_mm}}
: ${nsdd:=01}
: ${neyy:=${cur_yy}}
: ${nemm:=${cur_mm}}
: ${nedd:=${cur_dd}}
# manually (取得するデータの期間を指定する場合はここで行う)
#: ${nsyy:=2018}
#: ${nsmm:=03}
#: ${neyy:=2018}
#: ${nemm:=03}
#
# ダウンロードしたい変数をここに記述する
: ${vars:=ugrd vgrd tmp depr hgt rh spfh vvel relv reld vpot strm tcdc cwat clwc ciwc}
#: ${vars:=ugrd vgrd tmp depr hgt rh spfh vvel relv reld vpot strm column phy2m ozone pvor_th tcdc cwat clwc ciwc surf land snow phyland ice pres_th ugrd_th vgrd_th hgt_th mntsf_th vvel_th spfh_th bvf2_th}
#
: ${user:=user_name} # 登録したユーザ名
: ${pass:=password}  # パスワード
: ${URL:=http://gpvjma.ccs.hpcc.jp/data/jra55/Hist/Daily}
: ${sleep:=15}
: ${max_err:=5}
: ${lsleep:=3600}
: ${start:=${nsyy}${nsmm}${nsdd}}
: ${end:=${neyy}${nemm}${nedd}}


######################################################################
# get JRA-55/JCDAS data
######################################################################
for ivar in ${vars}
do
#
var=${ivar}
case ${ivar} in
  ozone|tcdc|cwat|clwc|ciwc)  prefix='fcst_p125';;
  ugrd|vgrd|tmp|depr|hgt|rh|spfh|vvel|relv|reld|vpot|strm)   prefix='anl_p125';;
  snow)   prefix='anl_snow125';;
  surf)   prefix='anl_surf125';;
  land)   prefix='anl_land125';;
  phyland)   prefix='fcst_phyland125';;
  phy2m)   prefix='fcst_phy2m125';;
  column)   prefix='fcst_column125';;
  ice)   prefix='ice125';;
  pres_th|pvor_th|ugrd_th|vgrd_th|hgt_th|mntsf_th|vvel_th|spfh_th|bvf2_th) prefix='anl_isentrop125' var=`echo ${var} | sed -e 's/_th//' `;;
  pres_thm|pvor_thm|ugrd_thm|vgrd_thm|hgt_thm|mntsf_thm|vvel_thm|spfh_thm|bvf2_thm) prefix='anl_isentrop' var=`echo ${var} | sed -e 's/_thm//' `;;
  ugrd_mdl|vgrd_mdl|tmp_mdl|hgt_mdl|vvel_mdl|spfh_mdl)  prefix='anl_mdl' var=`echo ${var} | sed -e 's/_mdl//' `;;
  snow_m)   prefix='anl_snow';;
  surf_m)   prefix='anl_surf';;
  land_m)   prefix='anl_land';;
  phyland_m)   prefix='fcst_phyland';;
  phy2m_m)   prefix='fcst_phy2m';;
  column_m)   prefix='fcst_column';;
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
  # new month
  if [ ${ofirst} -eq 1 ]; then
    ofirst=0
    filenames=
    case ${var} in
      snow|surf|land|phyland|phy2m|column|ice|snow_m|surf_m|land_m|phyland_m|phy2m_m|column_m|ice_m|minmaxsurf_m) 
        basename="${prefix}" ;;
      *) 
        basename="${prefix}_${var}" ;;
    esac
    filenames=`echo ${filenames} ${basename}.ctl ${basename}.idx`
    #
    if [ "${prefix}" = "anl_mdl" -o "${prefix}" = "anl_thm" ]; then
      filenames=`echo ${filenames} ${prefix}_${var}_fl.ctl TL319.pdef`
    fi
    #
    for file_name in ${filenames}
    do
      ${WGET} \
      ${option} \
      ${user:+--http-user=${user}} \
      ${pass:+--http-passwd=${pass}} \
      ${URL}/${prefix}/${year}${month}/${file_name}
      ESTAT=$?
      [ ${ESTAT} -eq 0 ] || exit 1
    done
  fi
  #
  ierr=1
  err_cnt=0
  while [ ${ierr} -eq 1 ]
  do
    case ${var} in
      snow|surf|land|phyland|phy2m|column|ice|snow_m|surf_m|land_m|phyland_m|phy2m_m|column_m|ice_m|minmaxsurf_m) 
        basename="${prefix}" ;;
     *)
        basename="${prefix}_${var}" ;;
    esac
    #
    for hh in 00 06 12 18
    do
      (${WGET} \
      ${option} \
      ${user:+--http-user=${user}} \
      ${pass:+--http-password=${pass}} \
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
