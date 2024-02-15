#!/bin/bash
######################################################################
# initialization
######################################################################
. /etc/bashrc 
#set -eu
: ${DATE:=gdate}
#
# manually (データの期間を指定する場合は引数として与える)
nsyy=${1:-2023}
nsmm=${2:-01}
nsdd=${3:-01}
neyy=${4:-2023}
nemm=${5:-12}
nedd=${6:-31}
var=${7:-"null"}
#
# 
# 引数が足りていない場合に止める
[ "${var}" = "null" ] && exit 1
#
: ${verbose:=f}
: ${idir:=${DATADIR_JRA3Q}/grib/Hist/Daily}
: ${check:="ls"}
: ${checkg:="wgrib2"}
: ${start:=${nsyy}${nsmm}${nsdd}}
: ${end:=${neyy}${nemm}${nedd}}


######################################################################
# check JRA-3Q/JCDAS data
######################################################################
case ${var} in
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
err_cnt=0
YMD=${start}
while [ ${YMD} -le ${end} ]
do
  year=`echo ${YMD} | cut -c 1-4`
  month=`echo ${YMD} | cut -c 5-6`
  day=`echo ${YMD} | cut -c 7-8`
  #
  case ${var} in
    snow|surf|land|phyland|phy2m|ice|snow_m|surf_m|land_m|phyland_m|phy2m_m|ice_m|minmaxsurf_m) 
      basename="${prefix}" ;;
    *)
      basename="${prefix}_${var}" ;;
  esac
  #
  for hh in 00 06 12 18
  do
    [ ${verbose} = "t" ] && echo "${basename}.${YMD}${hh}"
    ierr=0
    (${check} ${idir}/${prefix}/${year}${month}/${basename}.${YMD}${hh} >& /dev/null) || ierr=1
    if [ ${ierr} -eq 0 ]; then 
      len=$(${checkg} ${idir}/${prefix}/${year}${month}/${basename}.${YMD}${hh} | wc -l) >& /dev/null
      [ ${len} -eq 0 ] && ierr=1
    fi
    [ ${ierr} -ne 0 ] && echo "NOT FOUND: ${basename}.${YMD}${hh}"
    err_cnt=`expr ${err_cnt} + ${ierr}`
  done
  #
  cur_month=`echo ${YMD} | cut -c 5-6`
  YMD=`${DATE} --date "${YMD} 1 days" +%Y%m%d`
  # 
done # year month day
#
exit ${err_cnt}
