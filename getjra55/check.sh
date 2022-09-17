#!/bin/sh
# 
######################################################################
# initialization
######################################################################
set -eu
: ${DATE:=gdate}
#
#: ${nsyy:=1958}
: ${nsyy:=1979}
: ${nsmm:=01}
: ${nsdd:=01}
: ${neyy:=2020}
: ${nemm:=12}
: ${nedd:=31}
#
: ${vars:=hgt} # comp.
#: ${vars:=ugrd rh hgt} # comp.
#: ${vars:=ugrd vgrd tmp depr spfh vvel relv reld vpot strm} # comp.
#: ${vars:=column phy2m ozone}
#: ${vars:=rh vgrd tmp depr spfh vvel relv reld vpot strm column phy2m ozone pvor_th tcdc cwat clwc ciwc surf land snow phyland ice pres_th ugrd_th vgrd_th hgt_th mntsf_th vvel_th spfh_th bvf2_th}
#: ${vars:=hgt rh vgrd tmp depr spfh vvel relv reld vpot strm column phy2m ozone pvor_th tcdc cwat clwc ciwc surf land snow phyland ice pres_th ugrd_th vgrd_th hgt_th mntsf_th vvel_th spfh_th bvf2_th}
#: ${vars:=ugrd vgrd tmp depr hgt rh spfh vvel relv reld vpot strm column phy2m ozone pvor_th tcdc cwat clwc ciwc surf land snow phyland ice pres_th ugrd_th vgrd_th hgt_th mntsf_th vvel_th spfh_th bvf2_th}
#
: ${verbose:=f}
: ${idir:=./gpvjma.ccs.hpcc.jp/data/jra55/Hist/Daily}
: ${check:="ls"}
: ${checkg:="wgrib"}
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
    [ ${verbose} = "t" ] && echo "${var}: ${year}/${month}/${day}"
    #
    if [ ${ofirst} -eq 1 ]; then
      # new month
      ofirst=0
      filenames=
      case ${var} in
        snow|surf|land|phyland|phy2m|column|ice|snow_m|surf_m|land_m|phyland_m|phy2m_m|column_m|ice_m) 
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
        [ ${verbose} = "t" ] && echo "${file_name}"
        ierr=0
        (${check} ${idir}/${prefix}/${year}${month}/${file_name} >& /dev/null) || ierr=1
        [ ${ierr} -ne 0 ] && echo "NOT FOUND: ${file_name}"
      done
    fi
    #
    for hh in 00 06 12 18
    do
      case ${var} in
        snow|surf|column|land|phy2m|ice|column_m|surf_m|land_m|phyland_m|ice_m|minmaxsurf_m)
        basename="${prefix}" ;;
        *)
        basename="${prefix}_${var}" ;;
      esac
      #
      [ ${verbose} = "t" ] && echo "${basename}.${YMD}${hh}"
#      (${checkg} ${idir}/${prefix}/${year}${month}/${basename}.${YMD}${hh} >& /dev/null)
      ierr=0
      (${checkg} ${idir}/${prefix}/${year}${month}/${basename}.${YMD}${hh} >& /dev/null) || ierr=1
      [ ${ierr} -ne 0 ] && echo "NOT FOUND: ${basename}.${YMD}${hh}"
#      ESTAT=$?
    done # hh
    #
    # next day
    YMD=$( ${DATE} --date "${YMD} 1 days" +%Y%m%d ) || exit 1
    # next month
    nmonth=`echo ${YMD} | cut -c 5-6`
    if [ ${month} -ne ${nmonth} ]; then
      ofirst=1
    fi
    # 
  done # year month day
done # var
#
exit 0
