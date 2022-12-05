#!/bin/sh
#
#  gribid.sh
#  [history]
#  2017/02/03 Yamashita: first ver.
#
########################################################################
#
ESTAT=1
show_usage()
{
  echo "Usege: `basename $0`"
  echo "--titl|-t [input variable] (title)"
  echo "--pref|-p [input variable] (prefix)"
  echo "--base|-b [input variable] (basename)"
  echo "--grid|-g [input variable] (grib id)"
  echo "--desc|-d [input variable] (description)"
  echo "--help|-h (show this help)"
  exit_shell ${ESTAT} 1>&2
}
#
exit_shell()
{
  [ -e tmp.txt ] && rm tmp.txt
  exit $1
}
#
[ x$1 == "x" ] && show_usage $0 1>&2
#
#
#
var='NULL'
title='NULL'
otitl=f
prefix='NULL'
opref=f
basename='NULL'
obase=f
grid='NULL'
ogrid=f
desc='NULL'
odesc=f

#
cnt=1
cmax=$#
while [ ${cnt} -le ${cmax} ]; do
  case $1 in
    --titl|-t)
      var=`echo $2 | sed -e "s/'//g"`
      otitl=t
      shift
      ;;
    --pref|-p)
      var=`echo $2 | sed -e "s/'//g"`
      opref=t
      shift
      ;;
    --base|-b)
      var=`echo $2 | sed -e "s/'//g"`
      obase=t
      shift
      ;;
    --grid|-g)
      var=`echo $2 | sed -e "s/'//g"`
      ogrid=t
      shift
      ;;
    --desc|-d)
      var=`echo $2 | sed -e "s/'//g"`
      odesc=t
      shift
      ;;
    --help|-h)
      ESTAT=0
      show_usage $0 1>&2
      ;; 
    -*)
      show_usage $0 1>&2
      ;;
  esac
  cnt=`expr ${cnt} + 1`
  shift
done
#
#

cat > tmp.txt <<EOF
uwnd:U:analysis:data_prs:kpds=131,100:"U velocity [m s**-1]"
vwnd:V:analysis:data_prs:kpds=132,100:"V velocity [m s**-1]"
tmp:T:analysis:data_prs:kpds=130,100:"Temperature [K]"
geo:Z:analysis:data_prs:kpds=129,100:"Geopotential [m**2 s**-2]"
omega:W:analysis:data_prs:kpds=135,100:"Vertical velocity [Pa s**-1]"
reld:D:analysis:data_prs:kpds=155,100:"Divergence [s**-1]"
relv:VO:analysis:data_prs:kpds=138,100:"Vorticity (relative) [s**-1]"
rh:R:analysis:data_prs:kpds=157,100:"Relative humidity [%]"
spfh:Q:analysis:data_prs:kpds=133,100:"Specific humidity [kg kg**-1]"
ciwc:CIWC:analysis:data_prs:kpds=247,100:"Cloud ice water content [kg kg**-1]"
clwc:CLWC:analysis:data_prs:kpds=246,100:"Cloud liquid water content [kg kg**-1]"
crwc:CRWC:analysis:data_prs:kpds=75,100:"Cloud rain water content [kg kg**-1]"
cswc:CSWC:analysis:data_prs:kpds=76,100:"Cloud snow water content [kg kg**-1]"
cfrac:CC:analysis:data_prs:kpds=248,100:"Cloud cover [(0 - 1)]"
sp:PV:analysis:data_prs:kpds=60,100:"Potential vorticity [K m**2 kg**-1 s**-1]"
ozone:O3:analysis:data_prs:kpds=203,100:"Ozone mass mixing ratio [kg kg**-1]"
u10:10U:analysis:data_sur:kpds=165,1:"10 metre U wind component [m s**-1]"
v10:10V:analysis:data_sur:kpds=166,1:"10 metre V wind component [m s**-1]"
t2:2T:analysis:data_sur:kpds=167,1:"2 metre temperature [K]"
td2:2D:analysis:data_sur:kpds=168,1:"2 metre dewpoint temperature [K]"
slp:MSL:analysis:data_sur:kpds=151,1:"Mean sea level pressure [Pa]"
ps:SP:analysis:data_sur:kpds=134,1:"Surface pressure [Pa]"
tco3:TCO3:analysis:data_sur:kpds=206,1:"Total column ozone [kg m**-2]"
ccover:TCC:analysis:data_sur:kpds=164,1:"Total cloud cover [(0 - 1)]"
ccoverh:HCC:analysis:data_sur:kpds=188,1:"High cloud cover [(0 - 1)]"
ccoverm:MCC:analysis:data_sur:kpds=187,1:"Medium cloud cover [(0 - 1)]"
ccoverl:LCC:analysis:data_sur:kpds=186,1:"Low cloud cover [(0 - 1)]"
sst:SSTK:analysis:data_sur:kpds=34,1:"Sea surface temperature [K]"
ice:CI:analysis:data_sur:kpds=31,1:"Sea-ice cover [(0 - 1)]"
albedo:AL:analysis:data_sur:kpds=174,1:"Albedo [(0 - 1)]"
tcw:TCW:analysis:data_sur:kpds=136,1:"Total column water [kg m**-2]"
tcwv:TCWV:analysis:data_sur:kpds=137,1:"Total column water vapour [kg m**-2]"
snod:SNOD:analysis:data_sur:kpds=141,1:"Snow depth [m of water equivalent]"
sr:SR:analysis:data_sur:kpds=173,1:"Surface roughness [m]"
skt:SKT:analysis:data_sur:kpds=235,1:"Skin temperature [K]"
tsn:TSN:analysis:data_sur:kpds=238,1:"Temperature of snow layer [K]"
asn:ASN:analysis:data_sur:kpds=32,1:"Snow albedo [(0 - 1)]"
rsn:RSN:analysis:data_sur:kpds=33,1:"Snow density [kg m**-3]"
EOF

#
title=`cat tmp.txt | awk -F : '($1 == "'${var}'"){print $2}' `
prefix=`cat tmp.txt | awk -F : '($1 == "'${var}'"){print $3}' `
basename=`cat tmp.txt | awk -F : '($1 == "'${var}'"){print $4}' `
grid=`cat tmp.txt | awk -F : '($1 == "'${var}'"){print $5}' `
desc=`cat tmp.txt | awk -F : '($1 == "'${var}'"){print $6}' `

#
[ ${otitl} == "t" ] && echo ${title}
[ ${opref} == "t" ] && echo ${prefix}
[ ${obase} == "t" ] && echo ${basename}
[ ${ogrid} == "t" ] && echo ${grid}
[ ${odesc} == "t" ] && echo ${desc}
#
exit_shell 0 1>&2
#
exit 0
