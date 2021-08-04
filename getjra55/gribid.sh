#!/bin/sh
#
#  gribid.sh
#  [history]
#  2017/02/03 Yamashita: first ver.
#
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
uwnd:UGRDprs:anl_p125:anl_p125_ugrd:kpds=33,100:"u-component of wind [m/s]"
vwnd:VGRDprs:anl_p125:anl_p125_vgrd:kpds=34,100:"v-component of wind [m/s]"
tmp:TMPprs:anl_p125:anl_p125_tmp:kpds=11,100:"Temperature [K]"
hgt:HGTprs:anl_p125:anl_p125_hgt:kpds=7,100:"Geopotential height [gpm]"
omega:VVELprs:anl_p125:anl_p125_vvel:kpds=39,100:"Vertical velocity [Pa/s]"
depr:DEPRprs:anl_p125:anl_p125_depr:kpds=18,100:"Dew-point depression [K]"
reld:RELDprs:anl_p125:anl_p125_reld:kpds=44,100:"Relative divergence [1/s]"
relv:RELVprs:anl_p125:anl_p125_relv:kpds=43,100:"Relative vorticity [1/s]"
rh:RHprs:anl_p125:anl_p125_rh:kpds=52,100:"Relative humidity [%]"
spfh:SPFHprs:anl_p125:anl_p125_spfh:kpds=51,100:"Specific humidity [kg/kg]"
strm:STRMprs:anl_p125:anl_p125_strm:kpds=35,100:"Stream function [m^2/s]"
vpot:VPOTprs:anl_p125:anl_p125_vpot:kpds=36,100:"Velocity potential [m^2/s]"
ciwc:CIWCprs:fcst_p125:fcst_p125_ciwc:kpds=229,100:"Cloud ice [kg/kg]"
clwc:CLWCprs:fcst_p125:fcst_p125_clwc:kpds=228,100:"Cloud liquid water [kg/kg]"
cwat:CWATprs:fcst_p125:fcst_p125_cwat:kpds=221,100:"Cloud water [kg/kg]"
ozone:OZONEprs:fcst_p125:fcst_p125_ozone:kpds=237,100:"Ozone mixing ratio [mg/kg]"
tcdc:TCDCprs:fcst_p125:fcst_p125_tcdc:kpds=71,100:"Total cloud cover [%]"
pwat:PWATclm:anl_column125:anl_column125:kpds=54,200:"atmos column Precipitable water [kg/m^2]"
uthe:UTHEclm:anl_column125:anl_column125:kpds=190,200:"atmos column Zonal thermal energy flux [W/m]"
vthe:VTHEclm:anl_column125:anl_column125:kpds=191,200:"atmos column Meridional thermal energy flux [W/m]"
uwv:UWVclm:anl_column125:anl_column125:kpds=157,200:"atmos column Zonal water vapour flux [kg/m/s]"
vwv:VWVclm:anl_column125:anl_column125:kpds=152,200:"atmos column Meridional water vapour flux [Kg/m/s]"
rain:TPRAT:fcst_phy2m125:fcst_phy2m125:kpds=61,1:"surface Total precipitation [mm/day]"
rainl:LPRA:fcst_phy2m125:fcst_phy2m125:kpds=62,1:"surface Large scale precipitation [mm/day]"
rainc:CPRAT:fcst_phy2m125:fcst_phy2m125:kpds=63,1:"surface Convective precipitation [mm/day]"
evap:EVP:fcst_phy2m125:fcst_phy2m125:kpds=57,1:"surface Evaporation [mm/day]"
shtf:SHTFL:fcst_phy2m125:fcst_phy2m125:kpds=122,1:"surface Sensible heat flux [W/m^2]"
lhtf:LHTFL:fcst_phy2m125:fcst_phy2m125:kpds=121,1:"surface Latent heat flux [W/m^2]"
snow:SRWEQ:fcst_phy2m125:fcst_phy2m125:kpds=64,1:"surface Snowfall rate water equivalent [mm/day]"
ssr:USWRF:fcst_phy2m125:fcst_phy2m125:kpds=211,1:"surface Upward solar radiation flux [W/m^2]"
taux:UFLX:fcst_phy2m125:fcst_phy2m125:kpds=124,1:"surface Momentum flux, u-component [N/m^2]"
tauy:VFLX:fcst_phy2m125:fcst_phy2m125:kpds=125,1:"surface Momentum flux, v-component [N/m^2]"
ssruc:CSUSF:fcst_phy2m125:fcst_phy2m125:kpds=160,1:"surface Clear sky upward solar radiation flux [W/m^2]"
ssrdc:CSDSF:fcst_phy2m125:fcst_phy2m125:kpds=161,1:"surface Clear sky downward solar radiation flux [W/m^2]"
olrc:CSULF:fcst_phy2m125:fcst_phy2m125:kpds=162,1:"top of atmos Clear sky upward long wave radiation flux [W/m^2]"
slrdc:CSDLF:fcst_phy2m125:fcst_phy2m125:kpds=163,1:"surface Clear sky downward long wave radiation flux [W/m^2]"
osrc:CSUSF:fcst_phy2m125:fcst_phy2m125:kpds=160,8:"top of atmos Clear sky upward solar radiation flux [W/m^2]"
ssru:USWRF:fcst_phy2m125:fcst_phy2m125:kpds=211,1:"surface Upward solar radiation flux [W/m^2]"
osr:USWRF:fcst_phy2m125:fcst_phy2m125:kpds=211,8:"top of atmos Upward solar radiation flux [W/m^2]"
slru:ULWRF:fcst_phy2m125:fcst_phy2m125:kpds=212,1:"surface Upward long wave radiation flux [W/m^2]"
olr:ULWRF:fcst_phy2m125:fcst_phy2m125:kpds=212,8:"top of atmos Upward long wave radiation flux [W/m^2]"
u10:UGRD10m:anl_surf125:anl_surf125:kpds=33,105:"10 m above ground u-component of wind [m/s]"
v10:VGRD10m:anl_surf125:anl_surf125:kpds=34,105:"10 m above ground v-component of wind [m/s]"
t2:TMP2m:anl_surf125:anl_surf125:kpds=11,105:"2 m above ground Temperature [K]"
q2:SPFH2m:anl_surf125:anl_surf125:kpds=51,105:"2 m above ground Specific humidity [kg/kg]"
rg2:RH2m:anl_surf125:anl_surf125:kpds=52,105:"2 m above ground Relative humidity [kg/kg]"
slp:PRMSLmsl:anl_surf125:anl_surf125:kpds=2,102:"mean-sea level Pressure reduced to MSL [Pa]"
ps:PRESsfc:anl_surf125:anl_surf125:kpds=1,1:"surface Pressure [Pa]"
th0:POTsfc:anl_surf125:anl_surf125:kpds=13,1:"surface Potential temperature [K]"
td2:DEPR2m:anl_surf125:anl_surf125:kpds=18,105:"2 m above ground Dew-point depression [K]"
ccover:TCDC:fcst_surf125:fcst_surf125:kpds=71,101:"90-1100 hPa Total cloud cover [%]"
ccoverh:HCDC:fcst_surf125:fcst_surf125:kpds=75,101:"90-500 hPa High cloud cover [%]"
ccoverm:MCDC:fcst_surf125:fcst_surf125:kpds=74,101:"500-850 hPa Medium cloud cover [%]"
ccoverl:LCDC:fcst_surf125:fcst_surf125:kpds=73,101:"850-1100 hPa Low cloud cover [%]"
snod:SNOD:anl_snow125:anl_snow125:kpds=66,1:"surface Snow depth [m]"
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
