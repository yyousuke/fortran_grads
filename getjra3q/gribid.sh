#!/bin/bash
#
#  gribid.sh
#  [history]
#  2017/02/03 Yamashita: first ver. for JRA-55
#  2024/02/13 Yamashita: for JRA-3Q
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
uwnd:UGRD:anl_p125:anl_p125_ugrd:kpds=33,100:"U-Component of Wind [m/s]"
vwnd:VGRD:anl_p125:anl_p125_vgrd:kpds=34,100:"V-component of wind [m/s]"
tmp:TMP:anl_p125:anl_p125_tmp:kpds=11,100:"Temperature [K]"
hgt:HGT:anl_p125:anl_p125_hgt:kpds=7,100:"Geopotential height [gpm]"
omega:VVEL:anl_p125:anl_p125_vvel:kpds=39,100:"Vertical Velocity (Pressure) [Pa/s]"
depr:DEPR:anl_p125:anl_p125_depr:kpds=18,100:"Dew Point Depression (or Deficit) [K]"
reld:RELD:anl_p125:anl_p125_reld:kpds=44,100:"Relative Divergence [1/s]"
relv:RELV:anl_p125:anl_p125_relv:kpds=43,100:"Relative Vorticity [1/s]"
rh:RH:anl_p125:anl_p125_rh:kpds=52,100:"Relative Humidity [%]"
spfh:SPFH:anl_p125:anl_p125_spfh:kpds=51,100:"Specific Humidity [kg/kg]"
strm:STRM:anl_p125:anl_p125_strm:kpds=35,100:"Stream Function [m^2/s]"
vpot:VPOT:anl_p125:anl_p125_vpot:kpds=36,100:"Velocity Potential [m^2/s]"
cdca:CDCA:fcst_p125:fcst_p125_cdca:kpds=229,100:"Cloud Amount [%]"
o3mr:O3MR:fcst_p125:fcst_p125_o3mr:kpds=237,100:"Ozone Mixing Ratio [kg/kg]"
cwat:parm=41:fcst_p125:fcst_p125_cwat:kpds=221,100:"Cloud water [kg/kg]"
u10:UGRD:anl_surf125:anl_surf125:kpds=33,105:"10 m above ground U-Component of Wind [m/s]"
v10:VGRD:anl_surf125:anl_surf125:kpds=34,105:"10 m above ground V-Component of Wind [m/s]"
t2:TMP:anl_surf125:anl_surf125:kpds=11,105:"2 m above ground Temperature [K]"
q2:SPFH:anl_surf125:anl_surf125:kpds=51,105:"2 m above ground Specific Humidity [kg/kg]"
rg2:RH:anl_surf125:anl_surf125:kpds=52,105:"2 m above ground Relative Humidity [%]"
td2:DEPR:anl_surf125:anl_surf125:kpds=18,105:"2 m above ground Dew Point Depression (or Deficit) [K]"
slp:PRMSL:anl_surf125:anl_surf125:kpds=2,102:"Pressure Reduced to MSL [Pa]"
ps:PRES:anl_surf125:anl_surf125:kpds=1,1:"Surface Pressure [Pa]"
th0:POT:anl_surf125:anl_surf125:kpds=13,1:"surface Potential temperature [K]"
tciwv:TCIWV:anl_surf125:anl_surf125:kpds=18,105:"Total Column Integrated Water Vapour [kg/m^2]"
weasd:WEASD:anl_surf125:anl_surf125:kpds=18,105:"Water Equivalent of Accumulated Snow Depth [kg/m^2]"
snod:SNOD:anl_snow125:anl_snow125:kpds=66,1:"Surface Snow Depth [m]"
ccover:TCDC:fcst_surf125:fcst_surf125:kpds=71,101:"90-1100 hPa Total cloud cover [%]"
ccoverl:LCDC:fcst_surf125:fcst_surf125:kpds=73,101:"850-1100 hPa Low cloud cover [%]"
ccoverm:MCDC:fcst_surf125:fcst_surf125:kpds=74,101:"500-850 hPa Medium cloud cover [%]"
ccoverh:HCDC:fcst_surf125:fcst_surf125:kpds=75,101:"90-500 hPa High cloud cover [%]"
lhtf:LHTFL:fcst_phy2m125:fcst_phy2m125:kpds=121,1:"Surface Latent Heat Net Flux [W/m^2]"
shtf:SHTFL:fcst_phy2m125:fcst_phy2m125:kpds=122,1:"Surface Sensible Heat Net Flux [W/m^2]"
rain:TPRATE:fcst_phy2m125:fcst_phy2m125:kpds=61,1:"Surface Total Precipitation Rate [kg/m^2/s]"
snow:TSRWE:fcst_phy2m125:fcst_phy2m125:kpds=64,1:"Surface Snowfall Rate Water Equivalent [kg/m^2/s]"
rainl:LSPRATE:fcst_phy2m125:fcst_phy2m125:kpds=62,1:"Surface Large Scale Precipitation Rate [kg/m^2/s]"
rainc:CPRAT:fcst_phy2m125:fcst_phy2m125:kpds=63,1:"Surface Convective Precipitation Rate [kg/m^2/s]"
evap:EVARATE:fcst_phy2m125:fcst_phy2m125:kpds=57,1:"Surface Evaporation Rate [kg/m^2/s]"
taux:UFLX:fcst_phy2m125:fcst_phy2m125:kpds=124,1:"Surface Momentum flux, u-component [N/m^2]"
tauy:VFLX:fcst_phy2m125:fcst_phy2m125:kpds=125,1:"Surface Momentum flux, v-component [N/m^2]"
ssrdc:DSWRFCS:fcst_phy2m125:fcst_phy2m125:kpds=161,1:"Surface Clear Sky Downward Short-Wave Radiation Flux [W/m^2]"
slrdc:DLWRFCS:fcst_phy2m125:fcst_phy2m125:kpds=163,1:"Surface Clear Sky Downward Long-Wave Radiation Flux [W/m^2]"
ssrd:DSWRF S:fcst_phy2m125:fcst_phy2m125:kpds=161,1:"Surface Downward Short-Wave Radiation Flux [W/m^2]"
slrd:DLWRF S:fcst_phy2m125:fcst_phy2m125:kpds=163,1:"Surface Downward Long-Wave Radiation Flux [W/m^2]"
ssruc:USWRFCS:fcst_phy2m125:fcst_phy2m125:kpds=160,1:"Surface Clear Sky Upward Short-Wave Radiation Flux [W/m^2]"
ssru:USWRF U:fcst_phy2m125:fcst_phy2m125:kpds=211,1:"Surface Upward Short-Wave Radiation Flux [W/m^2]"
slru:ULWRF U:fcst_phy2m125:fcst_phy2m125:kpds=212,1:"Surface Upward Long-Wave Rad. Flux [W/m^2]"
osrc:USWRFCS:fcst_phy2m125:fcst_phy2m125:kpds=160,8:"Top of atmosphere Clear Sky Upward Short-Wave Radiation Flux [W/m^2]"
olrc:NLWRCS:fcst_phy2m125:fcst_phy2m125:kpds=162,1:"Top of atmosphere Clear Sky Net Long-Wave Radiation Flux [W/m^2]"
osr:USWRF U:fcst_phy2m125:fcst_phy2m125:kpds=211,8:"Top of atmosphere Upward Upward Short-Wave Radiation Flux [W/m^2]"
olr:ULWRF U:fcst_phy2m125:fcst_phy2m125:kpds=212,8:"Top of atmosphere Upward Long-Wave Rad. Flux [W/m^2]"
olrd:DSWRF:fcst_phy2m125:fcst_phy2m125:kpds=163,1:"Top of atmosphere Downward Short-Wave Radiation Flux [W/m^2]"
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
