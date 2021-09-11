#!/bin/bash
#
#  gtfmod.sh
#  [history]
#  2014/11/05 Yamashita: first ver.
#
#  modify input data unit
#  Sea Ice Area Fraction(0-1): sic [1] * 100 ==> sic [%]
#  Cloud Area Fraction(0-1): clt [1] * 100 ==> clt [%]
#  Convective Cloud Area Fraction(0-1): convclt [1] * 100 ==> convclt [%]
#
#  E-P flux: fy [kg/m/s^2] * 6371000 [m] ==> fy [kg s-2]
#  E-P flux: fz [kg/m/s^2] * 6371000 [m] ==> fz [kg s-2]
#
#  Surface Pressure : Ps [hPa] * 100 ==> Ps [Pa]
#  p-velocity : omg [hPa/s] * 100 ==> omg [Pa/s]
#
# programs
bindir=`config.sh --bindir`
gtfact=${bindir}/gtfact
gtinfo=${bindir}/gtinfo
#
#
show_usage()
{
  echo "Usage: $1 -i input_file -o output_file"
  echo "-b original_units"
  echo "-u new_units -t title"
  exit 1
}
#
exit_shell()
{
  [ -e gtool.tmp ] && rm gtool.tmp
  [ -e tmp.txt ] && rm tmp.txt
  exit $1
}
#
[ x$1 == "x" ] && show_usage $0 1>&2
#
# default values
ifile='gtool.in'
ofile='gtool.out'
unit='NULL'
org_unit='NULL'
titl='NULL'
#
cnt=1
cmax=$#
while [ ${cnt} -le ${cmax} ]; do
  case $1 in
    -i)
      ifile=`echo $2`
      shift
      ;;
    -o)
      ofile=`echo $2`
      shift
      ;;
    -u)
      unit=`echo $2`
      shift
      ;;
    -b)
      org_unit=`echo $2`
      shift
      ;;
    -t)
      titl=`echo $2`
      shift
      ;;
    -*)
      show_usage $0 1>&2
      ;;
  esac
  cnt=`expr ${cnt} + 1`
  shift
done
#
# file names
if [ ! -e "${ifile}" ]; then
  echo "Error: Not found ${ifile}"
  show_usage $0 1>&2
elif [ "${ifile}" == "${ofile}" ]; then
  echo "Error: input file name is the same as output file"
  show_usage $0 1>&2
elif [ "x${ofile}" == "x" ]; then
  show_usage $0 1>&2
fi
#
if [ "${org_unit}" == "NULL" ]; then
  ${gtinfo} -i "${ifile}" -o tmp.txt
  ESTAT=$?
  [ ${ESTAT} -eq 0 ] || exit_shell ${ESTAT} 1>&2
  #org_unit=`cat tmp.txt | grep 'unit =' | cut -d'=' -f2 | sed -e 's/^ //' -e 's/"//g' -e 's/|//g'`
  org_unit=`cat tmp.txt | grep 'unit =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
  echo "org_unit = ${org_unit}"
fi
#
if [ "x${org_unit}" == "x" ]; then
  echo "Warn: Do nothing"
  exit_shell 0 1>&2
fi
echo "org_unit = ${org_unit}"
echo "unit = ${unit}"
#
[ "${org_unit}" == 'kg/m/s^2' ] && org_unit='kg m s-2'
[ "${org_unit}" == 'kg/m/s2' ] && org_unit='kg m s-2'
[ "${org_unit}" == 'hPa/s' ] && org_unit='hPa s-1'
#
[ "${unit}" == 'kg/s^2' ] && unit='kg s-2'
[ "${unit}" == 'Pa/s' ] && unit='Pa s-1'
#
if [ "${org_unit}" == '1' -a "${unit}" == '%' ]; then
  fact=100.d0
elif [ "${org_unit}" == 'kg m s-2' -a "${unit}" == 'kg s-2' ]; then
  fact=6.370d6
elif [ "${org_unit}" == 'hPa' -a "${unit}" == 'Pa' ]; then
  fact=100.d0
elif [ "${org_unit}" == 'hPa s-1' -a "${unit}" == 'Pa s-1' ]; then
  fact=100.d0
else
  echo "Warn: Do nothing"
  exit_shell 0 1>&2
fi
echo "new_unit = ${unit}"
#
#
# gtfact
echo "${gtfact} -i '${ifile}' -o '${ofile}' -item '${item}' -titl '${titl}' -fact '${fact}'"
${gtfact} -i "${ifile}" -o "${ofile}" -unit "${unit}" -titl "${titl}" -fact "${fact}" 
ESTAT=$?
#
exit_shell ${ESTAT} 1>&2
