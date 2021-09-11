#!/bin/bash
#
#  gtfact.sh
#  [history]
#  2013/10/14 Yamashita: first ver.
#
#
# plev [hPa] *100 ==> plev [Pa]
# Ps [hPa] *100 ==> Ps [Pa]
# omg [hPa/s] *100 ==> omg [Pa/s]
# sad [cm2/cm3] *100 ==> sad [m2/m3]
# fxXX [1/cm3/s] * 1e+6/Na ==> fxXX [mole/m3/s]
# CXX [ppmv] *1e-6 ==> CXX [mole/mole]
#
# change name of unit
# qXX [mixing ratio] ==> qXX [mole/mole]
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
  echo "-u units -t title"
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
${gtinfo} -i "${ifile}" -o tmp.txt
ESTAT=$?
[ ${ESTAT} -eq 0 ] || exit_shell ${ESTAT} 1>&2
#org_unit=`cat tmp.txt | grep 'unit =' | cut -d'=' -f2 | sed -e 's/^ //' -e 's/"//g' -e 's/|//g'`
org_unit=`cat tmp.txt | grep 'unit =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
echo "org_unit = ${org_unit}"
#
if [ "x${org_unit}" == "x" ]; then
  echo "Warn: Do nothing"
  exit_shell 0 1>&2
fi
#
if [ ${org_unit} == 'cm2/cm3air' ]; then
  unit='m|2"/m|3"'
  fact=100.d0
elif [ ${org_unit} == 'hPa' ]; then
  unit='Pa'
  fact=100.d0
elif [ ${org_unit} == 'hPa/s' ]; then
  unit='Pa/s'
  fact=100.d0
elif [ ${org_unit} == '1/cm3/s' ]; then
  unit='mole/m|3"/s'
  fact=`echo "1.e+6 6.0221415e23" | awk '{printf "%.7e\n", $1/$2}'` # (*1e+6/Na)
elif [ ${org_unit} == 'ppmv' ]; then
  unit='mole/mole'
  fact=1.d-6
elif [ ${org_unit} == 'mixingratio' ]; then
  unit='mole/mole'
  fact=1.d0
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
