#!/bin/bash
#
#  gtrepint.sh
#  [history]
#  2013/10/21 Yamashita: first ver.
#
#
# programs
bindir=`config.sh --bindir`
gtinfo=${bindir}/gtinfo
gtcut=${bindir}/gtcut
gtrepint=${bindir}/gtrepint
gtreplace=${bindir}/gtreplace
#
#
#
show_usage()
{
  echo "Usage: $1 -o output_file"
  echo "-i input_file_for_time_def"
  echo "-r input_reference_data_file"
  echo "-nsyy start year"
  echo "-neyy end year"
  exit 1
}
#
exit_shell()
{
  [ -e tmp.txt ] && rm tmp.txt
  n=1
  while test ${n} -le 3
  do
    [ -e tmp0${n}.gt ] && rm tmp0${n}.gt
    n=`expr $n + 1`
  done
  exit $1
}
#
[ x$1 == "x" ] && show_usage $0 1>&2
#
#
# (1) prepare
# default values
ifile='gtool.in'
rfile='ref.gt'
ofile='gtool.out'
nsyy=1959
neyy=2100
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
    -r)
      rfile=`echo $2`
      shift
      ;;
    -nsyy)
      nsyy=`echo $2`
      shift
      ;;
    -neyy)
      neyy=`echo $2`
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
elif [ ! -e "${rfile}" ]; then
  echo "Error: Not found ${rfile}"
  show_usage $0 1>&2
fi
#
#
# (2) get axis names and sizes
${gtinfo} -i "${ifile}" -o tmp.txt
ESTAT=$?
[ ${ESTAT} -eq 0 ] || exit_shell ${ESTAT} 1>&2
haxisx=`cat tmp.txt | grep 'haxisx =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
haxisy=`cat tmp.txt | grep 'haxisy =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
haxisz=`cat tmp.txt | grep 'haxisz =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
imax=`cat tmp.txt | grep 'imax =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
jmax=`cat tmp.txt | grep 'jmax =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
kmax=`cat tmp.txt | grep 'kmax =' | cut -d'=' -f2 | sed -e 's/ //g' -e 's/"//g' -e 's/|//g'`
echo "(${haxisx},${haxisy},${haxisz})${imax}x${jmax}x${kmax}"
#
#
# (3) set output data time ==> tmp01.gt
# -i: input head file for definition of output data time
echo "${gtcut} -nsyy ${nsyy} -nsmm 1 -nsdd 1 -neyy ${neyy} -nemm 12 -nedd 31 -i ${ifile} -o tmp01.gt"
(${gtcut} -nsyy ${nsyy} -nsmm 1 -nsdd 1 -neyy ${neyy} -nemm 12 -nedd 31 -i ${ifile} -o tmp01.gt > /dev/null)
ESTAT=$?
echo status = ${ESTAT}
[ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
#
#
# (4) set replace data ==> tmp03.gt
# -r: input data file for replace
nrec=`gtshow -item ${rfile} |wc -l`
if [ ${nrec} -gt 1 ]; then
  echo "${gtcut} -nsyy ${nsyy} -nsmm 1 -nsdd 1 -neyy ${neyy} -nemm 12 -nedd 31 -i ${rfile} -o tmp02.gt"
  (${gtcut} -nsyy ${nsyy} -nsmm 1 -nsdd 1 -neyy ${neyy} -nemm 12 -nedd 31 -i ${rfile} -o tmp02.gt > /dev/null)
  ESTAT=$?
  [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
else
  # for constant data
  cp ${rfile} tmp02.gt
fi
#
echo "gtext tmp02.gt ax:${haxisx} xstr=1 xend=${imax} ay:${haxisy} ystr=1 yend=${jmax} az:${haxisz} zstr=1 zend=${kmax} out:tmp03.gt"
(gtext tmp02.gt ax:${haxisx} xstr=1 xend=${imax} ay:${haxisy} ystr=1 yend=${jmax} az:${haxisz} zstr=1 zend=${kmax} out:tmp03.gt > /dev/null)
ESTAT=$?
[ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
#
#
# (5) interpolation
if [ ${nrec} -gt 1 ]; then
  echo "${gtrepint} -i tmp01.gt -r tmp03.gt -o ${ofile} -intext t"
  (${gtrepint} -i tmp01.gt -r tmp03.gt -o ${ofile} -intext t)
  ESTAT=$?
else
  # for constant data
  echo "${gtreplace} -i tmp01.gt -r tmp03.gt -o ${ofile}"
  (${gtreplace} -i tmp01.gt -r tmp03.gt -o ${ofile})
  ESTAT=$?
fi
#
exit_shell ${ESTAT} 1>&2
#
exit 0
