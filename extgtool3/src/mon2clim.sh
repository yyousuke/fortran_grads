#!/bin/sh
#
#  mon2clim.sh
#  [history]
#  2008/03/04 Yamashita: first ver.
#  2014/12/09 Yamashita: add functions & use gtmmon
#
bindir=`config.sh --bindir`
gtmmon=${bindir}/gtmmon
gtavr='gtavr'
#
exit_shell()
{
   [ -e gttmp.gt ] && \rm gttmp.gt
   exit $1
}
#
show_usage()
{
   echo "Usage: $(basename $1) -i input_file -o output_file (-t year -f)"
   echo "-t: set year, -f: overwrite"
   exit_shell 1 1>&2
}
#
[ x$1 == "x" ] && show_usage $0 1>&2
#
ifile='gtool.in'
ofile='gtool.out'
year='2000'
owrite='f'
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
    -t)
      year=`echo $2`
      shift
      ;;
    -f)
      owrite='t'
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
if [ ! -e "${ifile}" ]; then
  echo "Error: Not found ${ifile}"
  show_usage $0 1>&2
elif [ "${ifile}" == "${ofile}" ]; then
  echo "Error: input file name is the same as output file name"
  show_usage $0 1>&2
elif [ "x${ofile}" == "x" ]; then
  show_usage $0 1>&2
elif [ -e "${ofile}" -a "${owrite}" == "f" ]; then
  echo "Error: output file is exist"
  show_usage $0 1>&2
fi
#
#
# average
cp /dev/null ${ofile}
t=1
while [ ${t} -le 12 ]; do
  (${gtavr} str=${t} step=12 ${ifile} out:${ofile} apnd=t) || exit_shell 1
  t=`expr ${t} + 1`
done
#
# modify time header
mv ${ofile} gttmp.gt
(${gtmmon} -i gttmp.gt -o ${ofile} -nsyy ${year} -nsmm 1) || exit_shell 1
#
#
exit_shell ${ESTAT} 1>&2
