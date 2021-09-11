#!/bin/bash
#
#  any2mon.sh
#  [history]
#  2013/10/14 Yamashita: first ver. from day2mon.sh by Yamashita
#
#
show_usage()
{
  echo "Usage: $1 -i input_file -o output_file"
  exit 1
}
#
exit_shell()
{
  [ -e gtool.tmp ] && rm gtool.tmp
  exit $1
}
#
[ x$1 == "x" ] && show_usage $0 1>&2
#
bindir=`config.sh --bindir`
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
    -*)
      show_usage $0 1>&2
      ;;
  esac
  cnt=`expr ${cnt} + 1`
  shift
done
#
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
yy=`gtshow -item tstr=1 tend=1 ${ifile} | awk '{yy=substr($4,2,4); print yy}'`
mm=`gtshow -item tstr=1 tend=1 ${ifile} | awk '{mm=substr($4,6,2); print mm}'`
dd=`gtshow -item tstr=1 tend=1 ${ifile} | awk '{dd=substr($4,8,2); print dd}'`
echo "start date: ${yy}/${mm}/${dd}"
#
#
ofirst=0
cp /dev/null ${ofile}
year=${yy}
while test ${year} -le 9999
do
  # leap year [(year%4 == 0 && year%100 != 0) || (year%400 == 0)]
  leap1=`echo ${year} | awk '{print $1%4}'`
  leap2=`echo ${year} | awk '{print $1%100}'`
  leap3=`echo ${year} | awk '{print $1%400}'`
  if [ ${leap1} -eq 0 -a ${leap2} -ne 0 -o ${leap3} -eq 0 ]; then
    mon='31:29:31:30:31:30:31:31:30:31:30:31'
  else
    mon='31:28:31:30:31:30:31:31:30:31:30:31'
  fi
  #
  if [ ${ofirst} -eq 0 ]; then
    month=${mm}
    ofirst=1
  else
    month=1
  fi
  while test ${month} -le 12
  do
    ndd=`echo ${mon}| cut -d':' -f${month}`
    echo "year/mon = ${year}/${month}"
    #
    rm gtool.tmp
    echo "gtcut: -nsyy ${year} -nsmm ${month} -nsdd 1 -neyy ${year} -nemm ${month} -nedd ${ndd} -i ${ifile} -o gtool.tmp"
    (${bindir}/gtcut -nsyy ${year} -nsmm ${month} -nsdd 1 -neyy ${year} -nemm ${month} -nedd ${ndd} -i ${ifile} -o gtool.tmp > /dev/null)
    ESTAT=$?
    [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
    flag=`gtshow -item gtool.tmp`
    [ "x${flag}" == "x" ] && exit_shell ${ESTAT} 1>&2
    #
    echo "gtavr gtool.tmp out:${ofile} apnd=t"
    (gtavr gtool.tmp out:${ofile} apnd=t > /dev/null)
    ESTAT=$?
    [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
    #
    month=`expr ${month} + 1`
  done # month
  year=`expr ${year} + 1`
done # year
#
exit_shell 0 1>&2
