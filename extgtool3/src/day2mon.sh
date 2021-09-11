#!/bin/bash
#
#  day2mon.sh
#  [history]
#  2013/03/28 Yamashita: first ver.
#
#
show_usage()
{
  echo "Usage: $1 -i input_file -o output_file"
  exit 1
}
#
[ x$1 == "x" ] && show_usage $0 1>&2
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
  echo "Usage: $0 -i ifile -o ofile"
  show_usage $0 1>&2
fi
#
year=`gtshow -item tstr=1 tend=1 ${ifile} | awk '{year=substr($4,2,4); print year}'`
mm=`gtshow -item tstr=1 tend=1 ${ifile} | awk '{mm=substr($4,6,2); print mm}'`
dd=`gtshow -item tstr=1 tend=1 ${ifile} | awk '{dd=substr($4,8,2); print dd}'`
echo "start date: ${year}/${mm}/${dd}"
if [ ${mm} -ne 1 -o ${dd} -ne 1 ]; then
  echo "${ifile} must be started at 1 Jan."
  exit 1
fi
#
#
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
cp /dev/null ${ofile}
sday=1
month=1
while test ${month} -le 12
do
  dadd=`echo ${mon}| cut -d':' -f${month}`

  eday=`expr ${sday} + ${dadd} - 1`
  echo "gtavr ${ifile} str=${sday} end=${eday} out:${ofile} apnd=t"
  (gtavr ${ifile} str=${sday} end=${eday} out:${ofile} apnd=t > /dev/null) || exit 1
  sday=`expr ${sday} + ${dadd}`
  month=`expr ${month} + 1`
done # month
#
exit 0
