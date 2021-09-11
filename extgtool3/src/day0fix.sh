#!/bin/bash
#
#  day0fix.sh 
#  [history]
#  2016/02/04 Yamashita: first ver.
#
#  INPUT:
#    file1: 0001/01/01, 0001/02/01, ...
#    file2: 0001/01/11, 0001/02/11, ...
#    file3: 0001/01/21, 0001/02/21, ...
#  OUTPUT:
#    file: 0001/01/01, 0001/01/11, 0001/01/21, 0001/02/01, ...
# 
#
show_usage()
{
  echo "Usage: $1 -o output_file  input_file[1] input_file[2]..."
  echo "         (-m maximum limit of record number)"
  exit 1
}
#
exit_shell()
{
  [ -e gtool.tmp ] && \rm -f gtool.tmp
  exit $1
}
#
[ x$1 == "x" ] && show_usage $0 1>&2
#
bindir=`config.sh --bindir`
#
ifiles=
nrec=9999
cnt=1
cmax=$#
while [ ${cnt} -le ${cmax} ]; do
  case $1 in
    -o)
      ofile=`echo $2`
      shift
      ;;
    -m)
      nrec=`echo $2`
      shift
      ;;
    -*)
      show_usage $0 1>&2
      ;;
    *)
      ifiles=`echo ${ifiles} $1`
      ;;
  esac
  cnt=`expr ${cnt} + 1`
  shift
done
#
if [ "x${ifiles}" == "x" ]; then
  show_usage $0 1>&2
fi
if [ "x${ofile}" == "x" ]; then
  show_usage $0 1>&2
elif [ -e "${ofile}" ]; then
  echo "Error: ${ofile} is exist"
  show_usage $0 1>&2
fi
if [ "${nrec}" -le 0 ]; then
  echo "Error: -m option"
  show_usage $0 1>&2
fi
#
#
for ifile in ${ifiles}
do
  nreci=`gtshow -item ${ifile} | wc -l`
  if [ ${nreci} -le ${nrec} ]; then
    nrec=${nreci}
  fi
done
#
cp /dev/null gtool.tmp
it=1
while test ${it} -le ${nrec}
do
  for ifile in ${ifiles}
  do
    (gtsel ${ifile} str=${it} end=${it} out:gtool.tmp apnd=t > /dev/null)
    ESTAT=$?
    [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
  done
  #
  it=`expr ${it} + 1`
done # year
#
\mv -f gtool.tmp ${ofile}
ESTAT=$?
exit_shell ${ESTAT} 1>&2
