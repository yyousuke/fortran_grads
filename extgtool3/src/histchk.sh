#!/bin/bash
#
#  histchk.sh
#  [history]
#  2013/03/06 Yamashita: first ver.
#  2013/03/28 Yamashita: use compiled program histchk
#  2013/03/28 Yamashita: add options & error code
#  check output file names in nmhisd of namelist input
#
#
bindir=`config.sh --bindir`
prog=${bindir}/histchk
#
show_usage()
{
  echo "Usage: $1 -i input_directory -o output_file"
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
      idir=`echo $2`
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
echo "idir = ${idir}"
echo "ofile = ${ofile}"
cur=`pwd`
#
if [ -e ${idir}/SYSIN ]; then
  [ "${idir}" != "${cur}" ] && cp ${idir}/SYSIN .
  stat=0
  (${prog}) || stat=1
  if [ ${stat} -eq 1 -o ! -e items.txt ]; then
    echo "Error: histchk"
    exit 1
  fi
else
  echo "Error: not found ${idir}/SYSIN"
  exit 1
fi
#
files=`awk '{printf "%s ", $2}' items.txt`
#
cp /dev/null ${ofile}
ierr=0
for filename in ${files}
do
  ifile=${idir}/${filename}
  if [ -e ${ifile} ]; then
    echo "Found: ${ifile}"
    echo "Found: ${ifile}" >> ${ofile}
  else
    echo "Warn: Not Found: ${ifile}"
    echo "Warn: Not Found: ${ifile}" >> ${ofile}
    ierr=`expr ${ierr} + 1`
  fi
done
#
[ ${ierr} -ne 0 ] && exit 1
#
exit 0
