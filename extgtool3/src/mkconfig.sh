#!/bin/bash
#
#  mkconfig.sh
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
  show_usage $0 1>&2
fi
#
#
#
########################################################################
### write config.sh
cp /dev/null ${ofile}
echo "#!/bin/sh" >> ${ofile}
echo "#" >> ${ofile}
#
# write initial options
cnt=1
cmax=`cat ${ifile} | wc -l`
while [ ${cnt} -le ${cmax} ]; do
  opt=`cat ${ifile} | head -${cnt}|tail -1| cut -d':' -f1`
  val=`cat ${ifile} | head -${cnt}|tail -1| cut -d':' -f2`
  echo "${opt}=${val}" >> ${ofile}
  cnt=`expr ${cnt} + 1`
done
cat >> ${ofile}<<EOF
#
show_usage()
{
  echo "Usage: \$1 "
EOF
# write error options
cnt=1
cmax=`cat ${ifile} | wc -l`
while [ ${cnt} -le ${cmax} ]; do
  opt=`cat ${ifile} | head -${cnt}|tail -1| cut -d':' -f1`
  val=`cat ${ifile} | head -${cnt}|tail -1| cut -d':' -f2`
  echo "  echo        --${opt}" >> ${ofile}
  cnt=`expr ${cnt} + 1`
done
#
cat >> ${ofile}<<EOF
  exit 1
}
#
[ x\$1 == "x" ] && show_usage \$0 1>&2
#
#
while test \$# -gt 0; do
  case \$1 in
EOF
#
# write main
cnt=1
cmax=`cat ${ifile} | wc -l`
while [ ${cnt} -le ${cmax} ]; do
  opt=`cat ${ifile} | head -${cnt}|tail -1| cut -d':' -f1`
  val=`cat ${ifile} | head -${cnt}|tail -1| cut -d':' -f2`
  echo "    --${opt})" >> ${ofile}
  echo "      echo \$${opt}" >> ${ofile}
  echo "      ;; " >> ${ofile}
  cnt=`expr ${cnt} + 1`
done
#
cat >> ${ofile}<<EOF
    *)
      show_usage \$0 1>&2
      ;;
  esac
  shift
done
#
exit 0
EOF
chmod +x ${ofile}
#
exit 0
