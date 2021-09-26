#!/bin/sh
#
if [ -e filedata.dat ]; then
  gfile=`awk '{print $2}' filedata.dat | head -1`
  tfile=`awk '{print $3}' filedata.dat | head -1`
  info=`awk '{print $3}' filedata.dat | head -9|tail -1`
  cfile=`awk '{print $2}' filedata.dat | head -2| tail -1`
  dfile=`awk '{print $2}' filedata.dat | head -3| tail -1`
  data=`awk '{print $2}' filedata.dat | head -4| tail -1`
  cov=`awk '{print $2}' filedata.dat | head -6| tail -1`
else
  gfile= ; tfile= ; info= ; cfile= ; dfile= ; data= ; cov=
fi

for tmpfiles in a.out tmp01 fort.* *.o runtime.dat ${data} ${cov}
do
  test -e ${tmpfiles} && rm -f ${tmpfiles}
done
#(cd 11regress; ./clear_tmp.sh ) || exit 1
#(cd 12regress_auto; ./clear_tmp.sh ) || exit 1
#(cd 12regress_auto2; ./clear_tmp.sh ) || exit 1

if [ "$1" == "all" ]; then
  for tmpfiles in  ${gfile} ${tfile} ${info} *.dat *.bin
  do
    test -e ${tmpfiles} && rm -f ${tmpfiles}
  done
  (cd 00readgrads; ./clear_tmp.sh ) || exit 1
fi

exit 0
