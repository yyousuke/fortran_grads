#!/bin/sh
#
for tmpfiles in  a.out tmp01 fort.* *.o *.dat *.bin *.grads
#for tmpfiles in  a.out tmp01 fort.* *.o *.dat 
do
  test -e ${tmpfiles} && rm -f ${tmpfiles}
done
exit 0
