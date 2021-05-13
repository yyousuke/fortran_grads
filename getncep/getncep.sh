#!/bin/sh
machine=ftp.cdc.noaa.gov

case "$1" in
pressure) data=pressure
       ;;
surface) data=surface
       ;;
gauss) data=surface_gauss
       ;;
other) data=other_gauss
       ;;
trop) data=tropopause
       ;;
spec) data=spectral
       ;;
--help|-h) cat ncep.doc
           exit 0
       ;;
*)
   echo  "Usage $0 {surface|pressure|gauss|other|tropopause|spec} {mon|day|6hr} filename"
   echo "or $0 --help, you can see the detail of this command."
   exit 1
esac

case "$2" in
mon) tstep=ncep.reanalysis.derived
       ;;
day) tstep=ncep.reanalysis.dailyavgs
       ;;
6hr) tstep=ncep.reanalysis
       ;;
*)
   echo  "Usage $0 {surface|pressure|gauss|other|tropopause|spec} {mon|day|6hr} filename"
   echo "or $0 --help, you can see the detail of this command."
   exit 1
esac

if [ x"$3" = "x" ] ; then
   echo  "Usage $0 {surface|pressure|gauss|other|tropopause|spec} {mon|day|6hr} filename"
   echo "or $0 --help, you can see the detail of this command."
   exit 1
fi

datapath=Datasets/${tstep}/${data}  # 4times_daily
echo ${datapath}
echo "machine ${machine} login anonymous password username@host.domain " > ${HOME}/.netrc 
chmod 700 ${HOME}/.netrc
ftp ${machine} <<EOF
bin
cd $datapath
get $3
EOF
rm -f ${HOME}/.netrc

exit 0
