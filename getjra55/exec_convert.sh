#! /bin/sh
#
#  exec_convert.sh
#  [history]
#  2017/02/08 Yamashita: first ver. (grd6hr2day)
#  2017/02/09 Yamashita: grd6hr2mon
#
#  add to .bashrc
#  export DATADIR_JRA55="path to JRA-55 dir."
#
#  Defaults:
#   DATADIR_JRA55="JRA-55データを置いたディレクトリの絶対パス"
#   idir="${DATADIR_JRA55}/grads/6hr"
#   odir_day="${DATADIR_JRA55}/grads/daily"
#   odir_mon="${DATADIR_JRA55}/grads/monthly"
########################################################################
#
. /etc/bashrc 
#
cur=`pwd`
log="log"
bin_6hr2day="grd6hr2day"
prog_6hr2day="grd6hr2day.f90"
libsrcs="libsrc/tave.f90 libsrc/calendar.f90 libsrc/rw.f90 libsrc/io.f90"
libconv="libconv.a"
bin_6hr2mon="grd6hr2mon"
prog_6hr2mon="grd6hr2mon.f90"
FC="gfortran" # compiler
OPT="-O -fconvert=big-endian -frecord-marker=4" # options
#
# default values
[ -z "${DATADIR_JRA55:-}" ] && DATADIR_JRA55="path_to_JRA-55"
idir="${DATADIR_JRA55}/grads/6hr"
odir_day="${DATADIR_JRA55}/grads/daily"
odir_mon="${DATADIR_JRA55}/grads/monthly"
dayout="f"
monout="f"
vars=''
nsyy='1958'
nsmm='1'
neyy='2021'
nemm='12'
rmiss='-999.0'
nrecparday=4
nx=288
ny=145
nz=37
suf='.grads'
rmexist="f"
#

ESTAT=1
exit_shell()
{
   for fname in *.o *.mod ; do
     [ -e "${fname}" ] && rm ${fname}
   done
   [ -e "${libconv}" ] && rm ${libconv}
   [ -e "${bin_6hr2day}" ] && rm ${bin_6hr2day}
   [ -e "${bin_6hr2mon}" ] && rm ${bin_6hr2mon}
   exit $1
}

#
show_usage()
{
   echo "Usege: `basename $0`"
   echo "Options:"
   echo "--out-day=true|-d [conversion from 6hr to daily, default: false]"
   echo "--out-mon=true|-m [conversion from 6hr to monthly, default: false]"
   echo "--indir|-i [prefix of input directory]"
   echo "--outdir|-o [prefix of output directory]"
   echo "--var|-v [input variable(s)]"
   echo "--nx/-x [x-size, default: 288]"
   echo "--ny|-y [y-size, default: 145]"
   echo "--nz/-z [z-size, default: 37]"
   echo "--nsyy/-ys [start year] --nsmm|-ms [end month]"
   echo "--neyy/-ye [end year] --nemm|-me [end month]"
   echo "--recday|-rd [records per day, default: 4]"
   echo "--rmiss|-r [missing value, default: -999.0]"
   echo "-f : remove exist files in output directory, default: NONE"
   echo "--help|-h (show this help)"
   echo ""
   echo "-v option is required"
   echo "-d or -m option is required"
   echo "-f option is required for replacing"
   echo "-z 1 option is required for surface data"
   exit_shell ${ESTAT} 1>&2
}

#
make_6hr2day()
{
  #
  for src in ${libsrcs} ${prog_6hr2day}; do
    echo "${FC} ${OPT} -c ${src}"
    ${FC} ${OPT} -c ${src}
    path=`echo ${src} | sed -e 's/f90/o/'`
    obj=`basename ${path}`
    ar cru ${libconv} ${obj}
  done
  echo "${FC} -o ${bin_6hr2day} ${libconv}"
  ${FC} -o ${bin_6hr2day} ${libconv}
  ESTAT=$?
  [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
}
#
#
make_6hr2mon()
{
  #
  for src in ${libsrcs} ${prog_6hr2mon}; do
    echo "${FC} ${OPT} -c ${src}"
    ${FC} ${OPT} -c ${src}
    path=`echo ${src} | sed -e 's/f90/o/'`
    obj=`basename ${path}`
    ar cru ${libconv} ${obj}
  done
  echo "${FC} -o ${bin_6hr2mon} ${libconv}"
  ${FC} ${OPT} -o ${bin_6hr2mon} ${libconv}
  ESTAT=$?
  [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
}


#
sub_6hr2day()
{
#
echo "${cur}/${bin_6hr2day} -od ${outdir} -ov ${var} -os ${suf} -id ${indir} -iv ${var} -is ${suf} -nx ${nx} -ny ${ny} -nz ${nz} -nr ${nrecparday} -rmiss ${rmiss} -nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm}"
#
(${cur}/${bin_6hr2day} -od "${outdir}" -ov "${var}" -os "${suf}" \
-id "${indir}" -iv "${var}" -is "${suf}" \
-nx ${nx} -ny ${ny} -nz ${nz} -nr ${nrecparday} -rmiss ${rmiss} \
-nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm} ) >> ${log} 2>&1
ESTAT=$?
[ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
#
}

#
sub_6hr2mon()
{
#
echo "${cur}/${bin_6hr2mon} -od ${outdir} -ov ${var}.mon.mean -os ${suf} -id ${indir} -iv ${var} -is ${suf} -nx ${nx} -ny ${ny} -nz ${nz} -nr ${nrecparday} -rmiss ${rmiss} -nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm}"
#
(${cur}/${bin_6hr2mon} -od "${outdir}" -ov "${var}.mon.mean" -os "${suf}" \
-id "${indir}" -iv "${var}" -is "${suf}" \
-nx ${nx} -ny ${ny} -nz ${nz} -nr ${nrecparday} -rmiss ${rmiss} \
-nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm} ) >> ${log} 2>&1
ESTAT=$?
[ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
#
}

#
#
########################################################################
#
#
while test $# -gt 0; do
  case $1 in
    --indir|-i)
      idir=$2
      shift
      ;; 
    --outdir|-o)
      odir_day="$2/daily"
      odir_mon="$2/monthly"
      shift
      ;; 
    --var|-v)
      vars=`echo "${vars} $2"`
      shift
      ;; 
    --out-day=true|-d)
      dayout="t"
      ;; 
    --out-mon=true|-m)
      monout="t"
      ;; 
    --out-day=false)
      dayout="f"
      ;; 
    --out-mon=false)
      monout="f"
      ;; 
    --rmiss|-r)
      rmiss=$2
      shift
      ;; 
    --recday|-rd)
      nrecparday=$2
      shift
      ;; 
    --nsyy|-ys)
      nsyy=$2
      shift
      ;; 
    --nsmm|-ms)
      nsmm=$2
      shift
      ;; 
    --neyy|-ye)
      neyy=$2
      shift
      ;; 
    --nemm|-me)
      nemm=$2
      shift
      ;; 
    --nx|-x)
      nx=$2
      shift
      ;;
    --ny|-y)
      ny=$2
      shift
      ;;
    --nz|-z)
      nz=$2
      shift
      ;;
    -f)
      rmexist="t"
      ;; 
    --help|-h)
      ESTAT=0
      show_usage $0 1>&2
      ;; 
    *)
      show_usage $0 1>&2
      ;;
  esac
  shift
done
#

[ "${dayout}" = "f" -a "${monout}" = "f" ] && exit_shell 1 1>&2
[ "${dayout}" = "t" ] && make_6hr2day
[ "${monout}" = "t" ] && make_6hr2mon
#
V=(`echo "${vars}"`)
cp /dev/null ${log}
for ((j=0; j < ${#V[@]}; j++)){
  var=$(  echo ${V[$j]} ) || exit_shell 1 1>&2
  ovar=${var}
  echo "var: ${var}"
  echo "ovar: ${ovar}"
  #
  YYYY="${nsyy}"
  YYMM_STR=$(  echo ${nsyy} ${nsmm} | awk '{printf "%4.4d%2.2d", $1,$2}' ) || exit_shell 1 1>&2
  YYMM_END=$(  echo ${neyy} ${nemm} | awk '{printf "%4.4d%2.2d", $1,$2}' ) || exit_shell 1 1>&2
  YYMM="${YYMM_STR}"
  echo "YYMM_STR: ${YYMM_STR}"
  echo "YYMM_END: ${YYMM_END}"
  # check
  while [ ${YYYY} -le ${neyy} ] ; do
    # infile
    indir="${idir}/${var}"
    INFILE="${indir}/${var}.${YYYY}${suf}"
    echo "INFILE: ${INFILE}"
    [ -e ${INFILE} ] || exit_shell 1 1>&2
    # outfile (daily)
    if [ "${dayout}" = "t" ]; then
      outdir="${odir_day}/${var}"
      OUTFILE="${outdir}/${var}.${YYYY}${suf}"
      echo "OUTFILE (daily): ${OUTFILE}"
      [ "${rmexist}" = "f" -a -e ${OUTFILE} ] && exit_shell 1 1>&2
      [ -d "${outdir}" ] || mkdir -p ${outdir}
    fi
    YYYY=$( expr ${YYYY} + 1 ) || exit_shell 1 1>&2
  done # YYYY
  # 6hr ==> daily
  [ "${dayout}" = "t" ] && sub_6hr2day
  #
  # check
  # outfile (monthly)
  if [ "${monout}" = "t" ]; then
    outdir="${odir_mon}/${var}"
    OUTFILE="${outdir}/${var}.mon.mean${suf}"
    echo "OUTFILE (monthly): ${OUTFILE}"
    [ "${rmexist}" = "f" -a -e ${OUTFILE} ] && exit_shell 1 1>&2
    [ -d "${outdir}" ] || mkdir -p ${outdir}
  fi
  # 6hr ==> monthly
  [ "${monout}" = "t" ] && sub_6hr2mon
  #
} # var
#

#
exit_shell 0 1>&2
