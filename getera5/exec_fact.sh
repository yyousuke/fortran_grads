#! /bin/sh
#
#  exec_fact.sh_
#  [history]
#  2017/04/20 Yamashita: first ver. 
#
#   DATADIR_ERA5="path_to_ERA-5_dir"
########################################################################
[ -z "${DATADIR_ERA5:-}" ] && DATADIR_ERA5="path_to_ERA-5"
#
cur=`pwd`
log="log"
bin_fact="grdfact"
prog_fact="grdfact.f90"
FC="f90_lib"
OPT=""
#
# default values
odir_1hr="${DATADIR_ERA5}/grads/1hr"
odir_day="${DATADIR_ERA5}/grads/daily"
odir_mon="${DATADIR_ERA5}/grads/monthly"
hr6out="f"
dayout="f"
monout="f"
vars=''
nsyy='1979'
nsmm='1'
neyy='2020'
nemm='12'
rmiss='-999.0'
factor='auto'
offset='auto'
nrecparday=4
nx=1440
ny=721
nz=37
suf='.grads'
rmexist="f"
#

ESTAT=1
exit_shell()
{
   [ -e "${bin_fact}" ] && rm ${bin_fact}
   exit $1
}

#
show_usage()
{
   echo "Usege: `basename $0`"
   echo "Options:"
   echo "--out-1hr=true|-1h [1hr, default: false]"
   echo "--out-day=true|-d [daily, default: false]"
   echo "--out-mon=true|-m [monthly, default: false]"
   echo "--outdir|-o [prefix of output directory]"
   echo "--var|-v [input variable(s)]"
   echo "--fact [factor, default: auto]"
   echo "--offset [offset, default: auto]"
   echo "--nx/-x [x-size] --ny|-y [y-size] --nz/-z [z-size]"
   echo "--nsyy/-ys [start year] --nsmm|-ms [end month]"
   echo "--neyy/-ye [end year] --nemm|-me [end month]"
   echo "--recday|-rd [records per day, default: 4]"
   echo "--rmiss|-r [missing value, default: -999.0]"
   echo "-f : remove exist files in output directory, default: NONE"
   echo "--help|-h (show this help)"
   echo ""
   echo "-v option is required"
   echo "-1h or -d or -m option is required"
   echo "-f option is required for replacing"
   echo "-z 1 option is required for surface data"
   exit_shell ${ESTAT} 1>&2
}

#
make_fact()
{
  #
  ${FC} ${OPT} -o ${bin_fact} ${prog_fact}
  ESTAT=$?
  [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
}
#
#

#
sub_hour()
{
#
echo "${cur}/${bin_fact} -od ${outdir} -ov ${ovar} -os ${suf} -id ${indir} -iv ${var} -is ${suf} -nx ${nx} -ny ${ny} -nz ${nz} -nr ${nrecparday} -rmiss ${rmiss} -fact ${factor} -offset ${offset} -onefile f -omonth f -nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm}"
#
(${cur}/${bin_fact} -od "${outdir}" -ov "${ovar}" -os "${suf}" \
-id "${indir}" -iv "${var}" -is "${suf}" \
-nx ${nx} -ny ${ny} -nz ${nz} -nr ${nrecparday} -rmiss ${rmiss} \
-fact "${factor}" -offset "${offset}" \
-onefile f -omonth f \
-nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm} ) >> ${log} 2>&1
ESTAT=$?
[ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
#
}

#
sub_day()
{
echo "${cur}/${bin_fact} -od ${outdir} -ov ${ovar} -os ${suf} -id ${indir} -iv ${var} -is ${suf} -nx ${nx} -ny ${ny} -nz ${nz} -nr 1 -rmiss ${rmiss} -fact ${factor} -offset ${offset} -onefile f -omonth f -nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm}"
#
(${cur}/${bin_fact} -od "${outdir}" -ov "${ovar}" -os "${suf}" \
-id "${indir}" -iv "${var}" -is "${suf}" \
-nx ${nx} -ny ${ny} -nz ${nz} -nr 1 -rmiss ${rmiss} \
-fact "${factor}" -offset "${offset}" \
-onefile f -omonth f \
-nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm} ) >> ${log} 2>&1
ESTAT=$?
[ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
#
}

#
sub_mon()
{
echo "${cur}/${bin_fact} -od ${outdir} -ov ${ovar} -os ${suf} -id ${indir} -iv ${var} -is ${suf} -nx ${nx} -ny ${ny} -nz ${nz} -rmiss ${rmiss} -fact ${factor} -offset ${offset} -onefile t -omonth t -nsyy ${nsyy} -nsmm ${nsmm} -neyy ${neyy} -nemm ${nemm}"
#
(${cur}/${bin_fact} -od "${outdir}" -ov "${ovar}" -os "${suf}" \
-id "${indir}" -iv "${var}" -is "${suf}" \
-nx ${nx} -ny ${ny} -nz ${nz} -rmiss ${rmiss} \
-fact "${factor}" -offset "${offset}" \
-onefile t -omonth t \
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
    --outdir|-o)
      odir_1hr="$2/1hr"
      odir_day="$2/daily"
      odir_mon="$2/monthly"
      shift
      ;; 
    --var|-v)
      vars=`echo "${vars} $2"`
      shift
      ;; 
    --out-1hr=true|-1h)
      hr6out="t"
      ;; 
    --out-day=true|-d)
      dayout="t"
      ;; 
    --out-mon=true|-m)
      monout="t"
      ;; 
    --out-1hr=false)
      hr6out="f"
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
    --factor)
      factor=$2
      shift
      ;; 
    --offset)
      offset=$2
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

[ "${hr6out}" = "f" -a "${dayout}" = "f" -a "${monout}" = "f" ] && exit_shell 1 1>&2

make_fact
#
V=(`echo "${vars}"`)
cp /dev/null ${log}
for ((j=0; j < ${#V[@]}; j++)){
  var=$(  echo ${V[$j]} ) || exit_shell 1 1>&2
  if [ "${var}" = "tco3" ]; then
    ovar='dobson'
    if [ "${factor}" = "auto" ]; then
      factor=$(  echo "6.0221415e23 4.8e-2 2.6868e20" | awk '{print $1/$2/$3}' ) || exit_shell 1 1>&2
    fi 
  elif [ "${var}" = "geo" ]; then
    ovar='hgt'
    if [ "${factor}" = "auto" ]; then
      factor=$(  echo "1.0 9.80665" | awk '{print $1/$2}' ) || exit_shell 1 1>&2
    fi
  else
    ovar=${var}
    if [ "${factor}" = "auto" ]; then
      factor='1.d0'
    fi
  fi
  if [ "${offset}" = "auto" ]; then
    offset='0.d0'
  fi
  echo "var: ${var}"
  echo "ovar: ${ovar}"
  echo "factor: ${factor}"
  echo "offset: ${offset}"
  #
  YYYY="${nsyy}"
  YYMM_STR=$(  echo ${nsyy} ${nsmm} | awk '{printf "%4.4d%2.2d", $1,$2}' ) || exit_shell 1 1>&2
  YYMM_END=$(  echo ${neyy} ${nemm} | awk '{printf "%4.4d%2.2d", $1,$2}' ) || exit_shell 1 1>&2
  YYMM="${YYMM_STR}"
  echo "YYMM_STR: ${YYMM_STR}"
  echo "YYMM_END: ${YYMM_END}"
  # check
  while [ ${YYYY} -le ${neyy} ] ; do
    if [ "${hr6out}" = "t" ]; then
      # infile (1hr)
      indir="${odir_1hr}/${var}"
      INFILE="${indir}/${var}.${YYYY}${suf}"
      echo "INFILE (1hr): ${INFILE}"
      [ -e ${INFILE} ] || exit_shell 1 1>&2
      # outfile (1hr)
      outdir="${odir_1hr}/${ovar}"
      OUTFILE="${outdir}/${ovar}.${YYYY}${suf}"
      echo "OUTFILE (1hr): ${OUTFILE}"
      [ "${rmexist}" = "f" -a -e ${OUTFILE} ] && exit_shell 1 1>&2
      [ -d "${outdir}" ] || mkdir -p ${outdir}
    fi
    #
    if [ "${dayout}" = "t" ]; then
      # infile (daily)
      indir="${odir_day}/${var}"
      INFILE="${indir}/${var}.${YYYY}${suf}"
      echo "INFILE (daily): ${INFILE}"
      [ -e ${INFILE} ] || exit_shell 1 1>&2
      # outfile (daily)
      outdir="${odir_day}/${ovar}"
      OUTFILE="${outdir}/${ovar}.${YYYY}${suf}"
      echo "OUTFILE (daily): ${OUTFILE}"
      [ "${rmexist}" = "f" -a -e ${OUTFILE} ] && exit_shell 1 1>&2
      [ -d "${outdir}" ] || mkdir -p ${outdir}
    fi
    YYYY=$( expr ${YYYY} + 1 ) || exit_shell 1 1>&2
  done # YYYY
  #
  [ "${hr6out}" = "t" ] && sub_hour
  [ "${dayout}" = "t" ] && sub_day
  #
  if [ "${monout}" = "t" ]; then
    # infile (monthly)
    indir="${odir_mon}/${var}"
    INFILE="${indir}/${var}.mon.mean${suf}"
    echo "INFILE (monthly): ${INFILE}"
    [ -e ${INFILE} ] || exit_shell 1 1>&2
    # outfile (monthly)
    outdir="${odir_mon}/${var}"
    OUTFILE="${outdir}/${var}.mon.mean${suf}"
    echo "OUTFILE (monthly): ${OUTFILE}"
    [ "${rmexist}" = "f" -a -e ${OUTFILE} ] && exit_shell 1 1>&2
    [ -d "${outdir}" ] || mkdir -p ${outdir}
    sub_mon
  fi
  #
} # var
#

#
exit_shell 0 1>&2
