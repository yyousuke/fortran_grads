#! /bin/bash
#
#  2017/02/03 Yamashita: first ver. for JRA-55
#  2024/02/13 Yamashita: for JRA-3Q
#
#  add to .bashrc
#  export WGRIB2="path to wgrib ver. 2"
#  export DATADIR_JRA3Q="path to JRA-3Q dir."
#
#  Defaults:
#   DATE="gdate"
#   WGRIB2="wgrib2"
#   DATADIR_JRA3Q="path to JRA-3Q dir."
#   idir="${DATADIR_JRA3Q}/grib/Hist/Daily/"
#   odir="${DATADIR_JRA3Q}/grads/6hr"
########################################################################
cur=`pwd`
prog_grid="${cur}/gribid.sh"
log="log"
#
# default values
[ -z "${DATE:-}" ] && DATE="gdate"
[ -z "${WGRIB2:-}" ] && WGRIB2="wgrib2"
[ -z "${DATADIR_JRA3Q:-}" ] && DATADIR_JRA3Q="path_to_JRA-3Q_dir"
idir="${DATADIR_JRA3Q}/grib/Hist/Daily/"
odir="${DATADIR_JRA3Q}/grads/6hr"
vars=''
nsyy='1948'
nsmm='1'
neyy='2023'
nemm='12'
suf='.grads'
rmexist="f"
basenamei="auto"
prefixi="auto"
gridi="auto"
#

ESTAT=1
exit_shell()
{
   exit $1
}

#
show_usage()
{
   echo "Usege: `basename $0`"
   echo "Options:"
   echo "--indir|-i [prefix of input directory]"
   echo "--outdir|-o [prefix of output directory]"
   echo "--var|-v [input variable(s)]"
   echo "--nsyy/-ys [start year] --nsmm|-ms [start month]"
   echo "--neyy/-ye [end year] --nemm|-me [end month]"
   echo "--basename|-b [input_file_basename or auto]"
   echo "--prefix|-p [input_dir_prefix or auto]"
   echo "--grid|-g [grib-id or auto]"
   echo "-f : remove exist files in output directory, default: NONE"
   echo "--help|-h (show this help)"
   echo ""
   echo "-v option is required"
   echo "-f option is required for replacing"
   exit_shell ${ESTAT} 1>&2
}

#
sub_conv()
{
#
I=`echo "${input_path}/${basename}.??????????"`
for INFILE in ${I} ; do
   echo "INFILE: ${INFILE}, OUTFILE: ${OUTFILE}" >> ${log}
   (${WGRIB2} ${INFILE} | grep ":${title}," | sort -nr -k5 -t':' \
   | ${WGRIB2} ${INFILE} -i -no_header -append -order we:ns -ieee \
   ${OUTFILE} ) >> ${log} 2>&1
   ESTAT=$?
   [ ${ESTAT} -ne 0 ] && exit_shell ${ESTAT} 1>&2
done
#
}

#
#
########################################################################
#
#
while test $# -gt 0; do
  case $1 in
    --basename|-b)
      basenamei=$2
      shift
      ;; 
    --prefix|-p)
      prefixi=$2
      shift
      ;; 
    --grid|-g)
      gridi=$2
      shift
      ;; 
    --indir|-i)
      idir=$2
      shift
      ;; 
    --outdir|-o)
      odir=$2
      shift
      ;; 
    --var|-v)
      vars=`echo "${vars} $2"`
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
#
V=(`echo "${vars}"`)
cp /dev/null ${log}
for ((j=0; j < ${#V[@]}; j++)){
  var=$(  echo ${V[$j]} ) || exit_shell 1 1>&2
  ovar=${var}
  echo "var: ${var}"
  echo "ovar: ${ovar}"
  basename=${basenamei}
  prefix=${prefixi}
  grid=${gridi}
  [ "${basename}" = "auto" ] && basename=`${prog_grid} -b ${var}`
  [ "${prefix}" = "auto" ] && prefix=`${prog_grid} -p ${var}`
  [ "${title}" = "auto" ] && grid=`${prog_grid} -t ${var}`
  echo "basename: ${basename}"
  echo "prefix: ${prefix}"
  echo "title: ${title}"
  #
  ofirst="t"
  YYYY="${nsyy}"
  YYMM_STR=$(  echo ${nsyy} ${nsmm} | awk '{printf "%4.4d%2.2d", $1,$2}' ) || exit_shell 1 1>&2
  YYMM_END=$(  echo ${neyy} ${nemm} | awk '{printf "%4.4d%2.2d", $1,$2}' ) || exit_shell 1 1>&2
  YYMM="${YYMM_STR}"
  echo "YYMM_STR: ${YYMM_STR}"
  echo "YYMM_END: ${YYMM_END}"
  while [ ${YYMM} -le ${YYMM_END} ] ; do
    YYYY_PREV=${YYYY}
    YYYY=$(  echo ${YYMM} | cut -b 1-4 ) || exit_shell 1 1>&2
    MM=$(  echo ${YYMM} | cut -b 5-6 ) || exit_shell 1 1>&2
    # every year
    [ ${YYYY} -eq ${YYYY_PREV} ] || ofirst="t"
    #
    input_path=${idir}/${prefix}/${YYMM}
    OUTFILE=${odir}/${ovar}/${ovar}.${YYYY}${suf}
    [ -d "${odir}/${ovar}" ] || mkdir -p ${odir}/${ovar}
    if [ "${ofirst}" = "t" ]; then
      [ "${rmexist}" = "f" -a -e ${OUTFILE} ] && exit_shell 1 1>&2
      ofirst="f"
      cp /dev/null ${OUTFILE}
    fi
    #
    sub_conv
    #
    YYMM=$( ${DATE} --date "${YYMM}01 1 month" +%Y%m ) || exit_shell 1 1>&2
  done # date
  #
} # var
#
#
exit_shell 0 1>&2
