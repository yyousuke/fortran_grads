!c
!c  program gtconv
!c  [history]
!c  2013/09/12 Yamashita: first ver. of mkgtool.f90
!c  2013/10/29 Yamashita: multiple time interval
!c  2013/11/27 Yamashita: gtconv.f90 from mkgtool.f90
!c  2013/11/27 Yamashita: add options
!c  2016/11/09 Yamashita: add options (grsize, xshift)
!c  2017/04/17 Yamashita: add options (exch)
!c  2017/04/28 Yamashita: modify I/O error
!c  2017/10/11 Yamashita: use common_typedef
!c  2018/07/18 Yamashita: modify getparms
!c
!c  ex. grads ==> gtool3
!c  -conv GR2GT
!c
!c  ex. UR8 ==> UR4
!c  -conv GT2GT -dfmt UR4
!c
!c  ex. modify time (2000/1/16, 2000/2/15, ...)
!c  -nsyy 2000 -nsmm 1 -nsdd 1 -tdur 1.d0 -htunit MON -cent t
!c
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module rw
!c            subroutines fopen2, fclose, fread, fwrite
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, rgt, rgt_r4, rgthd, 
!c            wgt, wgt_r4, seek_iounit
!c  external: module ucaln
!c            subroutines tnext, tcent
!c  external: module util
!c            subroutines get_axname, get_time, 
!c            shift_xgrids, conv_yrev, conv_zrev, conv_endian
!c
!c=====================================================================c
program gtconv
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use rw, only: fopen2, fclose, fread, fwrite
  use rwgtool, only: gtopen, gtclose, gtrewind, rgt, rgt_r4, rgthd, wgt, wgt_r4, seek_iounit
  use ucaln, only: tnext, tcent
  use util, only: get_axname, get_time, shift_xgrids, conv_yrev, conv_zrev, conv_endian
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ from header (gtool3 input) or getparms (grads input)
  integer(kind=i4b)           :: imax       !! x- size
  integer(kind=i4b)           :: jmax       !! y- size
  integer(kind=i4b)           :: kmax       !! z- size
  real(kind=r8b)              :: vmiss      !! missing values
  character(len=ncc)          :: haxisx     !! x-axis name
  character(len=ncc)          :: haxisy     !! y-axis name
  character(len=ncc)          :: haxisz     !! z-axis name
  !c+++ from getparms
  character(len=nfiln)        :: ifile      !! input file name
  character(len=nfiln)        :: ofile      !! output file name
  character(len=ncc)          :: hdset      !! data set
  character(len=ncc)          :: hitem      !! item
  character(len=ncc*2)        :: htitl      !! title
  character(len=ncc)          :: hunit      !! unit
  integer(kind=i4b)           :: ista       !! start record
  integer(kind=i4b)           :: iend       !! end record
  integer(kind=i4b)           :: nsyy       !! start year
  integer(kind=i4b)           :: nsmm       !! start month
  integer(kind=i4b)           :: nsdd       !! start day
  real(kind=r8b)              :: tdur       !! time_interval
  character(len=ncc)          :: htunit     !! unit of time
  character(len=ncc)          :: hdfmt      !! output data format (UR4/UR8)
  integer(kind=i4b)           :: grsize     !! input grads data size (4/8)
  integer(kind=i4b)           :: xshift     !! shift x-grids (default 0)
  logical                     :: otcent     !! f: write start time, t: write centre time
  logical                     :: omodt      !! t: enable time modification, f: disable
  logical                     :: ogtr       !! f: read grads format, t: gtool3
  logical                     :: ogtw       !! f: write grads format, t: gtool3
  logical                     :: oyrev      !! t: y-rev. on, f: off
  logical                     :: ozrev      !! t: z-rev. on, f: off
  logical                     :: oexch      !! t: endian-rev on, f: off
  logical                     :: oapnd      !! t: append, f: replace
  !c+++ [input/output]
  character(len=ncc)          :: head(ndc)  !! gtool3 header
  real(kind=r8b), allocatable :: d8(:,:,:)
  real(kind=r4b), allocatable :: d4(:,:,:)
  !c+++ [work]
  integer(kind=i4b)           :: it
  integer(kind=i4b)           :: ios        !! end code
  integer(kind=i4b)           :: jfile_i    !! I/O unit for input/output file
  integer(kind=i4b)           :: jfile_o    !! I/O unit for input/output file
  !c+++ time
  integer(kind=i4b)           :: jdate(6)   !! date/time array
  real(kind=r8b)              :: time       !! curren time
  real(kind=r8b)              :: timen      !! next time
  !c+++ current date, time
  integer(kind=i4b)           :: jyy, jmm, jdd, jhh, jmn, jss 
  !c+++ next date, time
  integer(kind=i4b)           :: jyyn, jmmn, jddn, jhhn, jmnn, jssn
  !c+++ output date, time
  integer(kind=i4b)           :: iyy, imm, idd, ihh, imn, iss

!c
!c init 
!c===
  head(1:ndc) = ' '
  !c+++ read init settings
  call getparms
  jyy = nsyy
  jmm = nsmm
  jdd = nsdd
  jhh = 0
  jmn = 0
  jss = 0
  time = 0.d0
  timen = 0.d0
  if (otcent) timen = 0.5d0 * tdur

!c
!c open
!c===
  !c+++ input
  write(6, *) 'open input file: ', trim(ifile)
  if (ogtr) then
    !c+++ open input file as gtool3 format
    call gtopen(trim(ifile), 'r', jfile_i, ios)
    if (ios /= 0) call ioerror(jfile_i, ios)
    !c+++ get x-, y-, z-axis sizes
    call rgthd(jfile_i, head, imax, jmax, kmax, vmiss, ios)
    if (ios /= 0) call ioerror(jfile_i, ios)
    call gtrewind(jfile_i, ios)
    !c+++ get x-, y-, z-axis names
    call get_axname(haxisx, haxisy, haxisz, head)
  else
    !c+++ get I/O unit numbers for input file
    call seek_iounit(jfile_i, ios)
    if (ios /= 0) call ioerror(jfile_i, ios)
    !c+++ open input file as grads format
    call fopen2(jfile_i, ifile, 'old', 0, imax * jmax * kmax * grsize, ios)
    if (ios /= 0) call ioerror(jfile_i, ios)
  endif
  !c+++ output
  write(6, *) 'open output file: ', trim(ofile)
  if (ogtw) then
    !c+++ open output file as gtool3 format
    if (oapnd) then
      call gtopen(trim(ofile), 'a', jfile_o, ios)
    else
      call gtopen(trim(ofile), 'w', jfile_o, ios)
    endif
  else
    !c+++ get I/O unit numbers for input file
    call seek_iounit(jfile_o, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)
    !c+++ open output file as grads format
    call fopen2(jfile_o, ofile, 'unknown', 0, imax * jmax * kmax * grsize, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

  allocate(d8(imax,jmax,kmax), d4(imax,jmax,kmax))
!c
!c main
!c===
  if (ista <= 0) ista = 1
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ set time
    time = timen
    timen = time + tdur

    !c+++ get next record time
    call tnext(jyy, jmm, jdd, jhh, jmn, jss, &
&       jyyn, jmmn, jddn, jhhn, jmnn, jssn, &
&       tdur, htunit)

    !c+++ calculate output time
    if (omodt) then !! enable time modification
      if (otcent) then
        call tcent(jyy, jmm, jdd, jhh, jmn, jss, &
&           jyyn, jmmn, jddn, jhhn, jmnn, jssn, &
&           iyy, imm, idd, ihh, imn, iss)
      else
        iyy = jyy
        imm = jmm
        idd = jdd
        ihh = jhh
        imn = jmn
        iss = jss
      endif
    endif

    !c+++ next record
    jyy = jyyn
    jmm = jmmn
    jdd = jddn
    jhh = jhhn
    jmn = jmnn
    jss = jssn

    !c+++ read ==> d(1:nx,1:ny,1:np)
    if (ogtr) then
      if (grsize == 4) then
        call rgt_r4(jfile_i, imax, jmax, kmax, head, d4, ios)
      else if (grsize == 8) then
        call rgt(jfile_i, imax, jmax, kmax, head, d8, ios)
      endif
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      if (it < ista) cycle
      !c+++ set dset of output data
      if (trim(hdset) == 'NULL') hdset = head(2)
      !c+++ set unit of output data
      if (trim(hunit) == 'NULL') hunit = head(16)
      !c+++ set item of output data
      if (trim(hitem) == 'NULL') hitem = head(3)
      !c+++ set title of output data
      if (trim(htitl) == 'NULL') then
        htitl(1:16) = head(14)
        htitl(17:32) = head(15)
      endif
    else
      if (it < ista) cycle
      if (grsize == 4) then
        call fread(imax, jmax, kmax, it-ista+1, jfile_i, d4)
      else if (grsize == 8) then
        call fread(imax, jmax, kmax, it-ista+1, jfile_i, d8)
      endif
    endif

    !c+++ shift x-grids
    if (xshift /= 0) then
      if (grsize == 4) then
        call shift_xgrids(imax, jmax, kmax, xshift, d4)
      else if (grsize == 8) then
        call shift_xgrids(imax, jmax, kmax, xshift, d8)
      endif
    endif
    !c+++ reverse y-grids
    if (oyrev) then
      if (grsize == 4) then
        call conv_yrev(imax, jmax, kmax, d4)
      else if (grsize == 8) then
        call conv_yrev(imax, jmax, kmax, d8)
      endif
    endif
    !c+++ reverse z-grids
    if (ozrev) then
      if (grsize == 4) then
        call conv_zrev(imax, jmax, kmax, d4)
      else if (grsize == 8) then
        call conv_zrev(imax, jmax, kmax, d8)
      endif
    endif
    !c+++ endian
    if (oexch) then
      if (grsize == 4) then
        call conv_endian(imax, jmax, kmax, d4)
      else if (grsize == 8) then
        call conv_endian(imax, jmax, kmax, d8)
      endif
    endif

    if (omodt) then !! enable time modification
      !c+++ set output time from header
      jdate(1) = iyy
      jdate(2) = imm
      jdate(3) = idd
      jdate(4) = ihh
      jdate(5) = imn
      jdate(6) = iss
    else
      !c+++ set output time from header
      call get_time(head, jdate, ios)
      if (ios /= 0) call ioerror(-1, ios)
      if (ogtr) then
        read(head(25), *) time
        read(head(28), *) tdur
        htunit = head(26)
      endif
    endif

    !c+++ write
    !c+++ d(1:imax,1:jmax,1:kmax) ==> write
    if (ogtw) then
      if (grsize == 4) then
        call wgt_r4(jfile_o, imax, jmax, kmax, vmiss, d4, hitem, htitl, hunit, &
&         hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, haxisz, ios)
      else if (grsize == 8) then
        call wgt(jfile_o, imax, jmax, kmax, vmiss, d8, hitem, htitl, hunit, &
&         hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, haxisz, ios)
      endif
      if (ios /= 0) call ioerror(jfile_i, ios)
    else
      if (grsize == 4) then
        call fwrite(imax, jmax, kmax, it-ista+1, jfile_o, d4)
      else if (grsize == 8) then
        call fwrite(imax, jmax, kmax, it-ista+1, jfile_o, d8)
      endif
    endif
    if (it == iend) exit
  enddo !! it

!c
!c close
!c===
  if (ogtr) then
    call gtclose(jfile_i, ios)
  else
    call fclose(jfile_i)
  endif
  if (ogtw) then
    call gtclose(jfile_o, ios)
  else
    call fclose(jfile_o)
  endif
  deallocate(d8, d4)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c=====
subroutine getparms
  use uopts, only: read_parms, get_parms, get_strend
  use char2var, only: c2var
  !c+++ [internal work]
  character(len=ncc)            :: hval        !!
  character(len=ncc)            :: hsta, hend  !! for conv option
  integer(kind=i4b)             :: ncol        !! for conv option
  integer(kind=i4b)             :: ios         !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('dset', 'NULL', hdset, ios) !! dataset
  call get_parms('item', 'NULL', hitem, ios) !! item
  call get_parms('titl', 'NULL', htitl, ios) !! title
  call get_parms('unit', 'NULL', hunit, ios) !! unit
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ start date
  call get_parms('nsyy', '2000', hval, ios) !! start year
  call c2var(nsyy, '(i16)', hval)
  call get_parms('nsmm', '1', hval, ios) !! start month
  call c2var(nsmm, '(i16)', hval)
  call get_parms('nsdd', '1', hval, ios) !! start day
  call c2var(nsdd, '(i16)', hval)
  !c+++ missing value
  call get_parms('vmiss', '999.d0', hval, ios)
  call c2var(vmiss, '(1pe15.5)', hval)
  !c+++ x-axis-sizes
  call get_parms('x', '1', hval, ios)
  call c2var(imax, '(i16)', hval)
  !c+++ y-axis-sizes
  call get_parms('y', '1', hval, ios)
  call c2var(jmax, '(i16)', hval)
  !c+++ z-axis-sizes
  call get_parms('z', '1', hval, ios)
  call c2var(kmax, '(i16)', hval)
  !c+++ axis-names
  call get_parms('ax', 'GLON128'   , haxisx, ios) !! x-axis name
  call get_parms('ay', 'GGLA64'    , haxisy, ios) !! y-axis name
  call get_parms('az', 'NUMBER1000', haxisz, ios) !! z-axis name
  !c+++ time_interval
  call get_parms('tdur', '6.d0', hval, ios) !! time_interval
  call c2var(tdur, '(1pe15.5)', hval)
  call get_parms('htunit', 'HOUR', htunit, ios) !! unit of time
  !c+++ input grads data size (4/8)
  call get_parms('grsize', '4', hval, ios)
  call c2var(grsize, '(i16)', hval)
  if (grsize /= 4.and.grsize /= 8) call xabort
  !c+++ shift x-grids (default 0)
  call get_parms('xshift', '0', hval, ios)
  call c2var(xshift, '(i16)', hval)
  !c+++ data format (UR4/UR8)
  call get_parms('dfmt', 'UR4', hdfmt, ios)
  !c+++ convert mode
  call get_parms('conv', 'GR2GT', hval, ios)
  ncol = scan(hval, '2')
  hsta = hval(1:ncol-1)
  hend = hval(ncol+1:len_trim(hval))
  ogtr   = .false. !! f: read grads format, t: gtool3
  ogtw   = .true.  !! f: write grads format, t: gtool3
  if (hsta == 'GT') ogtr = .true.
  if (hsta == 'GR') ogtr = .false.
  if (hend == 'GT') ogtw = .true.
  if (hend == 'GR') ogtw = .false.
  write(6, '(a, l1)') 'getparms: ogtr = ', ogtr
  write(6, '(a, l1)') 'getparms: ogtw = ', ogtw
  !c+++ option: write time (f: write start time, t: write centre time)
  call get_parms('cent', 'f', hval, ios)
  call c2var(otcent, '(l1)', hval)
  !c+++ option: time modification (t: enable time modification, f: disable)
  call get_parms('modt', 'f', hval, ios)
  call c2var(omodt, '(l1)', hval)
  !c+++ option: reverse y-grids (t: y-rev. on, f: off)
  call get_parms('yrev', 'f', hval, ios)
  call c2var(oyrev, '(l1)', hval)
  !c+++ option: reverse z-grids (t: z-rev. on, f: off)
  call get_parms('zrev', 'f', hval, ios)
  call c2var(ozrev, '(l1)', hval)
  !c+++ option: endian-rev. (t: endian-rev. on, f: off)
  call get_parms('exch', 'f', hval, ios)
  call c2var(oexch, '(l1)', hval)
  !c+++ option: write mode (t: append, f: replace)
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine xabort
!c=====
subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtconv -i input-file -o output-file'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-sta start-time -end end-time (for time count)'
  write(6, '(a)') ' '
  write(6, '(a)') 'for grads input'
  write(6, '(a)') '-sta, -end is used for time count'
  write(6, '(a)') 'input file is always read from the head'
  write(6, '(a)') ' '
  write(6, '(a)') 'for gtool input'
  write(6, '(a)') '-sta is used for determining the start point'
  write(6, '(a)') '-end is used for determining the end'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-tdur time_interval -htunit time_unit'
  write(6, '(a)') '  htunit = HOUR/MIN/SEC/DAY/MON/YEAR'
  write(6, '(a)') '  (defaults: tdur=6.d0, htunit=HOUR)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'only for grads input'
  write(6, '(a)') '-ax x_name -ay y_name -az z_name'
  write(6, '(a)') '-x x_size -y y_size -z z_size'
  write(6, '(a)') '-vmiss value (missing value, defalut: 999.d0)'
  write(6, '(a)') '-grsize 4/8 (input data size, default: 4)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'only for gtool3 output'
  write(6, '(a)') '-dset data_set -item hitem -titl title -unit unit'
  write(6, '(a)') '  (defalts: same as input data)'
  write(6, '(a)') '-nsyy year -nsmm month -nsdd day (data start time)'
  write(6, '(a)') '-cent f/t (default: f)'
  write(6, '(a)') '  (f: write start time, t: write centre time)'
  write(6, '(a)') '-modt f/t (default: f)'
  write(6, '(a)') '  (enable time mod., modt most be t for grads input)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: UR4)'
  write(6, '(a)') '  (output data format of gtool)'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '  (append option)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-conv GR2GT/GT2GR/GR2GR/GT2GT'
  write(6, '(a)') '  conv = GR2GT: grads  ==> gtool3'
  write(6, '(a)') '  conv = GT2GR: gtool3 ==> grads'
  write(6, '(a)') '  conv = GR2GR: grads  ==> grads'
  write(6, '(a)') '  conv = GT2GT: gtool3 ==> gtool3'
  write(6, '(a)') ' '
  write(6, '(a)') '-yrev f/t -zrev f/t (reverse y-, z-grids)'
  write(6, '(a)') '-xshift x_shift (shift x-grids, default 0)'
  write(6, '(a)') '-exch f/t (change endian: little=>big or big=>little)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') 'ex. grads ==> gtool3: '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'gtconv -i inp.grd -o out.gt -conv GR2GT' 
  write(6, '(a)') '-ax GLON128 -ay GGLA64 -az GPLV31 -x 1 -y 64 -z 31'
  write(6, '(a)') '-nsyy 2010 -nsmm 1 -nsdd 1 -cent f -modt t' 
  write(6, '(a)') '-sta 1 (is necessary for grads input)'
  write(6, '(a)') '------------------------------------------'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtconv
