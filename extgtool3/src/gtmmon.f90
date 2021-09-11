!c
!c  program gtmmon
!c  [history]
!c  2008/01/18 Yamashita: first ver.
!c  2013/02/01 Yamashita: for CCMI
!c  2013/03/11 Yamashita: use modules
!c  2013/09/27 Yamashita: add getparms for parameter input
!c  2013/10/13 Yamashita: modify gtopen
!c  2017/05/30 Yamashita: modify getparms & I/O 
!c  2017/05/30 Yamashita: use common_typedef
!c
!c  modify time of GTOOL3 header
!c
!c  external: module common_args
!c  external: module common_typedef
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module ucaln
!c            subroutines tcent, eday
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtskip, rgthd, rgt, wgthdd
!c  eaternal: error_handler
!c            subroutine ioerror
!c
!c=====================================================================c
program gtmmon
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use ucaln, only: tcent, eday
  use rwgtool, only: gtopen, gtclose, gtskip, rgthd, rgt, wgthdd
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  character(len=nfiln)        :: ifile            !! input file name
  character(len=nfiln)        :: ofile            !! output file name
  character(len=ncc)          :: hitem            !! new item of output data
  character(len=ncc*2)        :: htitl            !! new title of output data
  character(len=ncc)          :: hunit            !! new unit of output data
  character(len=ncc)          :: htunit           !! unit of time
  character(len=ncc)          :: hdfmt            !! data format
  integer(kind=i4b)           :: nsyy, nsmm       !! start year/mon
  integer(kind=i4b)           :: ista             !! start record
  integer(kind=i4b)           :: iend             !! end record
  logical                     :: oapnd            !! t: append, f: replace
  !c+++ input from rgthd
  character(len=ncc)          :: head(ndc)        !! gtool3 header
  integer(kind=i4b)           :: imax, jmax, kmax !! x-, y-, z-sizes
  real(kind=r8b)              :: rmiss            !! missing value
  !c+++ [input/output]
  !c+++ input with rgtd & output with wgthdd
  real(kind=r8b), allocatable :: d(:,:,:)         !!
  !c+++ [work]
  integer(kind=i4b)           :: it               !!
  integer(kind=i4b)           :: jyy, jmm         !! current month
  integer(kind=i4b)           :: jyyn, jmmn       !! next month
  integer(kind=i4b)           :: iyy, imm, idd    !! center date
  integer(kind=i4b)           :: ihh, imn, iss    !! center time
  integer(kind=i4b)           :: ndd              !! number of days
  integer(kind=i4b)           :: jdate(6)         !! time array
  integer(kind=i4b)           :: ios              !! end code
  integer(kind=i4b)           :: jfile_i, jfile_o !! I/O unit for input/output file
  !c+++ time
  real(kind=r4b)              :: tdur             !!
  real(kind=r4b)              :: time = 0.        !!
  real(kind=r4b)              :: timeb = 0.       !!

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ open input gtool file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open output gtool file
  write(6, *) 'open output file: ', trim(ofile)
  if (oapnd) then
    call gtopen(trim(ofile), 'a', jfile_o, ios)
  else
    call gtopen(trim(ofile), 'w', jfile_o, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

!c
!c main
!c===
  jyy = nsyy
  jmm = nsmm
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      cycle
    endif

    !c+++ read header & set axis-sizes, missing value
    call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    backspace(jfile_i)
    !c+++ allocate
    allocate(d(imax,jmax,kmax))

    !c+++ read gtool3 header & data
    call rgt(jfile_i, imax, jmax, kmax, head, d, ios)
    if (ios /= 0) then
      deallocate(d)
      call ioerror(jfile_i, ios)
    endif

    !c+++ set data format of output data
    if (hdfmt == 'NULL') hdfmt = head(38)
    !c+++ set unit of output data
    if (hunit /= 'NULL') head(16) = hunit
    !c+++ set item of output data
    if (hitem /= 'NULL') head(3) = hitem
    !c+++ set title of output data
    if (htitl /= 'NULL') then
      head(14) = htitl(1:16)
      head(15) = htitl(17:32)
    endif

    !c+++ set time, tdur
    call eday(jyy, jmm, ndd)
    tdur = real(ndd)
    time = timeb
    timeb = time + tdur
    !c+++ next month
    jyyn = jyy
    jmmn = jmm + 1
    if (jmmn == 13) then
      jyyn = jyy + 1
      jmmn = 1
    endif
    !c+++ center time
    call tcent(jyy, jmm, 1, 0, 0, 0, jyyn, jmmn, 1, 0, 0, 0, iyy, imm, idd, ihh, imn, iss)
    time = time + real(idd)

    !c+++ set jdate
    jdate(1) = iyy
    jdate(2) = imm
    jdate(3) = idd
    jdate(4) = ihh
    jdate(5) = imn
    jdate(6) = iss
    write(head(27), '(i4.4, 2i2.2, 1x, 3i2.2)') jdate(1:6)
    write(head(25), '(i16)') int(time)
    write(head(28), '(i16)') int(tdur)
    !c+++ set time unit of output data
    if (htunit /= 'NULL') head(26) = htunit

    !c+++ write gtool3 header & data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, d, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    deallocate(d)
    jmm = jmm + 1
    if (jmm == 13) then
      jyy = jyy + 1
      jmm = 1
    endif
    if (it == iend) exit
  enddo !! it
  !c+++ close files
  call gtclose(jfile_i, ios)
  call gtclose(jfile_o, ios)

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
  character(len=ncc)            :: hval             !!
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  call get_parms('htunit', 'DAY', htunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ start year/month
  call get_parms('nsyy', '2000', hval, ios)
  call c2var(nsyy, '(i4.4)', hval)
  call get_parms('nsmm', '01', hval, ios)
  call c2var(nsmm, '(i2.2)', hval)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
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
  write(6, '(a)') 'gtmmon -i input-file -o output-file'
  write(6, '(a)') '-nsyy start-year -nsmm start-month (for output data)'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-htunit DAY (default: DAY)'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtmmon
