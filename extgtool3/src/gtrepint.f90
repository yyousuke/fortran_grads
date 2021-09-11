!c
!c  program gtrepint
!c  [history]
!c  2013/10/18 Yamashita: first ver.
!c  2017/10/10 Yamashita: modify getparms & I/O 
!c  2017/10/10 Yamashita: use common_typedef
!c
!c  replace time-varying data by interpolated data
!c
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module intplt
!c            subroutine intplt_t
!c  external: module ucaln 
!c            subroutine date2day
!c  external: module util
!c            subroutines get_time
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd, get_etacoef
!c  eaternal: error_handler
!c            subroutine ioerror, werr
!c
!c=====================================================================c
program gtrepint
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use intplt, only: intplt_t
  use ucaln, only: date2day
  use util, only: get_time
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd, get_etacoef
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)       !! gtool3 header
  character(len=ncc)          :: head2(ndc)      !! gtool3 header
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax            !! x-axis sizes
  integer(kind=i4b)           :: jmax            !! y-axis sizes
  integer(kind=i4b)           :: kmax            !! z-axis sizes
  real(kind=r8b)              :: rmiss           !! missing value
  !c+++ input from getparms
  integer(kind=i4b)           :: ista            !! start record
  integer(kind=i4b)           :: iend            !! end record
  character(len=nfiln)        :: ifile           !! input file name
  character(len=nfiln)        :: rfile           !! input const-data file name
  character(len=nfiln)        :: ofile           !! output file name
  character(len=ncc)          :: hitem           !! item
  character(len=ncc*2)        :: htitl           !! title
  character(len=ncc)          :: hunit           !! unit
  character(len=ncc)          :: itype           !! intplt type (l: linear, s: spline)
  logical                     :: omiss           !! t: enable extrapolation, f: disable
  character(len=ncc)          :: hdfmt           !! data format
  logical                     :: oapnd           !! t: append, f: replace
  !c+++ [output]
  character(len=ncc), allocatable :: head_o(:,:) !! gtool3 header
  !c+++ [input/output]
  real(kind=r8b), allocatable :: d(:,:,:)        !! input/output data
  !c+++ [work]
  integer(kind=i4b)           :: it              !!
  integer(kind=i4b)           :: ios             !! end code
  character(len=ncc*2)        :: htitlz          !!
  integer(kind=i4b)           :: jdate(6)        !! time array
  integer(kind=i4b)           :: iday            !! time
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i         !! for input file
  integer(kind=i4b)           :: jfile_r         !! for input const-data file
  integer(kind=i4b)           :: jfile_o         !! for output file
  !c+++ for interpolation
  integer(kind=i4b)           :: nt_i, nt_o      !! records of input/output time
  real(kind=r8b), allocatable :: t_i(:)          !! input time
  real(kind=r8b), allocatable :: t_o(:)          !! output time
  real(kind=r8b), allocatable :: d_i(:,:,:,:)    !! replace data
  real(kind=r8b), allocatable :: d_o(:,:,:,:)    !! replace data with interpolation

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  iend = max(iend, 999)

  !c+++ open input gtool file
  write(6, *) 'open input-head file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input data file
  write(6, *) 'open input-data file: ', trim(rfile)
  call gtopen(trim(rfile), 'r', jfile_r, ios)
  if (ios /= 0) call ioerror(jfile_r, ios)

  !c+++ open output gtool file
  write(6, *) 'open output file: ', trim(ofile)
  if (oapnd) then
    call gtopen(trim(ofile), 'a', jfile_o, ios)
  else
    call gtopen(trim(ofile), 'w', jfile_o, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)
  call gtrewind(jfile_i, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(d(imax,jmax,kmax), head_o(ndc,iend), t_i(iend), t_o(iend))
  allocate(d_i(imax,jmax,kmax,iend), d_o(imax,jmax,kmax,iend))

!c
!c read input-data-file
!c===
  nt_i = 0
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_r, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_r, ios)
      cycle
    endif

    !c+++ read const-data file
    call rgt(jfile_r, imax, jmax, kmax, head2, d, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    !c+++ check data sizes
    if (head(31) /= head2(31)) call werr(head(31), head2(31), 'Data sizes are not match!')
    if (head(34) /= head2(34)) call werr(head(34), head2(34), 'Data sizes are not match!')
    if (head(37) /= head2(37)) call werr(head(37), head2(37), 'Data sizes are not match!')
    !c+++ check axis names
    if (head(29) /= head2(29)) call werr(head(29), head2(29), 'Axis names are not match!')
    if (head(32) /= head2(32)) call werr(head(32), head2(32), 'Axis names are not match!')
    if (head(35) /= head2(35)) call werr(head(35), head2(35), 'Axis names are not match!')

    nt_i = nt_i + 1
    !c+++ read time
    call get_time(head2, jdate, ios)
    if (ios /= 0) call ioerror(-1, ios)
    call date2day(jdate(1), jdate(2), jdate(3), iday)
    !c+++ save input data
    t_i(nt_i) = dble(iday)
    d_i(1:imax,1:jmax,1:kmax,nt_i) = d(1:imax,1:jmax,1:kmax)
  
    if (it == iend) exit
  enddo !! it
  !c+++ close input data file
  call gtclose(jfile_r, ios)

  !c+++ set unit of output data
  if (hunit == 'NULL') hunit = head2(16)
  !c+++ set item of output data
  if (hitem == 'NULL') hitem = head2(3)
  !c+++ set title of output data
  if (htitl == 'NULL') then
     htitl(1:16) = head2(14)
     htitl(17:32) = head2(15)
  endif

!c
!c read output header
!c===
  nt_o = 0
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
    endif

    !c+++ read input file
    call rgt(jfile_i, imax, jmax, kmax, head, d, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    nt_o = nt_o + 1
    !c+++ read time
    call get_time(head, jdate, ios)
    if (ios /= 0) call ioerror(-1, ios)
    call date2day(jdate(1), jdate(2), jdate(3), iday)
    !c+++ save output time
    t_o(nt_o) = dble(iday)
    !c+++ save output header
    head_o(1:ndc,nt_o) = head(1:ndc)

    if (it == iend) exit
  enddo !! it
  !c+++ close input file
  call gtclose(jfile_i, ios)

!c
!c interpolation
!c===
  call intplt_t(imax, jmax, kmax, nt_i, nt_o, rmiss, &
&   t_i(1:nt_i), t_o(1:nt_o), itype, omiss, &
&   d_i(1:imax,1:jmax,1:kmax,1:nt_i), d_o(1:imax,1:jmax,1:kmax,1:nt_o))
  write(6, *) 'nt_i = ', nt_i
  write(6, *) 'nt_o = ', nt_o

!c
!c write interpolated data
!c===
  do it = 1, nt_o
    !c+++ set output gtool header
    head(1:ndc) = head_o(1:ndc,it)
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit

    !c+++ write replace data
    d(1:imax,1:jmax,1:kmax) = d_o(1:imax,1:jmax,1:kmax,it)
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, d, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == ista+nt_o-1) exit
  enddo !! it
  !c+++ deallocate
  deallocate(d, head_o, t_i, t_o, d_i, d_o)
  !c+++ close output file
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
  !c+++ input/output files
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('r', 'const.gt', rfile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ intplt type: itype
  call get_parms('int', 'linear', hval, ios)
  if (trim(hval) == 'linear') itype = 'l' !! intplt type; l: linear
  if (trim(hval) == 'spline') itype = 's' !! intplt type; s: spline
  !c+++ extrapolation type: omiss
  call get_parms('intext', 'f', hval, ios)
  call c2var(omiss, '(l1)', hval)
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
  write(6, '(a)') 'gtrepint -o output-file'
  write(6, '(a)') '-i input-head-file for definition of output data time'
  write(6, '(a)') '-r input-data-file for replace with interpolation'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-int linear/spline (default: linear)'
  write(6, '(a)') '-int linear: linear interpolation'
  write(6, '(a)') '-int spline: spline interpolation'
  write(6, '(a)') ' '
  write(6, '(a)') '-intext t/f (default: t)'
  write(6, '(a)') '-intext t: enable extrapolation'
  write(6, '(a)') '-intext f: disable'
  write(6, '(a)') '------------------------------------------'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtrepint
