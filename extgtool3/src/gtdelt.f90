!c
!c  program gtdelt
!c  [history]
!c  2016/11/22 Yamashita: first ver.
!c  2017/04/28 Yamashita: modify getparms & I/O 
!c  2017/05/01 Yamashita: use common_typedef
!c
!c  dout = (din - din_prev) / tdur
!c
!c  external: module common_args
!c  external: module common_typedef
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module util
!c            subroutines get_time
!c  external: module ucaln
!c            subroutine datetime2sec
!c  external: module rwgtool
!c            subroutine gtopen, gtclose, gtrewind, rgthd, rgt, wgthdd
!c  eaternal: error_handler
!c            subroutine ioerror
!c
!c=====================================================================c
program gtdelt
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use util, only: get_time
  use ucaln, only: datetime2sec
  use rwgtool, only: gtopen, gtclose, gtrewind, rgthd, rgt, wgthdd
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  character(len=nfiln)        :: ifile        !! input file name
  character(len=nfiln)        :: ofile        !! output file name
  character(len=ncc)          :: hitem        !! new item of output data
  character(len=ncc*2)        :: htitl        !! new title of output data
  character(len=ncc)          :: hunit        !! new unit of output data
  character(len=ncc)          :: hdfmt        !! data format
  integer(kind=i4b)           :: ista         !! start record
  integer(kind=i4b)           :: iend         !! end record
  logical                     :: ocent        !! t: centre, f: backward for time diff.
  logical                     :: oapnd        !! t: append, f: replace
  !c+++ input from rgthd
  character(len=ncc)          :: head(ndc)    !! gtool3 header
  integer(kind=i4b)           :: imax         !! x-axis sizes
  integer(kind=i4b)           :: jmax         !! y-axis sizes
  integer(kind=i4b)           :: kmax         !! z-axis sizes
  real(kind=r8b)              :: rmiss        !! missing value
  !c+++ input from rgtd
  real(kind=r8b), allocatable :: d1(:,:,:)    !! input data (t=t1) previous
  real(kind=r8b), allocatable :: d2(:,:,:)    !! input data (t=t2) current for backward, next for cent.
  real(kind=r8b), allocatable :: d0(:,:,:)    !! input data (t=t0) for save
  !c+++ [output]
  !c+++ output with wgthdd
  real(kind=r4b), allocatable :: dout(:,:,:)  !! output data
  !c+++ [work]
  integer(kind=i4b)           :: it, i, j, k
  integer(kind=i4b)           :: ios          !! end code
  !c+++ time
  integer(kind=i4b)           :: jdate(6)     !! yy, mm, dd, hh, mn, ss
  real(kind=r8b)              :: tdur         !! tdur = time - timeb
  real(kind=r4b)              :: time         !! time (s)
  real(kind=r4b)              :: timeb        !! previous time (s)
  character(len=ncc)          :: head0(ndc)   !! gtool3 header (for ocent=t)
  character(len=ncc)          :: head1(ndc)   !! gtool3 header (for ocent=t)
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i      !! for input file
  integer(kind=i4b)           :: jfile_o      !! for output file

!c
!c prepare
!c===
  time = 0.0
  timeb = 0.0
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

  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)
  call gtrewind(jfile_i, ios)
  !c+++ allocate
  allocate(d1(imax,jmax,kmax), d2(imax,jmax,kmax), dout(imax,jmax,kmax))
  if (ocent) allocate(d0(imax,jmax,kmax))

!c
!c main
!c===
  if (ocent) then
    !c+++ read data
    call rgt(jfile_i, imax, jmax, kmax, head, d2, ios)
    if (ios /= 0) call ioerror(jfile_i, ios)
    d0(1:imax,1:jmax,1:kmax) = rmiss
    head0 = head
  else
    d2(1:imax,1:jmax,1:kmax) = rmiss
  endif
  it = 0
  do while(1 == 1)
    it = it + 1
    if (ocent) then
      d1(1:imax,1:jmax,1:kmax) = d0(1:imax,1:jmax,1:kmax)
      d0(1:imax,1:jmax,1:kmax) = d2(1:imax,1:jmax,1:kmax)
    else
      d1(1:imax,1:jmax,1:kmax) = d2(1:imax,1:jmax,1:kmax)
    endif
    !c+++ read data (current for ocent=f, next for ocent=t)
    call rgt(jfile_i, imax, jmax, kmax, head, d2, ios)
    if (ios /= 0) call ioerror(jfile_i, ios)
    if (it < ista) cycle
    if (ocent) then
      head1 = head
      head = head0
      head0 = head1
    endif

   !c+++ get time from header
    call get_time(head, jdate, ios)
    if (ios /= 0) call ioerror(-1, ios)
    !c+++ jdate ==> time (s)
    timeb = time
    call datetime2sec(jdate(1), jdate(2), jdate(3), jdate(4), jdate(5), jdate(6), time)
    tdur = time - timeb
    if (ocent) tdur = tdur * 2.d0
    write(6, '(a, e15.5, a)') 'tdur = ', tdur, ' (s)'

    !c+++ set data format of output data
    if (hdfmt == 'NULL') hdfmt = head(38)
    !c+++ set unit of output data
    if (hunit == 'NULL') hunit = trim(head(16))//'/s'
    head(16) = trim(hunit)
    !c+++ set item of output data
    if (hitem == 'NULL') hitem = trim(head(3))//'T'
    head(3) = hitem
    !c+++ set title of output data
    if (htitl /= 'NULL') then
      head(14) = htitl(1:16)
      head(15) = htitl(17:32)
    endif

    !c+++ d(d2-d1)/dt (1/s)
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          if (d1(i,j,k) /= rmiss.and. d2(i,j,k) /= rmiss.and.tdur > 0.d0) then
            dout(i,j,k) = (d2(i,j,k) - d1(i,j,k)) / tdur
          else
            dout(i,j,k) = rmiss
          endif
        enddo !! i
      enddo !! j
    enddo !! k

    !c+++ write gtool3 header & data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo !! it

  !c+++ deallocate
  deallocate(d1, d2, dout)
  if (ocent) deallocate(d0)
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
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ cent
  call get_parms('cent', 'f', hval, ios)
  call c2var(ocent, '(l1)', hval)
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
  write(6, '(a)') 'gtdelt -i input-file -o output-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-cent f/t (f: backward, t: centre, default: f)'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtdelt
