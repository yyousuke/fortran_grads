!c
!c  program gtzonal
!c  [history]
!c  2014/10/06 Yamashita: first ver. (from gtconst.f90)
!c  2017/05/15 Yamashita: use common_typedef
!c  2017/05/15 Yamashita: modify getparms & I/O 
!c
!c  replace input data by its zonal mean
!c
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module rwgtool
!c            subroutine gtopen, gtclose, gtskip, rgthd, rgt, wgthdd
!c  external: module zmean
!c            subroutine ZONALmean
!c  eaternal: error_handler
!c            subroutine ioerror
!c
!c=====================================================================c
program gtzonal
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use rwgtool, only: gtopen, gtclose, gtskip, rgthd, rgt, wgthdd
  use dzmean, only: dZONALmean
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  real(kind=r8b), allocatable :: d(:,:,:)       !! input data
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax           !! x-, y-, z-axis sizes
  integer(kind=i4b)           :: jmax           !! x-, y-, z-axis sizes
  integer(kind=i4b)           :: kmax           !! x-, y-, z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  !c+++ input from getparms
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  integer(kind=i4b)           :: ista, iend     !! start/end record
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  real(kind=r8b), allocatable :: r(:,:,:)       !! replace data
  !c+++ [work]
  integer(kind=i4b)           :: it, i          !!
  integer(kind=i4b)           :: ios            !! end code
  real(kind=r8b), allocatable :: zm(:,:)        !! zonal mean data
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input file
  integer(kind=i4b)           :: jfile_o        !! for output file

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

    !c+++ read gtool header & set axis-sizes, missing value
    call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    backspace(jfile_i)
    !c+++ allocate
    allocate(d(imax,jmax,kmax), r(imax,jmax,kmax), zm(jmax,kmax))

    !c+++ read gtool header & data
    call rgt(jfile_i, imax, jmax, kmax, head, d, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ zonal mean (d ==> zm)
    call dZONALmean(imax, jmax, kmax, rmiss, d, zm)
    !c+++ set replace data
    do i = 1, imax
      r(i,1:jmax,1:kmax) = zm(1:jmax,1:kmax)
    enddo !! i

    !c+++ set data format of output data
    if (hdfmt == 'NULL') hdfmt = head(38)
    !c+++ set unit of gtool header
    if (hunit /= 'NULL') head(16) = hunit
    !c+++ set item of gtool header
    if (hitem /= 'NULL') head(3) = hitem
    !c+++ set title of gtool header
    if (htitl /= 'NULL') then
      head(14) = htitl(1:16)
      head(15) = htitl(17:32)
    endif

    !c+++ write replace data (gtool3 header & data)
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, r, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    deallocate(d, r, zm)
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
!c
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
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtzonal -i input-file -o output-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtzonal
