!c
!c  program gtfact
!c  [history]
!c  2013/01/31 Yamashita: first ver.
!c  2013/03/11 Yamashita: use modules
!c  2013/09/27 Yamashita: add getparms for parameter input
!c  2013/10/18 Yamashita: modify gtopen and remove seek_iounit
!c  2014/10/06 Yamashita: add apnd option
!c  2017/05/13 Yamashita: modify getparms & I/O 
!c  2017/05/13 Yamashita: use common_typedef
!c
!c  dout = din * fact + offset
!c
!c  external: module common_args
!c  external: module common_typedef
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtskip, rgthd, rgt, wgthdd
!c  external: module error_handler
!c            subroutine ioerror
!c
!c=====================================================================c
program gtfact
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r8b
  use rwgtool, only: gtopen, gtclose, gtskip, rgthd, rgt, wgthdd
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
  real(kind=r8b)              :: fact         !! factor of output data
  real(kind=r8b)              :: offset       !! offset of output data
  integer(kind=i4b)           :: ista         !! start record
  integer(kind=i4b)           :: iend         !! end record
  logical                     :: oapnd        !! t: append, f: replace
  !c+++ input from file (rgthd)
  character(len=ncc)          :: head(ndc)    !! gtool3 header
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax         !! x-axis sizes
  integer(kind=i4b)           :: jmax         !! y-axis sizes
  integer(kind=i4b)           :: kmax         !! z-axis sizes
  real(kind=r8b)              :: rmiss        !! missing value
  !c+++ input from rgtd
  real(kind=r8b), allocatable :: din(:,:,:)   !!
  !c+++ [output]
  !c+++ output with wgthdd
  real(kind=r8b), allocatable :: dout(:,:,:)  !!
  !c+++ [work]
  integer(kind=i4b)           :: it, i, j, k  !!
  integer(kind=i4b)           :: ios          !! end code
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i      !! for input file
  integer(kind=i4b)           :: jfile_o      !! for output file

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

    !c+++ read header & set axis-sizes, missing value
    call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    backspace(jfile_i)
    !c+++ allocate
    allocate(din(imax,jmax,kmax), dout(imax,jmax,kmax))

    !c+++ read gtool3 header & data
    call rgt(jfile_i, imax, jmax, kmax, head, din, ios)
    if (ios /= 0) then
      deallocate(din, dout)
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

    !c+++ dout = din * fact + offset
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          if (din(i,j,k) /= rmiss) then
            dout(i,j,k) = din(i,j,k) * fact + offset
          else
            dout(i,j,k) = rmiss
          endif
        enddo !! i
      enddo !! j
    enddo !! k

    !c+++ write gtool3 header & data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    deallocate(din, dout)
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
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ fact & offset
  call get_parms('fact', '1.d0', hval, ios)
  call c2var(fact, '(1pe15.5)', hval)
  call get_parms('offset', '0.d0', hval, ios)
  call c2var(offset, '(1pe15.5)', hval)
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
  write(6, '(a)') 'gtfact -i input-file -o output-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-fact 1.d0 -offset 0.d0'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtfact
