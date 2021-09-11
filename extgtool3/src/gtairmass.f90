!c
!c  program gtairmass
!c  [history]
!c  2013/10/14 Yamashita: first ver.
!c  2013/10/18 Yamashita: modify gtopen and remove seek_iounit
!c  2017/08/03 Yamashita: modify getparms & I/O 
!c  2017/08/03 Yamashita: use common_typedef & common_const
!c  2017/08/04 Yamashita: use setup_cnst
!c
!c  airmass = Ps [hPa] * 100  / g
!c  (Pa = N/m2 = kg/m2 * m/s2)
!c
!c  internal: module common_airmass
!c            subroutine setup_cnst
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module dcalculate
!c            subroutines dshift
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd
!c  external: module  error_handler
!c            subroutine ioerror
!c
!c=====================================================================c
module common_airmass
  use common_typedef, only: r8b
  use common_const, only: cnst_egrav, const_setup
  !c+++ [parameter]
  real(kind=r8b), save          :: g                !! grav. acceleration of the Earth (m/s2)

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    g = cnst_egrav
    !ccc g = 9.81d0
  end subroutine setup_cnst
!c----------------------------------------------------------------------c

end module common_airmass

!c=====================================================================c
program gtairmass
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_airmass, only: setup_cnst, g
  use dcalculate, only: dshift
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from rgt
  real(kind=r8b), allocatable :: din(:,:,:)     !! Ps [hPa] or Ps [Pa] => Ps [hPa]
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax           !! x-axis sizes
  integer(kind=i4b)           :: jmax           !! y-axis sizes
  integer(kind=i4b)           :: kmax           !! z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  !c+++ input from getparms
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  character(len=nfiln)        :: ips            !! input ps file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  !c+++ output with wgthdd
  real(kind=r8b), allocatable :: dout(:,:,:)    !!
  !c+++ [internal work]
  integer(kind=i4b)           :: it, i, j, k    !! loop variables
  integer(kind=i4b)           :: ios            !! end code
  real(kind=r8b)              :: fact           !! factor 
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input file
  integer(kind=i4b)           :: jfile_o        !! for output file

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ constants
  call setup_cnst

  !c+++ open input gtool file
  write(6, *) 'open input file: ', trim(ips)
  call gtopen(trim(ips), 'r', jfile_i, ios)
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
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

!c
!c main
!c===
  it = 0
  do while(1 == 1)
    it = it + 1
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
    !c+++ read data
    call rgt(jfile_i, imax, jmax, kmax, head, din, ios)
    if (ios /= 0) then
      deallocate(din, dout)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
    endif
    !c+++ Pa ==> hPa
    if (trim(head(16)) == 'Pa') then
      call dshift(imax, jmax, kmax, rmiss, din, din, 0.01d0, 0.d0)
      write(6, '(a, f9.4, a)') 'ps(1,1) = ', din(1,1,1), ' (hPa)'
    endif

    !c+++ set unit of output data
    if (hunit /= 'NULL') head(16) = hunit
    !c+++ set item of output data
    if (hitem /= 'NULL') head(3) = hitem
    !c+++ set title of output data
    if (htitl /= 'NULL') then
      head(14) = htitl(1:16)
      head(15) = htitl(17:32)
    endif

    !c+++ airmass = Ps * 100 / g
    fact = 100.d0 / g
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          if (din(i,j,k) /= rmiss) then
            dout(i,j,k) = din(i,j,k) * fact
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
  !c+++ input/output files
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'AIRMASS', hitem, ios)
  call get_parms('titl', 'air mass', htitl, ios)
  call get_parms('unit', 'kg/m|2"', hunit, ios)
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

!c----------------------------------------------------------------------c
!c  subroutine xabort
!c=====

subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtairmass -o output-file'
  write(6, '(a)') '-ps input-ps-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtairmass
