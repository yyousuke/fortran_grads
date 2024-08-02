!c
!c  program gthsel
!c  [history]
!c  2024/05/16 Yamashita: first ver. from gtdob
!c
!c  output surface data at specified level
!c
!c  external: module common_args
!c  external: module common_typedef
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  internal: subroutine hsel
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd,
!c            rgt, rgt_r4, wgthdd, get_etacoef
!c  eaternal: error_handler
!c            subroutine ioerror
!c
!c======================================================================c

program gthsel
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgthdd
  use error_handler, only: werr, ioerror
  implicit none
  !c+++ [input]
  !c+++ input data (rgt_r4)
  real(kind=r4b), allocatable :: gda(:,:,:)     !! [unit]
  real(kind=r4b), allocatable :: gdk(:,:)       !! trop. level. []
  !c+++ input from file (rgthd & rgt_r4)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax           !! x-axis sizes
  integer(kind=i4b)           :: jmax           !! y-axis sizes
  integer(kind=i4b)           :: kmax           !! z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  !c+++ input from getparms
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: kfile          !! input level-file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: dout(:,:,:)    !! [unit]
  !c+++ [internal work]
  integer(kind=i4b)           :: it             !!
  integer(kind=i4b)           :: ios            !! end code
  character(len=ncc*2)        :: htitlz         !!
  !c+++ data
  real(kind=r4b), allocatable :: surface(:,:)   !! [unit]
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input file
  integer(kind=i4b)           :: jfile_k        !! for input level-file
  integer(kind=i4b)           :: jfile_o        !! for output file

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ open input file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input specified-level file
  write(6, *) 'open input level-file: ', trim(kfile)
  call gtopen(trim(kfile), 'r', jfile_k, ios)
  if (ios /= 0) call ioerror(jfile_k, ios)

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
  allocate(gda(imax,jmax,kmax), gdk(imax,jmax))
  allocate(surface(imax,jmax))

!c
!c surface output
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)

      call gtskip(jfile_k, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_k, ios)
      cycle
    endif

    !c+++ read gda
    call rgt_r4(jfile_i, imax, jmax, kmax, head, gda, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    !c+++ read ktp
    call rgt_r4(jfile_k, imax, jmax, 1, head2, gdk, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_k, ios)

    !c+++ GDA, ktp ==> surface
    call hsel(imax, jmax, kmax, rmiss, gda, gdk, surface)

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit
    head(35) = 'SFC1'
    write(head(37), '(i16)') 1
    write(head(64), '(i16)') imax*jmax

    !c+++ write gtool3 header & data
    allocate(dout(imax,jmax,1))
    dout(1:imax,1:jmax,1) = surface(1:imax,1:jmax)
    call wgthdd(jfile_o, imax, jmax, 1, head, hdfmt, dout, ios)
    deallocate(dout)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo

  !c+++ deallocate
  deallocate(gda, gdk, surface)
  !c+++ close files
  call gtclose(jfile_i, ios)
  call gtclose(jfile_k, ios)
  call gtclose(jfile_o, ios)
  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine hsel
!c=====
subroutine hsel(imax, jmax, kmax, rmiss, gda, gdk, dout)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-sizes
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r4b), intent(in)    :: gda(imax,jmax,kmax)  !! (unit)
  real(kind=r4b), intent(in)    :: gdk(imax,jmax)       !! ()
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(imax,jmax)      !! (same unit)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j                 !!
  integer(kind=i4b)             :: ktp                  !! trop. level ()

  !c+++ vertical integration
  do j = 1, jmax
    do i = 1, imax
      if (gdk(i,j) /= rmiss) then
        ktp = int(gdk(i,j))
        dout(i,j) = gda(i,j,ktp)
      endif
    enddo !! i
  enddo !! j

  return
end subroutine hsel

!c----------------------------------------------------------------------c

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
  call get_parms('k', 'ktp', kfile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'GDA', hitem, ios)
  call get_parms('titl', 'data at tropopause level', htitl, ios)
  call get_parms('unit', 'vmr', hunit, ios)
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
  write(6, '(a)') 'gtvsel -o output-surface-file'
  write(6, '(a)') '-i input-file'
  write(6, '(a)') '-k input-specified-level-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') 'ex. Tropause ozone with ktp file: '
  write(6, '(a)') '    gtvsel -i xo3_P -k ktp -o o3_trop'
  write(6, '(a)') ' '
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gthsel
