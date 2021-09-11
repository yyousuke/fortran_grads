!c
!c  program gtps2p
!c  [history]
!c  2013/03/11 Yamashita: 3-D pressure output (from 2013/03/03 gttheta_p.f90 by Yamashita)
!c  2017/10/08 Yamashita: modify getparms & I/O 
!c
!c  output 3-D pressure [hPa] for p-, sig-, and eta-levels
!c
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module common_typedef
!c  external: module calculate
!c            subroutine shift
!c  external: module util
!c            subroutines getyaxis, getzaxis, set_plevs
!c  external: module rwgtool
!c            subroutine gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd
!c
!c======================================================================c

program gtps2p
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use calculate, only: shift
  use util, only: getzaxis, set_plevs
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgthdd, get_zaxsize, get_etacoef
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r4b), allocatable :: ps(:,:)        !! Ps[hPa]
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
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
  character(len=ncc)          :: haxisz         !! z-axis name
  character(len=ncc)          :: hdfmt          !! data format
  character(len=ncc)          :: zax            !! zaxis plev/zlev/siglv/etalv
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: pout(:,:,:)    !! [hPa]
  !c+++ [internal work]
  integer(kind=i4b)           :: it             !!
  integer(kind=i4b)           :: ios            !! end code
  character(len=ncc*2)        :: htitlz         !!
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_ps       !! for input ps file
  integer(kind=i4b)           :: jfile_o        !! for output file
  !c+++ [internal switch]
  logical                     :: osig = .false. !! enable sigma-lev.
  logical                     :: oeta = .false. !! enable eta-lev.

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.

  !c+++ open input ps file
  write(6, *) 'open input ps file: ', trim(ips)
  call gtopen(trim(ips), 'r', jfile_ps, ios)
  if (ios /= 0) call ioerror(jfile_ps, ios)

  !c+++ open output file (3-d output of pressure)
  write(6, *) 'open output file: ', trim(ofile)
  if (oapnd) then
    call gtopen(trim(ofile), 'a', jfile_o, ios)
  else
    call gtopen(trim(ofile), 'w', jfile_o, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_ps, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_ps, ios)
  call gtrewind(jfile_ps, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ set z-axis-size
  write(6, *) 'haxisz = ', haxisz
  call get_zaxsize(haxisz, kmax)
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(ps(imax,jmax), p(kmax))
  allocate(pout(imax,jmax,kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
  ps(1:imax,1:jmax) = rmiss
  eta_fa(1:kmax) = rmiss
  eta_fb(1:kmax) = rmiss

  !c+++ read z-axis file (for p-lev & sig-lev)
  if (.not. oeta) then
    !c+++ read z-axis file (for p-lev & sig-lev)
    call getzaxis(kmax, haxisz, p)
  else
    !c+++ read z-axis file (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  endif

!c
!c 3-D P output
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      cycle
    endif

    !c+++ read surface pressure
    call rgt_r4(jfile_ps, imax, jmax, 1, head, ps, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_ps, ios)
    !c+++ Pa ==> hPa
    if (trim(head(16)) == 'Pa') then
      call shift(imax, jmax, 1, rmiss, ps, ps, 0.01d0, 0.d0)
      write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps(1,1), ' (hPa)'
    endif

    !c+++ set pres
    call set_plevs(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pout, osig, oeta)

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit
    head(35) = haxisz
    write(head(37), '(i16)') kmax
    write(head(64), '(i16)') imax*jmax*kmax

    !c+++ write gtool3 header & 3-D P data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, pout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo

  !c+++ deallocate
  deallocate(ps, p, pout)
  deallocate(eta_fa, eta_fb)
  !c+++ close files
  call gtclose(jfile_ps, ios)
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
  call get_parms('item', 'GAP', hitem, ios)
  call get_parms('titl', 'pressure', htitl, ios)
  call get_parms('unit', 'hPa', hunit, ios)
  call get_parms('z', ' ', haxisz, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  !c+++ axis
  call get_parms('zax', 'plev', zax, ios)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine xabort
!c=====
subroutine xabort
  write(6, '(a)') 'Usage: gtps2p -o output-file'
  write(6, '(a)') '-ps input-ps-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-z haxisz'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-zax plev/siglv/etalv'
  write(6, '(a)') 'plev: assume p-levels (default)'
  write(6, '(a)') 'siglv: assume sigma-levels, etalv: assume eta-levels'
  write(6, '(a)') '------------------------------------------'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtps2p
