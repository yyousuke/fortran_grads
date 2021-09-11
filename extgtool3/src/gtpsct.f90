!c
!c  program gtpsct
!c  [history]
!c  2013/10/13 Yamashita: first ver.
!c  2013/10/18 Yamashita: modify gtopen and remove seek_iounit
!c  2018/03/26 Yamashita: modify getparms & I/O 
!c  2018/03/26 Yamashita: use common_typedef & common_const & setup_cnst
!c  2018/03/26 Yamashita: set_plevs is replaced by set_pres
!c
!c  PSC area poleward of 60 (deg) at selected level with 195K/188K criterion
!c
!c
!c  internal: module common_psc
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module common_typedef
!c  external: module util
!c            subroutines getyaxis, getzaxis, set_plevs
!c  external: module rwgtool
!c            subroutine gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd
!c  external: module dcalclate
!c            subroutine dshift
!c  external: module error_handler 
!c            subroutines ioerror, werr
!c
!c=====================================================================c
module common_psc
  use common_typedef, only: r8b
  use common_const, only: cnst_eradius, cnst_h, const_setup

  real(kind=r8b), save       :: a        !! equatorial radius (m)
  real(kind=r8b), save       :: h        !! scale height [m]

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    a = cnst_eradius
    h = cnst_h
!   a = 6.370d6 
!   h = 6950.d0
  end subroutine setup_cnst
!c----------------------------------------------------------------------c
end module common_psc

!c=====================================================================c

program gtpsct
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_psc, only: setup_cnst, a, h
  use util, only: getyaxis, getzaxis, set_pres
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd
  use dcalculate, only: dshift
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r8b), allocatable :: d(:,:,:)       !! input data (cm2/cm3)
  real(kind=r8b), allocatable :: ps(:,:)        !! input Ps [hPa]
  real(kind=r8b), allocatable :: lat(:)         !! lat[deg]
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header (ps)
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax           !! x-size
  integer(kind=i4b)           :: jmax           !! y-sizes
  integer(kind=i4b)           :: kmax           !! z-size
  real(kind=r8b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisy         !! y-axis name
  character(len=ncc)          :: haxisz         !! z-axis name
  character(len=ncc*2)        :: htitlz         !! title
  !c+++ input from getparms
  !c+++ (ncc: length of gtool3 header, nfiln: length of file name)
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  integer(kind=i4b)           :: nymin          !! min. limit
  integer(kind=i4b)           :: nymax          !! max. limit
  real(kind=r8b)              :: lev            !! level (hPa)
  real(kind=r8b)              :: val            !! PSC criterion (K)
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: ips            !! input ps file name
  character(len=nfiln)        :: ip             !! input pressure file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  character(len=ncc)          :: zax            !! zaxis plev/zlev/siglv/etalv
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  real(kind=r8b), allocatable :: dout(:,:,:)    !! output data
  real(kind=r8b)              :: psc_out        !! output PSC Area (m2)
  !c+++ [work]
  integer(kind=i4b)           :: it             !!
  real(kind=r8b), allocatable :: pres(:,:,:)    !! [hPa]
  integer(kind=i4b)           :: ios            !! end code
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input T/TH file
  integer(kind=i4b)           :: jfile_ps       !! for input ps file
  integer(kind=i4b)           :: jfile_p        !! for input pressure file
  integer(kind=i4b)           :: jfile_o        !! for output file
  !c+++ [internal switch]
  logical                     :: osig = .false. !! enable sigma-lev.
  logical                     :: oeta = .false. !! enable eta-lev.
  logical                     :: ozlv = .false. !! enable z-lev.
  logical                     :: opin = .false. !! enable pressure input

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev' ) ozlv = .true.
  if (trim(ip) /= 'NONE' ) opin = .true.
  if (ozlv.and. .not.opin) osig = .true. !! for Ps input

  !c+++ constants
  call setup_cnst

  !c+++ open input file
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input ps file
  if (osig.or.oeta) then
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
  endif

  !c+++ open input pressure file
  if (opin) then
    write(6, *) 'open input pressure file: ', trim(ip)
    call gtopen(trim(ip), 'r', jfile_p, ios)
    if (ios /= 0) call ioerror(jfile_p, ios)
  endif

  !c+++ open output file
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
  !c+++ set y-, z-axis names
  haxisy = head(32)
  haxisz = head(35)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  write(6, *) 'haxisy = ', haxisy
  write(6, *) 'haxisz = ', haxisz
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(ps(imax,jmax), lat(jmax))
  allocate(d(imax,jmax,kmax))
  allocate(pres(imax,jmax,kmax))

  !c+++ read y-axis file
  call getyaxis(jmax, haxisy, lat)
  write(6, '(2(a, f7.3))') 'range = ', lat(nymin), '-', lat(nymax)

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
      if (osig.or.oeta) then
        call gtskip(jfile_ps, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_ps, ios)
      endif
      if (opin) then
        call gtskip(jfile_p, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_p, ios)
      endif
      cycle
    endif

    !c+++ read input file
    call rgt(jfile_i, imax, jmax, kmax, head, d, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ read surface pressure for sig-lev. and eta-lev.
    if (osig.or.oeta) then
      call rgt(jfile_ps, imax, jmax, 1, head2, ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, 1, rmiss, ps, ps, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps(1,1), ' (hPa)'
      endif
    endif

    !c+++ set pres
    if (opin) then
      !c+++ read input pressure data
      call rgt(jfile_p, imax, jmax, kmax, head2, pres, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_p, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, kmax, rmiss, pres, pres, 0.01d0, 0.d0)
      endif
    else
      call set_pres(imax, jmax, kmax, rmiss, haxisz, h, ps, pres, osig, oeta, ozlv)
    endif

    !c+++ PSC Area
    call psc_area(imax, jmax, kmax, nymin, nymax, lev, rmiss, d, pres, lat, val, psc_out)

    !c+++ set gtool header
    head(3)  = hitem
    htitlz   = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit
    head(35) = 'SFC1'
    write(head(31), '(i16)') 1
    write(head(34), '(i16)') 1
    write(head(37), '(i16)') 1
    write(head(64), '(i16)') 1

    !c+++ write PSC data
    allocate(dout(1,1,1))
    dout(1,1,1) = psc_out
    call wgthdd(jfile_o, 1, 1, 1, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)
    deallocate(dout)

    if (it == iend) exit
  enddo !! it
  !c+++ close files
  call gtclose(jfile_i, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  if (opin) call gtclose(jfile_p, ios)
  call gtclose(jfile_o, ios)
  !c+++ deallocate
  deallocate(ps, lat, d, pres)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine psc_area
!c  calculate PSC Area (m2)
!c=====
subroutine psc_area(imax, jmax, kmax, nymin, nymax, plev, rmiss, d, pres, lat, val, psc_out)
  use common_psc, only: a
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax                 !! x-size
  integer(kind=i4b), intent(in) :: jmax                 !! y-sizes
  integer(kind=i4b), intent(in) :: kmax                 !! z-size
  integer(kind=i4b), intent(in) :: nymin                !! min. limit of latitude
  integer(kind=i4b), intent(in) :: nymax                !! max. limit of latitude
  real(kind=r8b), intent(in)    :: plev                 !! level of PSC area (hPa)
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r8b), intent(in)    :: d(imax,jmax,kmax)    !! input data (K)
  real(kind=r8b), intent(in)    :: pres(imax,jmax,kmax) !! 3-D pressure (hPa)
  real(kind=r8b), intent(in)    :: lat(jmax)            !! lat (deg)
  real(kind=r8b), intent(in)    :: val                  !! PSC criterion (K)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: psc_out              !! output PSC Area (m2)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k              !!
  integer(kind=i4b)             :: k_c                  !!
  real(kind=r8b)                :: phi(jmax)            !! lat (rad)
  real(kind=r8b)                :: xlatm(jmax+1)        !! meridional length (m)
  real(kind=r8b)                :: area                 !! (m2)
  real(kind=r8b)                :: psc                  !! PSC Area (m2)
  real(kind=r8b)                :: pi                   !!
  pi = 4.d0 * datan(1.d0)

  !c+++ prepare
  do j = 1, jmax
    phi(j) = lat(j) * pi / 180.d0
  enddo !! j
  xlatm(1) = 90.d0 * pi / 180.d0
  xlatm(jmax+1) = -90.d0 * pi / 180.d0
  do j = 1, jmax-1
    xlatm(j+1) = 0.5d0 * (phi(j) + phi(j+1))
  enddo !! j

  !c+++ PSC Area
  psc = 0.d0
  do j = nymin, nymax
    area = 2.d0 * pi * a**2 / dble(imax) * &
&          abs(0.5d0 * (cos(xlatm(j)) + cos(xlatm(j+1))) * (xlatm(j) - xlatm(j+1)))
    do i = 1, imax
      k_c = 0
      do k = 3, kmax - 1
        if (pres(i,j,k) <= plev) then
          if (abs(pres(i,j,k) - plev) <= abs(pres(i,j,k-1) - plev)) then
            k_c = k
          else
            k_c = k - 1
          endif
          exit
        endif
      enddo !!
      if (k_c /= 0.and.d(i,j,k) /= rmiss.and.d(i,j,k) <= val) then
        psc = psc + area !! (m2)
      endif
    enddo !! i
  enddo !! j
  psc_out = psc

  return
end subroutine psc_area

!c----------------------------------------------------------------------c

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
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('p', 'NONE', ip, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'PSC', hitem, ios)
  call get_parms('titl', 'PSC Area', htitl, ios)
  call get_parms('unit', 'm2', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ lat range
  call get_parms('j1', '1', hval, ios)    !! min. lat. (90N)
  call c2var(nymin, '(i16)', hval)
  call get_parms('j2', '11', hval, ios)   !! max. lat. (60N)
  call c2var(nymax, '(i16)', hval)
  !c+++ level
  call get_parms('lev', '50.d0', hval, ios)
  call c2var(lev, '(1pe15.5)', hval)
  !c+++ PSC criterion (K)
  call get_parms('val', '195.d0', hval, ios)
  call c2var(val, '(1pe15.5)', hval)
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
  write(6, '(a)') 'Usage: gtpsc'
  write(6, '(a)') '-i input-T-file (K) -o output-file'
  write(6, '(a)') '(unit of input-file must be K)'
  write(6, '(a)') ' '
  write(6, '(a)') '-val temperature (K, PSC criterion)'
  write(6, '(a)') '-j1 min-lat -j2 max-lat (range of PSC area calculation)'
  write(6, '(a)') '-lev pressure (hPa, level of PSC area calculation)'
  write(6, '(a)') ' '
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-p input-p-file (pressure [hPa or Pa], optional)'
  write(6, '(a)') '  (default, NONE)'
  write(6, '(a)') '-ps input-ps-file (surface pressure [hPa or Pa], optional)'
  write(6, '(a)') '  (default, Ps)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-zax plev/zlev/siglv/etalv'
  write(6, '(a)') 'plev: assume p-levels (default), and ignore -ps option'
  write(6, '(a)') 'zlev: assume z-levels'
  write(6, '(a)') '(either -p input-p-file or -ps input-ps-file)'
  write(6, '(a)') 'siglv: assume sigma-levels, etalv: assume eta-levels'
  write(6, '(a)') '(-ps input-ps-file) for siglv/etalv'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtpsct
