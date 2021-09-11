!c
!c  program gtairmassv
!c  [history]
!c  2013/11/12 Yamashita: first ver. (from 2013/10/18 gtairmass.f90 by Yamashita)
!c  2014/08/28 Yamashita: bug fix of subroutine mass
!c  2017/08/04 Yamashita: modify getparms & I/O 
!c  2017/08/04 Yamashita: use common_typedef & common_const
!c  2017/08/04 Yamashita: use setup_cnst
!c
!c  vertically integrated mass content of air in layer
!c
!c  airmass = air [num/cm3] * 10^{6} / Na[num/mol] * mair [kg/mol]
!c          * area [m^2] * height [m]
!c
!c  internal: module common_airmass
!c            subroutine setup_cnst
!c  internal: subroutine mass
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c            subroutines shift
!c  external: module dcalculate
!c            subroutines dshift
!c  external: module util
!c            subroutines getyaxis, getzaxis, set_plevs
!c  external: module calculate
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, rgt_r4, 
!c            wgthdd, get_etacoef
!c  external: module  error_handler
!c            subroutines ioerror, werr
!c
!c=====================================================================c
module common_airmass
  use common_typedef, only: r8b
  use common_const, only: cnst_NA, cnst_mair, cnst_DU, cnst_eradius, &
&   cnst_egrav, cnst_h, const_setup

  !c+++ [parameter]
  real(kind=r8b), parameter     :: NA = cnst_NA     !! Avogadro number (num/mol)
  real(kind=r8b), parameter     :: mair = cnst_mair !! molecular weight of air (kg/mol)
  real(kind=r8b), parameter     :: DU = cnst_DU     !! definition of Dobson unit (num/m2)
  real(kind=r8b), save          :: a                !! equatorial radius (m)
  real(kind=r8b), save          :: g                !! grav. acceleration of the Earth (m/s2)
  real(kind=r8b), save          :: h                !! scale height [m]

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    a = cnst_eradius      !! equatorial radius (m)
    g = cnst_egrav        !! grav. acceleration of the Earth (m/s2)
    h = cnst_h            !! scale height [m]
    !ccc a = 6.370d6
    !ccc g = 9.81d0
  end subroutine setup_cnst
!c----------------------------------------------------------------------c

end module common_airmass

!c=====================================================================c

program gtairmassv
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_airmass, only: setup_cnst, h
  use calculate, only: shift
  use dcalculate, only: dshift
  use util, only: getyaxis, getzaxis, set_plevs
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, rgt_r4, wgthdd, get_etacoef
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r8b), allocatable :: ps(:,:)        !! Ps[hPa]
  real(kind=r8b), allocatable :: lat(:)         !! lat[deg]
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax           !! x-axis sizes
  integer(kind=i4b)           :: jmax           !! y-axis sizes
  integer(kind=i4b)           :: kmax           !! z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisy         !! y-axis name
  character(len=ncc)          :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
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
  logical                     :: onoerr         !! f: stop due to error, t: nothing
  !c+++ input from rgtd
  real(kind=r8b), allocatable :: din(:,:,:)     !!
  !c+++ [output]
  !c+++ output with wgthdd
  real(kind=r8b), allocatable :: dout(:,:,:)    !!
  !c+++ [work]
  integer(kind=i4b)           :: it, k          !! loop variables
  integer(kind=i4b)           :: ios            !! end code
  !c+++ data
  real(kind=r4b), allocatable :: pres(:,:,:)    !! [hPa]
  real(kind=r4b), allocatable :: ps4(:,:)       !! Ps[hPa]
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input file
  integer(kind=i4b)           :: jfile_ps       !! I/O unit for input ps file
  integer(kind=i4b)           :: jfile_p        !! for input pressure file
  integer(kind=i4b)           :: jfile_o        !! for output file
  !c+++ internal switch
  logical                     :: osig = .false. !! enable sigma-lev.
  logical                     :: oeta = .false. !! enable eta-lev.
  logical                     :: ozlv = .false. !! enable z-lev.
  logical                     :: opin = .false. !! enable pressure input

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  !c+++ set switches
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev' ) ozlv = .true.
  if (trim(ip) /= 'NONE' ) opin = .true.
  if (ozlv.and. .not.opin) osig = .true. !! for Ps input

  !c+++ constants
  call setup_cnst

  !c+++ open input gtool file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input ps file
  if (osig.or.oeta) then
    write(6, *) 'open input ps file: ', trim(ips)
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
  endif

  !c+++ open input pressure file
  if (opin) then
    write(6, *) 'open input pressure file: ', trim(ip)
    call gtopen(trim(ip), 'r', jfile_p, ios)
    if (ios /= 0) call ioerror(jfile_p, ios)
  endif

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
  !c+++ set axis-names
  haxisy = head(32)
  haxisz = head(35)
  write(6, *) 'haxisy = ', haxisy
  write(6, *) 'haxisz = ', haxisz
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(din(imax,jmax,kmax), dout(imax,jmax,kmax))
  allocate(ps(imax,jmax), ps4(imax,jmax))
  allocate(lat(jmax), p(kmax))
  allocate(pres(imax,jmax,kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
  ps(1:imax,1:jmax) = rmiss
  eta_fa(1:kmax) = rmiss
  eta_fb(1:kmax) = rmiss

  !c+++ read y-axis file
  call getyaxis(jmax, haxisy, lat)
  write(6, *) 'lat = ', lat

  !c+++ z-coefs.
  if (.not. oeta) then
    !c+++ read z-axis file (for p-lev & sig-lev)
    call getzaxis(kmax, haxisz, p)
    !c+++ z ==> sig (for z-lev)
    if (ozlv) then
      !c+++ z = - h log(sig), -z/h = log(sig), sig = exp(-z/h)
      do k = 1, kmax
        p(k) = exp(- p(k) / h)
      enddo !! j
    endif
    write(6, *) 'p = ', p
  else
    !c+++ read z-axis file (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
    write(6, *) 'eta_fa = ', eta_fa
    write(6, *) 'eta_fb = ', eta_fb
  endif

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

    !c+++ read input data
    call rgt(jfile_i, imax, jmax, kmax, head, din, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    !c+++ read input Ps data
    if (osig.or.oeta) then
      call rgt(jfile_ps, imax, jmax, 1, head2, ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (.not. onoerr) then
        if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      endif
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, 1, rmiss, ps, ps, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps(1,1), ' (hPa)'
      endif
      ps4(1:imax,1:jmax) = real(ps(1:imax,1:jmax))
    endif

    !c+++ set pres
    if (opin) then
      !c+++ read input pressure data
      call rgt_r4(jfile_p, imax, jmax, kmax, head2, pres, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_p, ios)
      if (.not. onoerr) then
        if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      endif
      !c+++ Pa ==> hPa 
      if (trim(head2(16)) == 'Pa') then
        call shift(imax, jmax, kmax, rmiss, pres, pres, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'pres(1,1,1) = ', pres(1,1,1), ' (hPa)'
      endif
    else
      !c+++ set pres by set_plevs
      call set_plevs(imax, jmax, kmax, rmiss, p, ps4, eta_fa, eta_fb, pres, osig, oeta)
    endif

    !c+++ airmass
    call mass(imax, jmax, kmax, rmiss, din, pres, lat, dout)

    !c+++ set unit of output data
    if (hunit /= 'NULL') head(16) = hunit
    !c+++ set item of output data
    if (hitem /= 'NULL') head(3) = hitem
    !c+++ set title of output data
    if (htitl /= 'NULL') then
      head(14) = htitl(1:16)
      head(15) = htitl(17:32)
    endif

    !c+++ write gtool3 header & data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo !! it
  !c+++ close files
  call gtclose(jfile_i, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  if (opin) call gtclose(jfile_p, ios)
  call gtclose(jfile_o, ios)
  !c+++ deallocate
  deallocate(din, dout, ps, ps4, lat, p, pres)
  deallocate(eta_fa, eta_fb)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine mass
!c
!c=====
subroutine mass(imax, jmax, kmax, rmiss, ndm, pres, lat, dout)
  use common_airmass, only: a, h, NA, mair
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r8b), intent(in)    :: ndm(imax,jmax,kmax)  !! (num/cm3)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! (hPa)
  real(kind=r8b), intent(in)    :: lat(jmax)            !! lat (deg)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(imax,jmax,kmax) !! (kg/m2)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, k1, k2
  real(kind=r8b)                :: p(imax,jmax,0:kmax+1) !! p (hPa)
  real(kind=r8b)                :: phi(jmax)             !! lat (rad)
  real(kind=r8b)                :: xlatm(jmax+1)         !! meridional length (m)
  real(kind=r8b)                :: area                  !! (m2)
  real(kind=r8b)                :: height                !! (m)
  real(kind=r8b)                :: pi                    !!
  pi = 4.d0 * datan(1.d0)

  p(1:imax,1:jmax,1:kmax) = pres(1:imax,1:jmax,1:kmax)
  p(1:imax,1:jmax,0)      = 1000.d0
  p(1:imax,1:jmax,kmax+1) = 1.d-10
  !c+++ prepare
  do j = 1, jmax
    phi(j) = lat(j) * pi / 180.d0
  enddo !! j
  xlatm(1) = 90.d0 * pi / 180.d0
  xlatm(jmax+1) = -90.d0 * pi / 180.d0
  do j = 1, jmax-1
    xlatm(j+1) = 0.5d0 * (phi(j) + phi(j+1))
  enddo !! j

  do k = 1, kmax
    k1 = k - 1
    k2 = k + 1
    do j = 1, jmax
      area = 2.d0 * pi * a**2 / dble(imax) * &
&            abs(0.5d0 * (cos(xlatm(j)) + cos(xlatm(j+1))) * (xlatm(j) - xlatm(j+1)))
      do i = 1, imax
        if (ndm(i,j,k) /= rmiss) then
          !c+++ height = (z(k+1)+z(k))/2 - (z(k)+z(k-1))/2
          height = - 0.5d0 * h * (log(p(i,j,k2)/1000.d0) - log(p(i,j,k1)/1000.d0))
          !ccc height = - 0.5d0 * h * (log(pres(i,j,k2)/1000.d0) - log(pres(i,j,k1)/1000.d0))

          !c+++  airmass [kg/m2] = air [num/cm3] * 10^6 / Na[num/mol] * mair [kg/mol]
          !c+++                  * area [m^2] * height [m]
          dout(i,j,k) = ndm(i,j,k) * 1.d6 / NA * mair &
&                     * area * height
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine mass

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
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('p', 'NONE', ip, ios)
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
  !c+++ noerr
  call get_parms('noerr', 'f', hval, ios)
  call c2var(onoerr, '(l1)', hval)
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
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtairmassv -o output-file'
  write(6, '(a)') '-i input-file'
  write(6, '(a)') '-ps input-ps-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-noerr f/t (disable header time check, default: f)'
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
  write(6, '(a)') 'ex. Total mass for p-levels: '
  write(6, '(a)') '    gtairmassv -i air_P -o airmass'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtairmassv
