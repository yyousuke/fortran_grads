!c
!c  program gtepfluxwn
!c  [history]
!c  2009/09/17 Yamashita: first ver.
!c  2009/09/23 Yamashita: for JRA25
!c  2013/02/07 Yamashita: for ERA-interim
!c  2013/03/29 Yamashita: add common_epf
!c  2013/03/29 Yamashita: move subroutines flux_pr, n2out, and omg2w to util_epf.f90
!c  2013/03/31 Yamashita: read/write gtool3 data
!c  2013/10/18 Yamashita: modify gtopen and remove seek_iounit
!c  2014/08/20 Yamashita: wavenumber components
!c  2016/11/25 Yamashita: bug fix of readopt
!c  2016/11/25 Yamashita: add Pa ==> hPa conversion
!c  2021/08/18 Yamashita: modify getparms & I/O 
!c  2021/08/19 Yamashita: use setup_cnst
!c  2021/08/22 Yamashita: use read_opt of uopts
!c
!c  MAD p128 (3.5.2a), (3.5.3) 
!c  for primitive EQ. epf=rho0*cos,epfdiv=/(cos*rho0)
!c  Using [dp/dz=-rho0(z)*g=-P/H] 
!c 
!c  Brunt Vaisalla frequency N^2
!c   N^2 = -P*RAIR/H^2*(dT/dp) + RAIR*KAPPA/H^2*(T)
!c   RAIR = 287.04 [J/kg/K], H = 6950 [m], KAPPA = 0.286
!c    => RAIR/H^2 = 0.0000059425496 [J/kg/K/m2], RAIR*KAPPA/H^2 = 0.000001699569 [J/kg/K/m2]
!c
!c  (old: RAIR = 287.04 [J/kg/K], H = 7000 [m], KAPPA = 0.286
!c        => RAIR/H^2 = 0.00000585795 [J/kg/K/m2], RAIR*KAPPA/H^2 = 0.00000167537 [J/kg/K/m2])
!c
!c  Basic air density (rho_0) 
!c   rho_0 = rho_s*exp(-z/H), where z is log-P height, z = -H*ln(P/P_s).
!c   P_s = rho_s*R*T_s, where H = R*T_s/g
!c     ==> rho_0 = rho_s*P/P_s = P/R/T_s = P/(gH)
!c    g = 9.81 [m/s^2], H = 6950 [m] => 1/(gH) = 0.000014667165 [s^2/m^2]
!c
!c  (old: g = 9.8 [m/s^2], H = 7000 [m] => 1/(gH) = 0.00001457725 [s^2/m^2])
!c
!c
!c  internal: module common_epf
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine readopt
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module common_typedef
!c  external: module util_epf
!c            subroutine omg2w, n2out, flux_pr, conv_fourier
!c  external: module util
!c            subroutines conv_zaxis, get_axname, getyaxis, getzaxis
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgt,
!c            get_zaxsize, get_hdtime
!c  external: module calculate
!c            subroutines mlt
!c  external: module dcalculate
!c            subroutines dshift
!c  external: module eddy
!c            subroutines eddyz
!c  external: module zmean
!c            subroutines ZONALmean
!c  external: module zmean2s
!c            subroutines ZONALmean2d
!c  external: error_handler
!c            subroutine ioerror, werr, werr2
!c
!c=======================================================================c
module common_epf
  use common_typedef, only: i4b, r8b
  use common_const, only: cnst_eradius, cnst_cp, cnst_egrav, cnst_h, cnst_rair, cnst_kappa, const_setup

  integer(kind=i4b), parameter :: nv = 10      !! number of output variables

  real(kind=r8b), save         :: a            !! equatorial radius (m)
  real(kind=r8b), save         :: cp           !! specific heat at constant pressure of air (J/K/kg)
  real(kind=r8b), save         :: g            !! grav. acceleration of the Earth (m/s2)
  real(kind=r8b), save         :: h            !! scale height [m]
  real(kind=r8b), save         :: rd           !! gas constant of dry air (J/K/kg)
  real(kind=r8b), save         :: rkappa       !! Rair / Cp

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    a = cnst_eradius
    cp = cnst_cp
    g = cnst_egrav
    h = cnst_h
    rd = cnst_rair
    rkappa = cnst_kappa
  end subroutine setup_cnst
!c----------------------------------------------------------------------c
end module common_epf

!c=====================================================================c

program gtepfluxwn
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_epf,  only: nv, setup_cnst, a, cp, g, h, rd, rkappa
  use util_epf, only: omg2w, n2out, flux_pr, conv_fourier
  use util, only: conv_zaxis, get_axname, getyaxis, getzaxis
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgt, get_zaxsize, get_hdtime
  use calculate, only: mlt
  use dcalculate, only: dshift
  use eddy, only: eddyz
  use zmean, only: ZONALmean
  use zmean2d, only: ZONALmean2d
  use error_handler, only: ioerror, werr, werr2
  implicit none
  !c+++ [input]
  !c+++ input data
  character(len=ncc)          :: head(ndc)   !! header
  character(len=ncc)          :: head2(ndc)  !! header for check
  real(kind=r8b), allocatable :: u0(:,:,:)   !! zonal wind (m/s)
  real(kind=r8b), allocatable :: v0(:,:,:)   !! meridional wind (m/s)
  real(kind=r8b), allocatable :: t0(:,:,:)   !! temperature (K)
  real(kind=r8b), allocatable :: omg0(:,:,:) !! omega (hPa/s)
  real(kind=r8b), allocatable :: ps0(:,:)    !! Ps (hPa)
  !c+++ input data or internal variable (depend on option)
  real(kind=r8b), allocatable :: uv0(:,:,:)  !! eddy u'v' (m2/s2)
  real(kind=r8b), allocatable :: vt0(:,:,:)  !! eddy v't' (Km/s)
  real(kind=r8b), allocatable :: uomg0(:,:,:)!! eddy u'omg' (hPa m/s2)
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax        !! x- axis size
  integer(kind=i4b)           :: jmax        !! y- axis size
  integer(kind=i4b)           :: kmax        !! z- axis size for p-levs.
  integer(kind=i4b)           :: kmax0       !! z- axis size for sig-/eta-levs.
  character(len=ncc)          :: haxisx      !! x- axis names
  character(len=ncc)          :: haxisy      !! y- axis names
  character(len=ncc)          :: haxisz      !! z- axis names
  real(kind=r8b)              :: rmiss       !! missing value
  character(len=ncc)          :: hdset       !! dataset name
  character(len=ncc)          :: htunit      !! unit of time
  real(kind=r8b)              :: time        !! time
  real(kind=r8b)              :: tdur        !! time step
  integer(kind=i4b)           :: jdate(6)    !! time array (year,mon,day,hour,minute,second)
  !c+++ input from getparms
  integer(kind=i4b)           :: ista, iend  !! start/end record
  character(len=nfiln)        :: iu          !! input zonal wind file name
  character(len=nfiln)        :: iv          !! input meridional wind file name
  character(len=nfiln)        :: itemp       !! input temperature file name
  character(len=nfiln)        :: iomg        !! input omega file name
  character(len=nfiln)        :: ips         !! input ps file name
  character(len=ncc)          :: ipout       !! output z-axis name
  character(len=ncc)          :: itype       !! intplt type (l: linear, s: spline)
  logical                     :: omiss       !! t: enable extrapolation, f: disable
  character(len=nfiln)        :: iuv         !! input u'v' file
  character(len=nfiln)        :: ivt         !! input v't' file
  character(len=nfiln)        :: iuomg       !! input u'omg' file
  character(len=nfiln)        :: ofile(nv)   !! output file names
  character(len=ncc)          :: hitem(nv)   !! output file items
  character(len=ncc*2)        :: htitl(nv)   !! output file titles
  character(len=ncc)          :: hunit(nv)   !! output file units
  character(len=ncc)          :: hdfmt       !! data format
  character(len=ncc)          :: zax         !! zaxis plev/siglv/etalv
  logical                     :: oapnd       !! t: append, f: replace
  integer(kind=i4b)           :: iwn         !! wave number
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: d4(:,:,:)   !!
  real(kind=r8b), allocatable :: d8(:,:,:)   !!
  !c d(1): epfy (kg/m/s2), d(2): epfz (kg/m/s2), d(3): epfdiv (m/s2)
  !c d(4): epfy0y (m/s2), d(5): epfz0z (m/s2)
  !c d(6): epfy2 (kg/s2), d(7): epfz2 (kg/s2), d(8): epfdiv2 (kg/m/s2)
  !c d(9): epfy0y2 (kg/m/s2), d(10): epfz0z2 (kg/m/s2)
  !c+++ [internal work]
  integer(kind=i4b)           :: j, k        !! loop variables (lat, lev)
  integer(kind=i4b)           :: it, ivar    !! loop variables (time, var)
  real(kind=r8b)              :: pi          !! pi
  real(kind=r8b)              :: omge        !! angular velocity omge (rad/sec)
  !c+++ 3-D data
  real(kind=r4b), allocatable :: u(:,:,:)    !! zonal wind (m/s)
  real(kind=r4b), allocatable :: v(:,:,:)    !! meridional wind (m/s)
  real(kind=r4b), allocatable :: t(:,:,:)    !! temperature (K)
  real(kind=r4b), allocatable :: omg(:,:,:)  !! omega (hPa/s)
  !c+++ conversion pf input data or internal variable (depend on option)
  real(kind=r4b), allocatable :: uv(:,:,:)   !! eddy u'v' (m2/s2)
  real(kind=r4b), allocatable :: vt(:,:,:)   !! eddy v't' (Km/s)
  real(kind=r4b), allocatable :: uomg(:,:,:) !! eddy u'omg' (hPa m/s2)
  !c+++ converted 3-D data
  real(kind=r4b), allocatable :: w(:,:,:)    !! vertical wind (m/s)
  real(kind=r4b), allocatable :: uw(:,:,:)   !! eddy u'w' (m2/s2)
  !c+++ zonal mean data
  real(kind=r4b), allocatable :: ulon(:,:)   !! [u] (m/s)
  real(kind=r4b), allocatable :: vlon(:,:)   !! [v] (m/s)
  real(kind=r4b), allocatable :: tlon(:,:)   !! [t] (K)
  real(kind=r4b), allocatable :: wlon(:,:)   !! [w] (m/s)
  real(kind=r4b), allocatable :: n2(:,:)     !! [N^2] (1/s2)
  real(kind=r4b), allocatable :: rho0(:,:)   !! [rho] (kg/m3)
  !c+++ y-, z- axis
  real(kind=r8b), allocatable :: p(:)        !! p(hPa)
  real(kind=r8b), allocatable :: lat(:)      !! latitude (deg)
  real(kind=r8b), allocatable :: phi(:)      !! latitude (rad)
  real(kind=r8b), allocatable :: f(:)        !! Coriolis parameter (1/s)
  real(kind=r8b), allocatable :: cs(:)       !! cosine of latitude ()
  !c+++ I/O unit for input/output files
  integer(kind=i4b) :: jfile_u               !! for zonal wind file
  integer(kind=i4b) :: jfile_v               !! for meridional wind file
  integer(kind=i4b) :: jfile_t               !! for temperature file
  integer(kind=i4b) :: jfile_omg             !! for omega file
  integer(kind=i4b) :: jfile_ps              !! for ps file
  integer(kind=i4b) :: jfile_uv              !! for u'v' file
  integer(kind=i4b) :: jfile_vt              !! for v't' file
  integer(kind=i4b) :: jfile_uomg            !! for u'omg' file
  integer(kind=i4b) :: jfile_o(nv)           !! for output files
  integer(kind=i4b) :: ios                   !! end code
  !c+++ [internal switch]
  logical :: osig = .false.                  !! enable sigma-lev.
  logical :: oeta = .false.                  !! enable eta-lev.
  logical :: oruv = .false.                  !! read uv file
  logical :: orvt = .false.                  !! read vt file
  logical :: oruo = .false.                  !! read uomg file
  logical :: orps = .false.                  !! read ps file

!c
!c prepare
!c===
  pi = acos(-1.d0)
  omge = 2.d0 * pi / 86400.d0 !! angular velocity omge (rad/sec)
  !c+++ read parameters
  call getparms
  !c+++ set switches
  if (zax == 'siglv')  osig = .true.
  if (zax == 'etalv')  oeta = .true.
  if (iuv /= 'NULL')   oruv = .true.
  if (ivt /= 'NULL')   orvt = .true.
  if (iuomg /= 'NULL') oruo = .true.
  if (osig.or.oeta) then
    orps = .true.
    if (ipout == 'NULL') call werr2('must be specified -pout option')
  endif

  !c+++ constants
  call setup_cnst

!c
!c open
!c===
  !c+++ open input zonal wind file
  write(6, '(2a)') 'open input zonal wind file: ', trim(iu)
  call gtopen(trim(iu), 'r', jfile_u, ios)
  if (ios /= 0) call ioerror(jfile_u, ios)

  !c+++ open input meridional wind file
  write(6, '(2a)') 'open input meridional wind file: ', trim(iv)
  call gtopen(trim(iv), 'r', jfile_v, ios)
  if (ios /= 0) call ioerror(jfile_v, ios)

  !c+++ open input temperature file
  write(6, '(2a)') 'open input temperature file: ', trim(itemp)
  call gtopen(trim(itemp), 'r', jfile_t, ios)
  if (ios /= 0) call ioerror(jfile_t, ios)

  !c+++ open input vertical wind file
  write(6, '(2a)') 'open input vertical wind file: ', trim(iomg)
  call gtopen(trim(iomg), 'r', jfile_omg, ios)
  if (ios /= 0) call ioerror(jfile_omg, ios)

  !c+++ open input eddy u'v' file
  if (oruv) then
    write(6, '(2a)') 'open input eddy uv file: ', trim(iuv)
    call gtopen(trim(iuv), 'r', jfile_uv, ios)
    if (ios /= 0) call ioerror(jfile_uv, ios)
  endif

  !c+++ open input eddy v't' file
  if (orvt) then
    write(6, '(2a)') 'open input eddy vt file: ', trim(ivt)
    call gtopen(trim(ivt), 'r', jfile_vt, ios)
    if (ios /= 0) call ioerror(jfile_vt, ios)
  endif

  !c+++ open input eddy u'omg' file
  if (oruo) then
    write(6, '(2a)') 'open input eddy uomg file: ', trim(iuomg)
    call gtopen(trim(iuomg), 'r', jfile_uomg, ios)
    if (ios /= 0) call ioerror(jfile_uomg, ios)
  endif

  !c+++ open input surface pressure file
  if (orps) then
    write(6, '(2a)') 'open input surface pressure file: ', trim(ips)
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
  endif

  !c+++ open output files
  do ivar = 1, nv
    write(6, '(2a)') 'open output file: ', trim(ofile(ivar))
    if (oapnd) then
      call gtopen(trim(ofile(ivar)), 'a', jfile_o(ivar), ios)
    else
      call gtopen(trim(ofile(ivar)), 'w', jfile_o(ivar), ios)
    endif
    if (ios /= 0) call ioerror(jfile_o(ivar), ios)
  enddo !! ivar

!c
!c axis
!c===
  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_u, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_u, ios)
  kmax0 = kmax
  call gtrewind(jfile_u, ios)
  !c+++ x- y- z-axis names from gtool header
  call get_axname(haxisx, haxisy, haxisz, head)
  !c+++ for sig- and eta-levs
  if (osig.or.oeta) then
    call get_zaxsize(ipout, kmax) !! replace kmax by z-axis size of ipout
    write(6, *) 'kmax0 = ', kmax0
  else
    ipout = haxisz !! set output z-axis name
  endif
  !c+++ set dataset name
  hdset = head(2)
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  write(6, *) 'hdset = ', trim(hdset)
  write(6, *) 'hdfmt = ', trim(hdfmt)

  !c+++ allocate
  !c+++ for input
  allocate(u0(imax,jmax,kmax0), v0(imax,jmax,kmax0), t0(imax,jmax,kmax0))
  allocate(omg0(imax,jmax,kmax0), ps0(imax,jmax))
  allocate(uv0(imax,jmax,kmax0), vt0(imax,jmax,kmax0), uomg0(imax,jmax,kmax0))
  !c+++ for 3-D variables
  allocate(u(imax,jmax,kmax), v(imax,jmax,kmax), t(imax,jmax,kmax))
  allocate(omg(imax,jmax,kmax))
  allocate(uv(imax,jmax,kmax), vt(imax,jmax,kmax), uomg(imax,jmax,kmax))
  allocate(w(imax,jmax,kmax), uw(imax,jmax,kmax))
  !c+++ for zonal mean variables
  allocate(ulon(jmax,kmax), vlon(jmax,kmax), tlon(jmax,kmax), wlon(jmax,kmax))
  allocate(n2(jmax,kmax), rho0(jmax,kmax))
  !c+++ for axis data
  allocate(p(kmax), lat(jmax), f(jmax), phi(jmax), cs(jmax))

  !c+++ get y-axis data
  call getyaxis(jmax, haxisy, lat)
  write(6, *) 'lat = ', lat
  !c+++ read z-axis file (set p-levs for output)
  if (osig.or.oeta) then !! for sig- and eta-levs
    call getzaxis(kmax, ipout, p)
  else !! for plev
    call getzaxis(kmax, haxisz, p)
  endif
  write(6, *) 'output pressure lev = ', p

  do j = 1, jmax
    phi(j) = lat(j) * pi / 180.d0
    f(j) = 2.d0 * omge * sin(phi(j)) 
    cs(j) = cos(phi(j)) 
  enddo !! j
  !c+++ rho0(z)
  do k = 1, kmax
    rho0(1:jmax,k) = p(k) * 100.d0 / g / h 
  enddo !! k

!c
!c Time loop
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      if (orps) then
        call gtskip(jfile_ps, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_ps, ios)
      endif
      call gtskip(jfile_u, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_u, ios)
      call gtskip(jfile_v, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_v, ios)
      call gtskip(jfile_t, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_t, ios)
      call gtskip(jfile_omg, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_omg, ios)
      if (oruv) then
        call gtskip(jfile_uv, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_uv, ios)
      endif
      if (orvt) then
        call gtskip(jfile_vt, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_vt, ios)
      endif
      if (oruo) then
        call gtskip(jfile_uomg, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_uomg, ios)
      endif
      cycle
    endif

    !ccc write(6, *) 'epfdiv t=',it
    !c+++ read ps
    if (orps) then
      call rgt(jfile_ps, imax, jmax, 1, head2, ps0, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, 1, rmiss, ps0, ps0, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps0(1,1), ' (hPa)'
      endif
    endif

    !c+++ read u
    call rgt(jfile_u, imax, jmax, kmax0, head, u0, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_u, ios)
    if (orps) then
      if (head(27) /= head2(27)) call werr(head2(27), head(27), 'Dates are not match!')
    endif
    !c+++ u0 ==> u (interpolate or copy)
    if (osig.or.oeta) then !! for sig- and eta-levs
      call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, u0, u, &
&       itype, omiss, osig, oeta)
    else !! for p-levs
      u(1:imax,1:jmax,1:kmax) = u0(1:imax,1:jmax,1:kmax)
    endif
    !c+++ get time & unit from gtool header
    call get_hdtime(time, tdur, htunit, jdate, head)

    !c+++ read v
    call rgt(jfile_v, imax, jmax, kmax0, head2, v0, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_v, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
    !c+++ v0 ==> v (interpolate or copy)
    if (osig.or.oeta) then !! for sig- and eta-levs
      call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, v0, v, &
&       itype, omiss, osig, oeta)
    else !! for p-levs
      v(1:imax,1:jmax,1:kmax) = v0(1:imax,1:jmax,1:kmax)
    endif

    !c+++ read t
    call rgt(jfile_t, imax, jmax, kmax0, head2, t0, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_t, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
    !c+++ t0 ==> t (interpolate or copy)
    if (osig.or.oeta) then !! for sig- and eta-levs
      call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, t0, t, &
&       itype, omiss, osig, oeta)
    else !! for p-levs
      t(1:imax,1:jmax,1:kmax) = t0(1:imax,1:jmax,1:kmax)
    endif

    !c+++ read omg
    call rgt(jfile_omg, imax, jmax, kmax0, head2, omg0, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_omg, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
    !c+++ Pa ==> hPa
    if (trim(head2(16)) == 'Pa/s') then
      call dshift(imax, jmax, kmax, rmiss, omg0, omg0, 0.01d0, 0.d0)
      write(6, '(a, f9.4, a)') 'omg(1,1,1) = ', omg0(1,1,1), ' (hPa/s)'
    endif
    !c+++ omg0 ==> omg (interpolate or copy)
    if (osig.or.oeta) then !! for sig- and eta-levs
      call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, omg0, omg, &
&       itype, omiss, osig, oeta)
    else !! for p-levs
      omg(1:imax,1:jmax,1:kmax) = omg0(1:imax,1:jmax,1:kmax)
    endif

    !c+++ omg ==> w 
    call omg2w(imax, jmax, kmax, rmiss, omg, w, p, h)

    !c+++ zonal mean 
    call ZONALmean(imax, jmax, kmax, rmiss, u, ulon)
    call ZONALmean(imax, jmax, kmax, rmiss, v, vlon)
    call ZONALmean(imax, jmax, kmax, rmiss, t, tlon)
    call ZONALmean(imax, jmax, kmax, rmiss, w, wlon)
    !c+++ deviation from zonal mean 
    call eddyz(imax, jmax, kmax, rmiss, u, ulon, u)
    call eddyz(imax, jmax, kmax, rmiss, v, vlon, v)
    call eddyz(imax, jmax, kmax, rmiss, t, tlon, t)
    call eddyz(imax, jmax, kmax, rmiss, w, wlon, w)

    !c+++ n2
    call n2out(1, jmax, kmax, rmiss, tlon, n2, p, rd, h, rkappa)

    !c+++ fourier transform
    call conv_fourier(imax, jmax, kmax, iwn, rmiss, u)
    call conv_fourier(imax, jmax, kmax, iwn, rmiss, v)
    call conv_fourier(imax, jmax, kmax, iwn, rmiss, t)
    call conv_fourier(imax, jmax, kmax, iwn, rmiss, w)

    !c+++ u'v'
    if (oruv) then
      call rgt(jfile_uv, imax, jmax, kmax0, head2, uv0, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_uv, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ uv0 ==> uv (interpolate or copy)
      if (osig.or.oeta) then !! for sig- and eta-levs
        call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, uv0, uv, &
&         itype, omiss, osig, oeta)
      else !! for p-levs
        uv(1:imax,1:jmax,1:kmax) = uv0(1:imax,1:jmax,1:kmax)
      endif
    else
      !c+++ [u'v'] calculation
      call mlt(imax, jmax, kmax, rmiss, u, v, uv, 1.d0, 1.d0, 0.d0, 0.d0)
    endif

    !c+++ v't'
    if (orvt) then
      call rgt(jfile_vt, imax, jmax, kmax0, head2, vt0, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_vt, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ vt0 ==> vt (interpolate or copy)
      if (osig.or.oeta) then !! for sig- and eta-levs
        call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, vt0, vt, &
&         itype, omiss, osig, oeta)
      else !! for p-levs
        vt(1:imax,1:jmax,1:kmax) = vt0(1:imax,1:jmax,1:kmax)
      endif
    else
      !c+++ [v't'] calculation
      call mlt(imax, jmax, kmax, rmiss, v, t, vt, 1.d0, 1.d0, 0.d0, 0.d0)
    endif

    !c+++ u'w'
    if (oruo) then
      call rgt(jfile_uomg, imax, jmax, kmax0, head2, uomg0, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_uomg, ios)
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == '(m/s)(Pa/s)') then
        call dshift(imax, jmax, kmax, rmiss, uomg0, uomg0, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'uomg(1,1,1) = ', uomg0(1,1,1), ' (m/s)(hPa/s)'
      endif
      !c+++ uomg0 ==> uomg (interpolate or copy)
      if (osig.or.oeta) then !! for sig- and eta-levs
        call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, uomg0, uomg, &
&         itype, omiss, osig, oeta)
      else !! for p-levs
        uomg(1:imax,1:jmax,1:kmax) = uomg0(1:imax,1:jmax,1:kmax)
      endif
      !c+++ uomg ==> uw 
      call omg2w(imax, jmax, kmax0, rmiss, uomg, uw, p, h)
    else
      !c+++ [v'w'] calculation
      call mlt(imax, jmax, kmax, rmiss, u, w, uw, 1.d0, 1.d0, 0.d0, 0.d0)
    endif
  
    !c+++ E-P flux
    allocate(d4(jmax,kmax,nv), d8(jmax,kmax,nv))
    call flux_pr(imax, jmax, kmax, nv, rmiss, &
&     uv, vt, uw, ulon, n2, rho0, d4, &
&     lat, phi, f, cs, p, a, g, h, rd)
  
    !c+++ write
    d8(1:jmax,1:kmax,1:nv) = d4(1:jmax,1:kmax,1:nv)
    do ivar = 1, nv
      call wgt(jfile_o(ivar), 1, jmax, kmax, rmiss, d8(1:jmax,1:kmax,ivar), &
&       hitem(ivar), htitl(ivar), hunit(ivar), &
&       hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, ipout, ios)
      if (ios /= 0) call ioerror(jfile_o(ivar), ios)
    enddo !! ivar
    deallocate(d4, d8)

    if (it == iend) exit
  enddo !! it
  !c+++ deallocate
  deallocate(u0, v0, t0, omg0, ps0, uv0, vt0, uomg0)
  deallocate(u, v, t, omg, uv, vt, uomg, w, uw)
  deallocate(ulon, vlon, tlon, wlon, n2, rho0)
  deallocate(p, lat, f, phi, cs)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c
!c=====
subroutine getparms
  use uopts, only: read_parms, get_parms, get_strend, read_opt
  use char2var, only: c2var
  !c+++ [modify]
  !c+++ [internal work]
  character(len=ncc)            :: hval             !!
  character(len=nfiln*nv)       :: ofiles           !! for file names
  character(len=ncc*2*nv)       :: hdump            !! for titles and units
  character(len=ncc)            :: awn              !! wave number
  integer(kind=i4b)             :: ios              !! end code
  !c+++ init
  ofile(1:nv) = ' '
  hitem(1:nv) = ' '
  htitl(1:nv) = ' '
  hunit(1:nv) = ' '

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input files
  call get_parms('u', 'u_P', iu, ios) !! input u file
  call get_parms('v', 'v_P', iv, ios) !! input v file
  call get_parms('t', 'T_P', itemp, ios) !! input t file
  call get_parms('omg', 'omg_P', iomg, ios) !! input omg file
  call get_parms('ps', 'Ps', ips, ios) !! input Ps file
  call get_parms('uv', 'NULL', iuv, ios) !! input u'v' file
  call get_parms('vt', 'NULL', ivt, ios) !! input v't' file
  call get_parms('uomg', 'NULL', iuomg, ios) !! input u'omg' file
  !c+++ wavenumber
  call get_parms('wn', '1', hval, ios)
  call c2var(iwn, '(i16)', hval)
  !c+++ converted z-axis name
  call get_parms('pout', 'NULL', ipout, ios)
  !c+++ input intplt type
  call get_parms('pint', 'NULL', hval, ios)
  itype    = 's' !! defalut: s: spline
  if (hval == 'linear') itype = 'l' !! intplt type; l: linear
  if (hval == 'spline') itype = 's' !! intplt type; s: spline
  !c+++ input extrapolation type (t: enable extrapolation, f: disable)
  call get_parms('pintext', 'f', hval, ios)
  call c2var(omiss, '(l1)', hval)
  !c+++ output files
  call get_parms('o', 'epfy,epfz,epfdiv,epfy0y,epfz0z,epfy2,epfz2,epfdiv2,epfy0y2,epfz0z', ofiles, ios)
  call read_opt(nv, ofiles, ofile)
  !c+++ items
  call get_parms('item', &
& 'EPFY,EPFZ,EPFDIV,EPFY0Y,EPFZ0Z,EPFY,EPFZ,EPFDIV,EPFY0Y,EPFZ0Z', &
& hdump, ios)
  call read_opt(nv, hdump, hitem)
  !c+++ titles 
  call get_parms('titl', &
& 'EP flux meridional,EP flux vertical,EP flux divergence,EP flux div.  (y),'// &
& 'EP flux div. (z),EP flux meridional,EP flux vertical,EP flux divergence,'// &
& 'EP flux div. (y),EP flux div. (z)', &
& hdump, ios)
  call read_opt(nv, hdump, htitl)
  !c+++ units
  call get_parms('unit', &
& 'kg/m/s2,kg/m/s2,m/s2,m/s2,m/s2,kg/s2,kg/s2,kg/m/s2,kg/m/s2,kg/m/s2', &
& hdump, ios)
  call read_opt(nv, hdump, hunit)
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

  !c+++ write options
  do ivar = 1, nv
    ofile(ivar) = trim(ofile(ivar))//'_wn'//trim(awn)
    htitl(ivar) = trim(htitl(ivar))//'(WN'//trim(awn)//')'
    write(6, '(a, i2, 2a)') 'getparms: ofile(', ivar, ') = ', trim(ofile(ivar))
    write(6, '(a, i2, 2a)') 'getparms: hitem(', ivar, ') = ', trim(hitem(ivar))
    write(6, '(a, i2, 2a)') 'getparms: htitl(', ivar, ') = ', trim(htitl(ivar))
    write(6, '(a, i2, 2a)') 'getparms: hunit(', ivar, ') = ', trim(hunit(ivar))
  enddo !! ivar

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: gtepfluxwn'
  write(6, '(a)') '-o output-file1,output-file2,...'
  write(6, '(a)') '-item item-1,item-2,.. -titl title-1,title-2,... -unit unit-1,unit-2,...'
  write(6, '(a)') '-u input-u-file'
  write(6, '(a)') '-v input-v-file'
  write(6, '(a)') '-t input-t-file'
  write(6, '(a)') '-omg input-omg-file'
  write(6, '(a)') '(-ps input-ps-file, -pout output-z-axis-name)'
  write(6, '(a)') '(-pint linear/spline, -pintext t/f)'
  write(6, '(a)') '(-uv input-uv-file)'
  write(6, '(a)') '(-vt input-vt-file)'
  write(6, '(a)') '(-uomg input-uomg-file)'
  write(6, '(a)') ' '
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-wn wavenumber'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') ' '
  write(6, '(a)') '-zax plev/siglv/etalv'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-zax plev: assume p-levels (default), and ignore -ps, -pout options'
  write(6, '(a)') '-zax siglv: assume sigma-levels (convert to p-levs)'
  write(6, '(a)') '-zax etalv: assume eta-levels (convert to p-levs)'
  write(6, '(a)') 'siglv/etalv: must specify -pout option'
  write(6, '(a)') '-pint linear: linear interpolation'
  write(6, '(a)') '-pint spline: spline interpolation'
  write(6, '(a)') '-pintext t: enable extrapolation'
  write(6, '(a)') '-pintext f: disable'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'eddy flux terms: -uv, -vt, -uomg'
  write(6, '(a)') 'if these options are specified, inputted eddy flux terms are used,'
  write(6, '(a)') 'instead of calculation with -u, -v, -t, -omg.'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') 'E-P flux for zonal wavenumber 1'
  write(6, '(a)') 'ex. for p-levels: gtepflux -u u_P -v v_P -t T_P -omg omg_P -wn 1'
  write(6, '(a)') ' '
  write(6, '(a)') 'ex. for sigma-levels: gtepflux -u u -v v -t T -omg omg -wn 1'
  write(6, '(a)') '    -ps Ps -zax siglv -pout GPL31 -pint linear'
  write(6, '(a)') ' '
  write(6, '(a)') 'ex. for eta-levels: gtepflux -u u -v v -t T -omg omg -wn 1'
  write(6, '(a)') '    -ps Ps -zax etalv -pout GPL31 -pint spline'
  stop 2
end subroutine xabort

!c---------------------------------------------------------------------c

!c=====================================================================c

end program gtepfluxwn
