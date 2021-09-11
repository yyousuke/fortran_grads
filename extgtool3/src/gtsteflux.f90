!c
!c  program gtsteflux
!c  [history]
!c  2016/11/14 Yamashita: first ver.
!c  2016/11/18 Yamashita: -zmean option
!c  2016/11/22 Yamashita: -p and -zax zlev options
!c  2016/11/25 Yamashita: bug fix of readopt
!c  2017/02/27 Yamashita: options (-mval)
!c  2017/03/15 Yamashita: bug fix of ilevub
!c  2021/08/22 Yamashita: use setup_cnst
!c  2021/08/22 Yamashita: modify getparms & I/O
!c
!c  STE fluxを計算するプログラム
!c
!c  internal: module common_ste
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine readopt
!c    internal: subroutine xabort
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module common_typedef

!c  external: module util
!c            subroutines get_axname, getyaxis, getzaxis, set_plevs
!c  external: module util_steflux
!c            subroutine  conv_t2th, conv_th2t, pvseek2, trop_pv, trop_wmo, 
!c            trop_miroc, trop_tmin, trop_lrtpv
!c  external: module rwgtool
!c            subroutine gtopen, gtclose,  gtrewind, gtskip, rgthd, rgt_r4,
!c            wgt_r4, get_etacoef, get_hdtime
!c  external: module calculate
!c            subroutine shift
!c  external: module zmean
!c            subroutine ZONALmean
!c  eaternal: error_handler
!c            subroutine ioerror, werr, werr2
!c
!c=====================================================================c
module common_ste
  use common_typedef, only: i4b, r4b, r8b
  use common_const, only: cnst_eradius, cnst_cp, cnst_egrav, cnst_h, &
&   cnst_rair, cnst_kappa, cnst_PVU, const_setup

  !c+++ [parameter]
  integer(kind=i4b), parameter :: nvar = 11     !! number of output variables

  real(kind=r8b), save         :: ar           !! equatorial radius (m)
  real(kind=r8b), save         :: cp           !! specific heat at constant pressure of air (J/K/kg)
  real(kind=r8b), save         :: g            !! grav. acceleration of the Earth (m/s2)
  real(kind=r8b), save         :: h            !! scale height [m]
  real(kind=r8b), save         :: rd           !! gas constant of dry air (J/K/kg)
  real(kind=r8b), save         :: rkappa       !! Rair / Cp
  real(kind=r8b), save         :: PVU          !! 1 [PVU] = 1e-6 [K m2/s/kg]

  integer(kind=i4b), parameter   :: ithlev = 13
  real(kind=r4b)                 :: thlev(ithlev)  !! reference theta level
  data thlev / 280., 290., 300., 310., 320., 330., 340., 350., 360., 370., 380., 390., 400. /
!  integer(kind=i4b), parameter   :: ithlev = 25
!  real(kind=r4b)                 :: thlev(ithlev)  !! reference theta level
!  data thlev / 280., 285., 290., 295., 300., 305., 310., 315., 320., 325., &
!  &            330., 335., 340., 345., 350., 355., 360., 365., 370., 375., &
!  &            380., 385., 390., 395., 400. /
!

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    ar = cnst_eradius
    cp = cnst_cp
    g = cnst_egrav
    h = cnst_h
    rd = cnst_rair
    rkappa = cnst_kappa
    PVU = cnst_PVU
  end subroutine setup_cnst
!c----------------------------------------------------------------------c
end module common_ste

!c=====================================================================c

program gtsteflux
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_ste,  only: nvar, setup_cnst, ar, cp, g, h, rd, rkappa, PVU, ithlev, thlev
  use util, only: get_axname, getyaxis, getzaxis, set_plevs
  use util_steflux, only: conv_t2th, conv_th2t, pvseek2, trop_pv, trop_wmo, &
&   trop_miroc, trop_tmin, trop_lrtpv
  use rwgtool, only: gtopen, gtclose,  gtrewind, gtskip, rgthd, rgt_r4, wgt_r4, get_etacoef, get_hdtime
  use calculate, only: shift
  use zmean, only: ZONALmean
  use error_handler, only: ioerror, werr, werr2
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r4b), allocatable    :: uu(:,:,:)       !! u (m/s) 
  real(kind=r4b), allocatable    :: vv(:,:,:)       !! v (m/s)
  real(kind=r4b), allocatable    :: tt(:,:,:)       !! t (K)
  real(kind=r4b), allocatable    :: th(:,:,:)       !! theta (K)
  real(kind=r4b), allocatable    :: zz(:,:,:)       !! z (m) 
  real(kind=r4b), allocatable    :: qq(:,:,:)       !! q (K/s) 
  real(kind=r4b), allocatable    :: ps(:,:)         !! Ps (hPa)
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)             :: head(ndc)       !! gtool3 header
  character(len=ncc)             :: head2(ndc)      !! gtool3 header
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)              :: imax            !! x-axis sizes
  integer(kind=i4b)              :: jmax            !! y-axis sizes
  integer(kind=i4b)              :: kmax            !! z-axis sizes
  real(kind=r4b)                 :: rmiss           !! missing value
  character(len=ncc)             :: hdset           !! dataset name
  !c+++ input from get_hdtime
  character(len=ncc)             :: htunit          !! unit of time
  integer(kind=i4b)              :: jdate(6)        !! yy, mm, dd, hh, mn, ss
  real(kind=r8b)                 :: tdur            !! tdur = time - timeb
  real(kind=r8b)                 :: time            !! time (s)
  real(kind=r8b)                 :: timeb           !! previous time (s)
  !c+++ input from getparms
  integer(kind=i4b)              :: ista            !! start record
  integer(kind=i4b)              :: iend            !! end record
  character(len=nfiln)           :: iu              !! input zonal wind file name
  character(len=nfiln)           :: iv              !! input meridional wind file name
  character(len=nfiln)           :: ith             !! input potential temp. file name (or temp. file)
  character(len=nfiln)           :: iz              !! input geopotential height file name
  character(len=nfiln)           :: iq              !! input diabatic heating rate file name
  character(len=nfiln)           :: ip              !! input pressure file name
  character(len=nfiln)           :: ips             !! input ps file name
  character(len=nfiln)           :: ofile(nvar)     !! output file names
  character(len=ncc)             :: hitem(nvar)     !! output file items
  character(len=ncc*2)           :: htitl(nvar)     !! output file titles
  character(len=ncc)             :: hunit(nvar)     !! output file units
  character(len=ncc)             :: ztrop           !! trop. def. pv/wmo/miroc
  real(kind=r4b)                 :: mval            !! minimum value
  real(kind=r4b)                 :: levub           !! th-level of upper boundary (K)
  logical                        :: otinp           !! t: t-input , f: theta-input
  character(len=ncc)             :: hdfmt           !! data format
  character(len=ncc)             :: zax             !! zaxis plev/zlev/siglv/etalv
  logical                        :: ozm             !! t: zonal mean, f: original
  logical                        :: oapnd           !! t: append, f: replace
  !c+++ axis names (get_axname)
  character(len=ncc)             :: haxisx          !! x-axis names
  character(len=ncc)             :: haxisy          !! y-axis names
  character(len=ncc)             :: haxisz          !! z-axis names
  !c+++ y-, z- axis
  real(kind=r4b), allocatable    :: rlat(:)         !! latitude (rad)
  real(kind=r4b), allocatable    :: pres(:,:,:)     !! 3-D p(hPa)
  !c+++ y-, z- axis from file
  real(kind=r8b), allocatable    :: lat(:)          !! latutude (deg)
  real(kind=r8b), allocatable    :: p(:)            !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable    :: eta_fa(:)       !! for eta half lev.
  real(kind=r8b), allocatable    :: eta_fb(:)       !! for eta half lev.
  !c+++ [output]
  !c+++ output data (convine)
  real(kind=r4b), allocatable    :: dout(:,:,:)     !! output data
  !c+++ from pvseek2
  real(kind=r4b), allocatable    :: pv_th(:,:,:)    !! 温位面上のPV (K m2/s/kg)
  real(kind=r4b), allocatable    :: pv_thpv(:,:,:)  !! 温位面上のPV (PVU)
  real(kind=r4b), allocatable    :: p_th(:,:,:)     !! 温位に対応する気圧面(hPa)
  real(kind=r4b), allocatable    :: mf_th(:,:,:)    !! 温位に対応する気圧面の質量フラックス(kg/m2/s)
  real(kind=r4b), allocatable    :: t_th(:,:,:)     !! 温位に対応する気温(K)
  real(kind=r4b), allocatable    :: z_th(:,:,:)     !! 温位に対応するジオポテンシャル高度(m)
  !c+++ from trop_xxx
  integer(kind=i4b), allocatable :: kout(:,:)       !! 圏界面のgrid-id
  real(kind=r4b), allocatable    :: pout(:,:)       !! 圏界面の気圧p (hPa)
  real(kind=r4b), allocatable    :: tout(:,:)       !! 圏界面の気温T (K)
  real(kind=r4b), allocatable    :: zout(:,:)       !! 圏界面高度Z (m)
  !c+++ mass 
  real(kind=r4b), allocatable    :: mass(:,:)       !! mass of LMS (kg/m2)
  real(kind=r4b), allocatable    :: mass_prev(:,:)  !! mass of LMS (kg/m2) at previous time
  real(kind=r4b), allocatable    :: dmdt(:,:)       !! mass tendency dM/dt (kg/m2/s)
  real(kind=r4b), allocatable    :: ste(:,:)        !! STE flux (kg/m2/s)
  !c+++ for zonal mean (-zmean t )
  real(kind=r4b), allocatable    :: mf_th_zm(:,:)   !! 帯状平均 温位に対応する質量フラックス(kg/m2/s)
  !c+++ [work]
  integer(kind=i4b)              :: i, j, k         !! loop variables
  integer(kind=i4b)              :: it, ivar        !! loop variables
  integer(kind=i4b)              :: imax2           !! x-axis sizes for zonal mean calc.
  character(len=ncc*2)           :: htitlz          !! title
  real(kind=r8b)                 :: pi              !! pi
  integer(kind=i4b)              :: ilevub          !! level of upper boundary
  integer(kind=i4b)              :: ios             !! end code
  !c+++ I/O unit number
  integer(kind=i4b)              :: jfile_u         !! for zonal wind file (input)
  integer(kind=i4b)              :: jfile_v         !! for meridional wind file (input)
  integer(kind=i4b)              :: jfile_th        !! for potential temp. file (input)
  integer(kind=i4b)              :: jfile_z         !! for geopotential height file (input)
  integer(kind=i4b)              :: jfile_q         !! for diabatic heating rate file (input)
  integer(kind=i4b)              :: jfile_p         !! for pressure file (input)
  integer(kind=i4b)              :: jfile_ps        !! for ps file (input)
  integer(kind=i4b)              :: jfile_o(nvar)   !! for output file
  !c+++ for zonal mean (-zmean t )
  real(kind=r4b), allocatable    :: t_zm(:,:)       !! 帯状平均 t (K)
  real(kind=r4b), allocatable    :: z_zm(:,:)       !! 帯状平均 z (m)
  real(kind=r4b), allocatable    :: p_zm(:,:)       !! 帯状平均 p(hPa)
  real(kind=r4b), allocatable    :: pv_thpv_zm(:,:) !! 帯状平均 温位面上のPV (K m2/s/kg)
  real(kind=r4b), allocatable    :: p_th_zm(:,:)    !! 帯状平均 温位に対応する気圧面(hPa)
  real(kind=r4b), allocatable    :: t_th_zm(:,:)    !! 帯状平均 温位に対応する気温(K)
  real(kind=r4b), allocatable    :: z_th_zm(:,:)    !! 帯状平均 温位に対応するジオポテンシャル高度(m)
  !c+++ [internal switch]
  logical                        :: osig = .false.  !! enable sigma-lev.
  logical                        :: oeta = .false.  !! enable eta-lev.
  logical                        :: ozlv = .false.  !! enable z-lev.
  logical                        :: opin = .false.  !! enable pressure input
  logical                        :: oqin = .false.  !! enable diab. heat. input

!c
!c prepare
!c===
  pi = 4.d0 * atan(1.d0)
  time = 0.0
  timeb = 0.0
  !c+++ read parameters
  call getparms
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev' ) ozlv = .true.
  if (trim(ip) /= 'NONE') opin = .true.
  if (trim(iq) /= 'NONE') oqin = .true.
  if (ozlv.and. .not.opin) osig = .true. !! for Ps input
  ilevub = 1
  do k = 1, ithlev
    if (thlev(k) >= levub) then
      ilevub = k
      exit
    endif
  enddo !! k 
  write(6, *) 'ilevub = ', ilevub

  !c+++ constants
  call setup_cnst
  
  !c+++ open input zonal wind file
  write(6, *) 'open input zonal wind file: ', trim(iu)
  call gtopen(trim(iu), 'r', jfile_u, ios)
  if (ios /= 0) call ioerror(jfile_u, ios)

  !c+++ open input meridional wind file
  write(6, *) 'open input meridional wind file: ', trim(iv)
  call gtopen(trim(iv), 'r', jfile_v, ios)
  if (ios /= 0) call ioerror(jfile_v, ios)

  !c+++ open input potential temp. file
  if (otinp) then
    write(6, *) 'open input temp. file: ', trim(ith)
  else
    write(6, *) 'open input potential temp. file: ', trim(ith)
  endif
  call gtopen(trim(ith), 'r', jfile_th, ios)
  if (ios /= 0) call ioerror(jfile_th, ios)

  !c+++ open input geopotential height file
  write(6, *) 'open input geopotential height file: ', trim(iz)
  call gtopen(trim(iz), 'r', jfile_z, ios)
  if (ios /= 0) call ioerror(jfile_z, ios)

  !c+++ open input diabatic heating rate file
  if (oqin) then
    write(6, *) 'open input diabatic heating rate file: ', trim(iq)
    call gtopen(trim(iq), 'r', jfile_q, ios)
    if (ios /= 0) call ioerror(jfile_q, ios)
  endif

  !c+++ open input pressure file
  if (opin) then
    write(6, *) 'open input pressure file: ', trim(ip)
    call gtopen(trim(ip), 'r', jfile_p, ios)
    if (ios /= 0) call ioerror(jfile_p, ios)
  endif

  !c+++ open input surface pressure file
  if (osig.or.oeta) then
    write(6, *) 'open input surface pressure file: ', trim(ips)
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
  endif

  !c+++ open output file
  do ivar = 1, nvar
    write(6, '(4a)') 'open output file: ', trim(ofile(ivar)), ', title = ', trim(htitl(ivar))
    if (oapnd) then
      call gtopen(trim(ofile(ivar)), 'a', jfile_o(ivar), ios)
    else
      call gtopen(trim(ofile(ivar)), 'w', jfile_o(ivar), ios)
    endif
    if (ios /= 0) call ioerror(jfile_o(ivar), ios)
  enddo !! ivar

  !c+++ read gtool header
  call rgthd(jfile_u, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_u, ios)
  call gtrewind(jfile_u, ios)
  !c+++ set dataset name
  hdset = head(2)
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)
  !c+++ get axis names
  call get_axname(haxisx, haxisy, haxisz, head)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  write(6, *) 'hdset = ', trim(hdset)
  write(6, *) 'hdfmt = ', trim(hdfmt)

  !c+++ allocate
  allocate(uu(imax,jmax,kmax))
  allocate(vv(imax,jmax,kmax))
  allocate(tt(imax,jmax,kmax))
  allocate(th(imax,jmax,kmax))
  allocate(zz(imax,jmax,kmax))
  allocate(qq(imax,jmax,kmax))
  allocate(ps(imax,jmax))
  allocate(pv_th(imax,jmax,ithlev))
  allocate(pv_thpv(imax,jmax,ithlev))
  allocate(p_th(imax,jmax,ithlev))
  allocate(mf_th(imax,jmax,ithlev))
  allocate(t_th(imax,jmax,ithlev))
  allocate(z_th(imax,jmax,ithlev))
  allocate(mass(imax,jmax), mass_prev(imax,jmax))
  allocate(dmdt(imax,jmax))
  allocate(ste(imax,jmax))
  allocate(lat(jmax), rlat(jmax), p(kmax))
  allocate(pres(imax,jmax,kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
  mass(1:imax,1:jmax) = rmiss
  dmdt(1:imax,1:jmax) = rmiss
  ste(1:imax,1:jmax)  = rmiss
  ps(1:imax,1:jmax) = rmiss
  eta_fa(1:kmax) = rmiss
  eta_fb(1:kmax) = rmiss

  !c+++ get y-axis data
  call getyaxis(jmax, haxisy, lat)
  do j = 1, jmax
    rlat(j) = pi * lat(j) / 180.d0
  enddo !! j
  if (.not. oeta) then
    !c+++ read z-axis file (for p-lev & z-lev & sig-lev)
    call getzaxis(kmax, haxisz, p)
    !c+++ z ==> sig (for z-lev)
    if (ozlv) then
      !c+++ z = - h log(sig), -z/h = log(sig), sig = exp(-z/h)
      do k = 1, kmax
        p(k) = exp(- p(k) / h)
      enddo !! k
    endif
  else
    !c+++ read z-axis file (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  endif

!c
!c PV on theta
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_u, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_u, ios)
      call gtskip(jfile_v, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_v, ios)
      call gtskip(jfile_th, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_th, ios)
      call gtskip(jfile_z, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_z, ios)
      if (oqin) then
        call gtskip(jfile_q, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_q, ios)
      endif
      if (opin) then
        call gtskip(jfile_p, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_p, ios)
      endif
      if (osig.or.oeta) then
        call gtskip(jfile_ps, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_ps, ios)
      endif
      cycle
    endif

    !c+++ read input u data
    call rgt_r4(jfile_u, imax, jmax, kmax, head, uu, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_u, ios)

    !c+++ read input v data
    call rgt_r4(jfile_v, imax, jmax, kmax, head2, vv, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_v, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')

    !c+++ read input theta data
    if (otinp) then
      call rgt_r4(jfile_th, imax, jmax, kmax, head2, tt, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_th, ios)
    else
      call rgt_r4(jfile_th, imax, jmax, kmax, head2, th, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_th, ios)
    endif 
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')

    !c+++ read input geopotential height data
    call rgt_r4(jfile_z, imax, jmax, kmax, head2, zz, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_z, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')

    !c+++ read input diabatic heating rate data
    if (oqin) then
      call rgt_r4(jfile_q, imax, jmax, kmax, head2, qq, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_q, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
    else
      qq(1:imax,1:jmax,1:kmax) = rmiss
    endif

    !c+++ read input Ps data
    if (osig.or.oeta) then
      call rgt_r4(jfile_ps, imax, jmax, 1, head2, ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call shift(imax, jmax, 1, dble(rmiss), ps, ps, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps(1,1), ' (hPa)'
      endif
    endif

    !c+++ jdate ==> time (s)
    timeb = time
    !c+++ get time & unit from gtool header
    call get_hdtime(time, tdur, htunit, jdate, head)
    tdur = time - timeb
    write(6, '(a, e15.5, a)') 'tdur = ', tdur, ' (s)'

    !c+++ set pres
    if (opin) then
      !c+++ read input pressure data
      call rgt_r4(jfile_p, imax, jmax, kmax, head2, pres, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_p, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call shift(imax, jmax, kmax, dble(rmiss), pres, pres, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'pres(1,1,1) = ', pres(1,1,1), ' (hPa)'
      endif
    else
      !c+++ set pres by set_plevs
      call set_plevs(imax, jmax, kmax, dble(rmiss), p, ps, eta_fa, eta_fb, pres, osig, oeta)
    endif

    !c+++ T ==> TH
    if (otinp) then
      call conv_t2th(imax, jmax, kmax, rmiss, tt, pres, th, rkappa) !! tt ==> th
    else
      call conv_th2t(imax, jmax, kmax, rmiss, th, pres, tt, rkappa) !! th ==> tt
    endif

    !c+++ calculate PV, p, MF, t, and z on theta
    do k = 1, ithlev
      call pvseek2(imax, jmax, kmax, rmiss, uu, vv, tt, zz, th, thlev(k), pres, qq, rlat,  &
&       pv_th(1,1,k), pv_thpv(1,1,k), p_th(1,1,k), mf_th(1,1,k), t_th(1,1,k), z_th(1,1,k), &
&       g, ar, rkappa, PVU)
    enddo !! k

    if (ozm) then
      imax2 = 1
      !c+++ zonal mean case
      allocate(t_zm(jmax,kmax), z_zm(jmax,kmax), p_zm(jmax,kmax))
      allocate(pv_thpv_zm(jmax,ithlev), mf_th_zm(jmax,ithlev))
      allocate(p_th_zm(jmax,ithlev), t_th_zm(jmax,ithlev), z_th_zm(jmax,ithlev))
      allocate(kout(1,jmax), pout(1,jmax), tout(1,jmax), zout(1,jmax))
      !c+++ p-levs
      call ZONALmean(imax, jmax, kmax, dble(rmiss), tt, t_zm)
      call ZONALmean(imax, jmax, kmax, dble(rmiss), zz, z_zm)
      call ZONALmean(imax, jmax, kmax, dble(rmiss), pres, p_zm)
      !c+++ theta-levs
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), pv_thpv, pv_thpv_zm)
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), t_th, t_th_zm)
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), z_th, z_th_zm)
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), p_th, p_th_zm)
      !c+++ theta-levs for outputs
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), mf_th, mf_th_zm)
    else
      imax2 = imax
      !c+++ for original data
      allocate(kout(imax,jmax), pout(imax,jmax), tout(imax,jmax), zout(imax,jmax))
    endif

    !c+++ tropopause
    if (trim(ztrop) == 'pv'.or.trim(ztrop) == 'PV'.or.trim(ztrop) == 'pv2'.or.trim(ztrop) == 'PV2') then
      !c+++ 2 PVU
      if (ozm) then
        call trop_pv(1, jmax, ithlev, rmiss, 2.0, pv_thpv_zm, t_th_zm, z_th_zm, p_th_zm, pout, tout, zout)
      else
        call trop_pv(imax, jmax, ithlev, rmiss, 2.0, pv_thpv, t_th, z_th, p_th, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'pv2.5'.or.trim(ztrop) == 'PV2.5') then
      !c+++ 2.5 PVU
      if (ozm) then
        call trop_pv(1, jmax, ithlev, rmiss, 2.5, pv_thpv_zm, t_th_zm, z_th_zm, p_th_zm, pout, tout, zout)
      else
        call trop_pv(imax, jmax, ithlev, rmiss, 2.5, pv_thpv, t_th, z_th, p_th, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'pv3'.or.trim(ztrop) == 'PV3') then
      !c+++ 3 PVU
      if (ozm) then
        call trop_pv(1, jmax, ithlev, rmiss, 3.0, pv_thpv_zm, t_th_zm, z_th_zm, p_th_zm, pout, tout, zout)
      else
        call trop_pv(imax, jmax, ithlev, rmiss, 3.0, pv_thpv, t_th, z_th, p_th, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'pv3.5'.or.trim(ztrop) == 'PV3.5') then
      !c+++ 3.5 PVU
      if (ozm) then
        call trop_pv(1, jmax, ithlev, rmiss, 3.5, pv_thpv_zm, t_th_zm, z_th_zm, p_th_zm, pout, tout, zout)
      else
        call trop_pv(imax, jmax, ithlev, rmiss, 3.5, pv_thpv, t_th, z_th, p_th, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'pv4'.or.trim(ztrop) == 'PV4') then
      !c+++ 4 PVU
      if (ozm) then
        call trop_pv(1, jmax, ithlev, rmiss, 4.0, pv_thpv_zm, t_th_zm, z_th_zm, p_th_zm, pout, tout, zout)
      else
        call trop_pv(imax, jmax, ithlev, rmiss, 4.0, pv_thpv, t_th, z_th, p_th, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'wmo'.or.trim(ztrop) == 'WMO') then
      !c+++ LRT of WMO
      if (ozm) then
        call trop_wmo(1, jmax, kmax, rmiss, t_zm, z_zm, p_zm, kout, pout, tout, zout)
      else
        call trop_wmo(imax, jmax, kmax, rmiss, tt, zz, pres, kout, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'miroc'.or.trim(ztrop) == 'MIROC') then
      !c+++ LRT of MIROC
      if (ozm) then
        call trop_miroc(1, jmax, kmax, rmiss, t_zm, z_zm, p_zm, kout, pout, tout, zout)
      else
        call trop_miroc(imax, jmax, kmax, rmiss, tt, zz, pres, kout, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'tmin'.or.trim(ztrop) == 'TMIN') then
      !c+++ temperature minimum
      if (ozm) then
        call trop_tmin(1, jmax, kmax, rmiss, t_zm, z_zm, p_zm, kout, pout, tout, zout)
      else
        call trop_tmin(imax, jmax, kmax, rmiss, tt, zz, pres, kout, pout, tout, zout)
      endif
    else if (trim(ztrop) == 'lrtpv'.or.trim(ztrop) == 'LRTPV') then
      if (ozm) then
        call trop_lrtpv(1, jmax, kmax, ithlev, ilevub, rmiss, 3.5, &
&         pv_thpv_zm, t_th_zm, z_th_zm, p_th_zm, t_zm, z_zm, p_zm, kout, pout, tout, zout)
      else
        call trop_lrtpv(imax, jmax, kmax, ithlev, ilevub, rmiss, 3.5, &
&         pv_thpv, t_th, z_th, p_th, tt, zz, pres, kout, pout, tout, zout)
      endif
    else
      call werr2('in ztrop option')
    endif

    if (ozm) then
      p_th(1,1:jmax,ilevub) = p_th_zm(1:jmax,ilevub)
      t_th(1,1:jmax,ilevub) = t_th_zm(1:jmax,ilevub)
      z_th(1,1:jmax,ilevub) = z_th_zm(1:jmax,ilevub)
      mf_th(1,1:jmax,ilevub) = mf_th_zm(1:jmax,ilevub)
    endif
    mass_prev(1:imax,1:jmax) = mass(1:imax,1:jmax)
    do j = 1, jmax
      do i = 1, imax2
        !c+++ mass between P_trop and P_380K (kg/m2)
        if (pout(i,j) /= rmiss.and.p_th(i,j,ilevub) /= rmiss) then
          mass(i,j) = max((pout(i,j) - p_th(i,j,ilevub)) * 100.d0 / g, mval)
        else
          mass(i,j) = rmiss
        endif
        !c+++ dM/dt (kg/m2/s)
        if (mass(i,j) /= rmiss.and.mass_prev(i,j) /= rmiss.and.tdur > 0.0) then
          if (mass(i,j) >= mval.and.mass_prev(i,j) >= mval) then
            dmdt(i,j) = (mass(i,j) - mass_prev(i,j)) / dble(tdur)
          else 
            dmdt(i,j) = 0.0
          endif
        else
          dmdt(i,j) = rmiss
        endif
        !c+++ STE flux (kg/m2/s)
        if (dmdt(i,j) /= rmiss.and.mf_th(i,j,ilevub) /= rmiss) then
          ste(i,j) = dmdt(i,j) + mf_th(i,j,ilevub) 
        else
          ste(i,j) = rmiss
        endif
      enddo !! i
    enddo !! j

    !c+++ set output data
    allocate(dout(imax2,jmax,nvar))
    dout(1:imax2,1:jmax, 1) = mf_th(1:imax2,1:jmax,ilevub)   !! Mass flux at 380 K (upper)
    dout(1:imax2,1:jmax, 2) = pv_thpv(1:imax2,1:jmax,ilevub) !! PV (PVU) at 380 K (upper)
    dout(1:imax2,1:jmax, 3) = p_th(1:imax2,1:jmax,ilevub)    !! P at 380 K (upper)
    dout(1:imax2,1:jmax, 4) = t_th(1:imax2,1:jmax,ilevub)    !! T at 380K (upper)
    dout(1:imax2,1:jmax, 5) = z_th(1:imax2,1:jmax,ilevub)    !! Z at 380 K (upper)
    dout(1:imax2,1:jmax, 6) = pout(1:imax2,1:jmax)           !! P at tropopause (lower)
    dout(1:imax2,1:jmax, 7) = tout(1:imax2,1:jmax)           !! T at tropopause (lower)
    dout(1:imax2,1:jmax, 8) = zout(1:imax2,1:jmax)           !! Z at tropopause (lower)
    dout(1:imax2,1:jmax, 9) = mass(1:imax2,1:jmax)           !! mass between trop. and 380K
    dout(1:imax2,1:jmax,10) = dmdt(1:imax2,1:jmax)           !! mass tendency
    dout(1:imax2,1:jmax,11) = ste(1:imax2,1:jmax)            !! STE flux at trop.  (lower)

    !c+++ write output data
    do ivar = 1, nvar
      call wgt_r4(jfile_o(ivar), imax2, jmax, 1, dble(rmiss), dout(1:imax2,1:jmax,ivar), &
&       hitem(ivar), htitl(ivar), hunit(ivar), &
&       hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, 'SFC1', ios)
    enddo !! iv

    !c+++ deallocate
    if (ozm) deallocate(t_zm, z_zm, p_zm, pv_thpv_zm, mf_th_zm, p_th_zm, t_th_zm, z_th_zm)
    deallocate(kout, pout, tout, zout)
    deallocate(dout)
    if (it == iend) exit
  enddo !! it

  !c+++ close files
  call gtclose(jfile_u, ios)
  call gtclose(jfile_v, ios)
  call gtclose(jfile_th, ios)
  call gtclose(jfile_z, ios)
  call gtclose(jfile_q, ios)
  if (opin) call gtclose(jfile_p, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  do ivar = 1, nvar
    call gtclose(jfile_o(ivar), ios)
  enddo
  !c+++ deallocate
  deallocate(tt)
  deallocate(uu, vv, th, zz, qq, ps, pv_th, pv_thpv, p_th, t_th, z_th, mf_th)
  deallocate(mass, mass_prev, dmdt, ste)
  deallocate(lat, rlat, p, pres)
  deallocate(eta_fa, eta_fb)

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
  character(len=nfiln*nvar)     :: ofiles           !! for file names
  character(len=ncc*2*nvar)     :: hdump            !! for titles and units
  integer(kind=i4b)             :: ios              !! end code
  character(len=nfiln)          :: itmp             !! temp. file name
  character(len=ncc)            :: hlevs(nvar)      !! th-levels
  !c+++ init
  ofile(1:nvar) = ' '
  hitem(1:nvar) = ' '
  htitl(1:nvar) = ' '
  hunit(1:nvar) = ' '


  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input files
  call get_parms('u', 'u_P', iu, ios)       !! input u file
  call get_parms('v', 'v_P', iv, ios)       !! input v file
  call get_parms('th', 'THETA_P', ith, ios) !! input potential temp. file
  call get_parms('t', 'T_P', itmp, ios)     !! input temp. file
  call get_parms('z', 'z_P', iz, ios)       !! input geopotential height file name
  call get_parms('q', 'diab_P', iq, ios)    !! input diabatic heating rate file name
  call get_parms('p', 'NONE', ip, ios)      !! input pressure file name
  call get_parms('ps', 'Ps', ips, ios)      !! input Ps file
  !c+++ theta/temp
  otinp = .false. !! mode (th-inp)
  call get_parms('tinp', 'theta', hdump, ios)
  if (trim(hdump) == 'temp') otinp = .true. !! mode (t-inp)
  !c+++ output files
  call get_parms('o', &
& 'MF_380K,PV_380K,P_380K,T_380K,Z_380K,P_trop,T_trop,Z_trop,M_LMS,MT_LMS,STE_trop', &
& ofiles, ios)
  call read_opt(nvar, ofiles, ofile)
  !c+++ items
  call get_parms('item', 'Mass flux,PV,P,T,Z,P,T,Z,M,MT,STE', hdump, ios)
  call read_opt(nvar, hdump, hitem)
  !c+++ titles
  call get_parms('titl', &
& 'Mass flux on 380K AH96,PV on 380K,P on 380K,T on 380K,Z on 380K,P on trop.,'// &
& 'T on trop.,Z on trop.,Mass of atm. in LMS,Mass tend. in LMS,STE flux on trop.', &
& hdump, ios)
  call read_opt(nvar, hdump, htitl)
  !c+++ units
  call get_parms('unit', &
& 'kg/m2/s,PVU,hPa,K,m,hPa,K,m,kg/m2,kg/m2/s,kg/m2/s', hdump, ios)
  call read_opt(nvar, hdump, hunit)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ tropopause definition
  call get_parms('ztrop', 'pv', ztrop, ios)
  !c+++ minimum-value
  call get_parms('mval', '300.0', hval, ios)
  call c2var(mval, '(1pe15.5)', hval)
  !c+++ th-level of upper boundary
  call get_parms('levub', '380.0', hval, ios)
  call c2var(levub, '(1pe15.5)', hval)
  !c+++ zonal mean
  call get_parms('zmean', 'f', hval, ios)
  call c2var(ozm, '(l1)', hval)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  !c+++ axis
  call get_parms('zax', 'plev', zax, ios)
  if (ios /= 0) call xabort

! 'MF_380K,PV_380K,P_380K,T_380K,Z_380K,P_trop,T_trop,Z_trop,M_LMS,MT_LMS,STE_trop'
! 'Mass flux,PV,P,T,Z,P,T,Z,M,MT,STE'
! 'Mass flux on 380K AH96,PV on 380K,P on 380K,T on 380K,Z on 380K,P on trop.,T on trop.,Z on trop.,Mass of atm. in LMS,Mass tend. in LMS,STE flux on trop.'
! 'kg/m2/s,PVU,hPa,K,m,hPa,K,m,kg/m2,kg/m2/s,kg/m2/s'

  !c+++ 
! ofile( 1) = 'MF_380K'                !! output file name
! hitem( 1) = 'Mass flux'              !! item
! htitl( 1) = 'Mass flux on 380K AH96' !! title
! hunit( 1) = 'kg/m2/s'                !! unit
! !c+++
! ofile( 2) = 'PV_380K'                !! output file name
! hitem( 2) = 'PV'                     !! item
! htitl( 2) = 'PV on 380K'             !! title
! hunit( 2) = 'PVU'                    !! unit
! !c+++
! ofile( 3) = 'P_380K'                 !! output file name
! hitem( 3) = 'P'                      !! item
! htitl( 3) = 'P on 380K'              !! title
! hunit( 3) = 'hPa'                    !! unit
! !c+++
! ofile( 4) = 'T_380K'                 !! output file name
! hitem( 4) = 'T'                      !! item
! htitl( 4) = 'T on 380K'              !! title
! hunit( 4) = 'K'                      !! unit
! !c+++
! ofile( 5) = 'Z_380K'                 !! output file name
! hitem( 5) = 'Z'                      !! item
! htitl( 5) = 'Z on 380K'              !! title
! hunit( 5) = 'm'                      !! unit
! !c+++
! ofile( 6) = 'P_trop'                 !! output file name
! hitem( 6) = 'P'                      !! item
! htitl( 6) = 'P on trop.'             !! title
! hunit( 6) = 'hPa'                    !! unit
! !c+++
! ofile( 7) = 'T_trop'                 !! output file name
! hitem( 7) = 'T'                      !! item
! htitl( 7) = 'T on trop.'             !! title
! hunit( 7) = 'K'                      !! unit
! !c+++
! ofile( 8) = 'Z_trop'                 !! output file name
! hitem( 8) = 'Z'                      !! item
! htitl( 8) = 'Z on trop.'             !! title
! hunit( 8) = 'm'                      !! unit
! !c+++
! ofile( 9) = 'M_LMS'                  !! output file name
! hitem( 9) = 'M'                      !! item
! htitl( 9) = 'Mass of atm. in LMS'    !! title
! hunit( 9) = 'kg/m2'                  !! unit
! !c+++
! ofile(10) = 'MT_LMS'                 !! output file name
! hitem(10) = 'MT'                     !! item
! htitl(10) = 'Mass tend. in LMS'      !! title
! hunit(10) = 'kg/m2/s'                !! unit
! !c+++
! ofile(11) = 'STE_trop'               !! output file name
! hitem(11) = 'STE'                    !! item
! htitl(11) = 'STE flux on trop.'      !! title
! hunit(11) = 'kg/m2/s'                !! unit

  if (otinp) then
    write(6, '(2a)') 'getparms: itmp = ', trim(itmp)
    ith = itmp
  else
    write(6, '(2a)') 'getparms: ith = ', trim(ith)
  endif
  do ivar = 1, nvar
    if (ozm) then
      ofile(ivar) = trim(ofile(ivar))//'_x0'
    endif
    write(6, '(a, i2, 2a)') 'getparms: ofile(', ivar, ') = ', trim(ofile(ivar))
    write(6, '(a, i2, 2a)') 'getparms: hitem(', ivar, ') = ', trim(hitem(ivar))
    write(6, '(a, i2, 2a)') 'getparms: htitl(', ivar, ') = ', trim(htitl(ivar))
    write(6, '(a, i2, 2a)') 'getparms: hunit(', ivar, ') = ', trim(hunit(ivar))
  enddo !! ivar

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: gtsteflux '
  write(6, '(a)') '-o output-MF_UB-file,PV_UB-file,P_UB-file,T_UB-file,Z_UB-file,'
  write(6, '(a)') '   P_LB-file,T_LB-file,Z_LB-file,M_LMS-file,MT_LMS-file,STE_LB-file'
  write(6, '(a)') '-item item-1,item-2,.. -titl title-1,title-2,... -unit unit-1,unit-2,...'
  write(6, '(a)') ' '
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') ' '
  write(6, '(a)') '-u input-u-file (zonal wind)'
  write(6, '(a)') '-v input-v-file (meridional wind)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-tinp theta/temp' 
  write(6, '(a)') '-th input-theta-file -tinp theta (potential temperature)' 
  write(6, '(a)') ' or' 
  write(6, '(a)') '-t input-t-file -tinp temp (temperature)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '-z input-z-file (geopotential height)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-q input-q-file (diabatic heating rate)'
  write(6, '(a)') ' (caution: -q NONE, output MF=0)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-p input-p-file (pressure, optional)'
  write(6, '(a)') '  (default, NONE)'
  write(6, '(a)') '-ps input-ps-file (surface pressure, optional)'
  write(6, '(a)') '  (default, Ps)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-ztrop pv/wmo/miroc/tmin/pv2/pv2.5/pv3/pv3.5/pv4/lrtpv'
  write(6, '(a)') 'pv: 2 PVU potential vorticity surface (default)'
  write(6, '(a)') 'wmo: WMO definition, miroc: in MIROC radiation'
  write(6, '(a)') 'tmin: temperature minimum'
  write(6, '(a)') 'pv2/2.5/3/3.5/4: 2/2.5/3/3.5/4 PVU potential vorticity surface'
  write(6, '(a)') 'lrtpv 3.5 PVU < LRT of WMO < 380 K (Schoeberl, 2004)'
  write(6, '(a)') '(used as lower boundary of lowermost stratosphere)'
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
  write(6, '(a)') '-mval 300.0 (kg/m2, minimum-value of M_LMS)'
  write(6, '(a)') '-levub 380.0 (K, theta-level of upper boundary)'
  write(6, '(a)') '-zmean f/t (zonal mean calculation)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') '-apnd f/t'
  stop 2
end subroutine xabort

!c---------------------------------------------------------------------c

!c=====================================================================c

end program gtsteflux
