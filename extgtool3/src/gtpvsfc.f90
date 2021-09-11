!c
!c  program gtpvsfc
!c  [history]
!c  2017/03/15 Yamashita: first ver. (from gtsteflux 2017/02/27)
!c  2021/08/19 Yamashita: modify getparms & I/O
!c  2021/08/21 Yamashita: use setup_cnst
!c  2021/08/22 Yamashita: use read_opt of uopts
!c
!c  指定した温位面のPVを計算するプログラム
!c
!c  internal: module common_pv
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine readopt
!c    internal: subroutine xabort
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  internal: subroutine putzaxis
!c  external: module common_args
!c  external: module common_typedef
!c  external: module util
!c            subroutines get_axname, getyaxis, getzaxis, set_plevs
!c  external: module util_steflux
!c            subroutine  conv_t2th, conv_th2t, pvseek2
!c  external: module rwgtool
!c            subroutine gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, 
!c            wgt_r4, get_etacoef, get_hdtime
!c  external: module calculate
!c            subroutine shift
!c  external: module zmean
!c            subroutine ZONALmean
!c  eaternal: error_handler
!c            subroutine ioerror, werr, werr2
!c
!c=====================================================================c
module common_pv
  use common_typedef, only: i4b, r4b, r8b
  use common_const, only: cnst_eradius, cnst_cp, cnst_egrav, cnst_h, &
&   cnst_rair, cnst_kappa, cnst_PVU, const_setup

  !c+++ [parameter]
  integer(kind=i4b), parameter :: nvar = 3      !! number of output variables

  real(kind=r8b), save         :: ar           !! equatorial radius (m)
  real(kind=r8b), save         :: cp           !! specific heat at constant pressure of air (J/K/kg)
  real(kind=r8b), save         :: g            !! grav. acceleration of the Earth (m/s2)
  real(kind=r8b), save         :: h            !! scale height [m]
  real(kind=r8b), save         :: rd           !! gas constant of dry air (J/K/kg)
  real(kind=r8b), save         :: rkappa       !! Rair / Cp
  real(kind=r8b), save         :: PVU          !! 1 [PVU] = 1e-6 [K m2/s/kg]
! real(kind=r8b), parameter :: ar = 6.370d6     !! equatorial radius (m)
! real(kind=r8b), parameter :: cp = 1004.d0     !! specific heat at constant pressure of air (J/K/kg)
! real(kind=r8b), parameter :: g = 9.81d0       !! gravitational acceleration (m/s2)
! real(kind=r8b), parameter :: h = 6950.d0      !! scale height H(m)
! real(kind=r8b), parameter :: rd = 287.04d0    !! gas constant of dry air (J/K/kg)
! real(kind=r8b), parameter :: rkappa = rd / cp
! real(kind=r8b), parameter :: PVU  = 1d6       !! 1 [PVU] = 1e-6 [K m2/s/kg]

  integer(kind=i4b), parameter   :: ithlev = 13
  real(kind=r4b)                 :: thlev(ithlev)  !! reference theta level
  data thlev / 280., 290., 300., 310., 320., 330., 340., 350., 360., 370., 380., 390., 400. /
!  integer(kind=i4b), parameter   :: ithlev = 25
!  real(kind=r4b)                 :: thlev(ithlev)  !! reference theta level
!  data thlev / 280., 285., 290., 295., 300., 305., 310., 315., 320., 325., &
!  &            330., 335., 340., 345., 350., 355., 360., 365., 370., 375., &
!  &            380., 385., 390., 395., 400. /

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
end module common_pv

!c=====================================================================c

program gtpvsfc
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_pv,  only: nvar, setup_cnst, ar, cp, g, h, rd, rkappa, PVU, ithlev, thlev
  use util, only: get_axname, getyaxis, getzaxis, set_plevs
  use util_steflux, only: conv_t2th, conv_th2t, pvseek2
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgt_r4, get_etacoef, get_hdtime
  use calculate, only: shift
  use zmean, only: ZONALmean
  use error_handler, only: ioerror, werr, werr2
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r4b), allocatable :: uu(:,:,:)       !! u (m/s) 
  real(kind=r4b), allocatable :: vv(:,:,:)       !! v (m/s)
  real(kind=r4b), allocatable :: tt(:,:,:)       !! t (K)
  real(kind=r4b), allocatable :: th(:,:,:)       !! theta (K)
  real(kind=r4b), allocatable :: zz(:,:,:)       !! z (m) dummy
  real(kind=r4b), allocatable :: qq(:,:,:)       !! q (K/s) dummy
  real(kind=r4b), allocatable :: ps(:,:)         !! Ps (hPa)
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)       !! gtool3 header
  character(len=ncc)          :: head2(ndc)      !! gtool3 header
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax            !! x-axis sizes
  integer(kind=i4b)           :: jmax            !! y-axis sizes
  integer(kind=i4b)           :: kmax            !! z-axis sizes
  real(kind=r4b)              :: rmiss           !! missing value
  character(len=ncc)          :: hdset           !! dataset name
  !c+++ input from get_hdtime
  character(len=ncc)          :: htunit          !! unit of time
  real(kind=r8b)              :: time            !! time
  real(kind=r8b)              :: tdur            !! time step
  integer(kind=i4b)           :: jdate(6)        !! time array (year,mon,day,hour,minute,second)
  !c+++ input from getparms
  integer(kind=i4b)           :: ista            !! start record
  integer(kind=i4b)           :: iend            !! end record
  character(len=nfiln)        :: iu              !! input zonal wind file name
  character(len=nfiln)        :: iv              !! input meridional wind file name
  character(len=nfiln)        :: ith             !! input potential temp. file name (or temp. file)
  character(len=nfiln)        :: ip              !! input pressure file name
  character(len=nfiln)        :: ips             !! input ps file name
  character(len=nfiln)        :: ofile(nvar)     !! output file names
  character(len=ncc)          :: hitem(nvar)     !! output file items
  character(len=ncc*2)        :: htitl(nvar)     !! output file titles
  character(len=ncc)          :: hunit(nvar)     !! output file units
  real(kind=r4b)              :: levs(nvar)      !! th-levels (K)
  logical                     :: otinp           !! t: t-input , f: theta-input
  character(len=ncc)          :: hdfmt           !! data format
  character(len=ncc)          :: zax             !! zaxis plev/siglv/etalv
  logical                     :: ozm             !! t: zonal mean, f: original
  logical                     :: oapnd           !! t: append, f: replace
  !c+++ axis names (get_axname)
  character(len=ncc)          :: haxisx          !! x-axis names
  character(len=ncc)          :: haxisy          !! y-axis names
  character(len=ncc)          :: haxisz          !! z-axis names
  !c+++ y-, z- axis
  real(kind=r4b), allocatable :: rlat(:)         !! latitude (rad)
  real(kind=r4b), allocatable :: pres(:,:,:)     !! 3-D p(hPa)
  !c+++ y-, z- axis from file
  real(kind=r8b), allocatable :: lat(:)          !! latutude (deg)
  real(kind=r8b), allocatable :: p(:)            !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)       !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)       !! for eta half lev.
  !c+++ [output]
  !c+++ output data (convine)
  real(kind=r4b), allocatable :: dout(:,:,:)     !! output data
  !c+++ from pvseek2
  real(kind=r4b), allocatable :: pv_th(:,:,:)    !! 温位面上のPV (K m2/s/kg)
  real(kind=r4b), allocatable :: pv_thpv(:,:,:)  !! 温位面上のPV (PVU)
  real(kind=r4b), allocatable :: p_th(:,:,:)     !! 温位に対応する気圧面(hPa)
  real(kind=r4b), allocatable :: mf_th(:,:,:)    !! 温位に対応する気圧面の質量フラックス(kg/m2/s)
  real(kind=r4b), allocatable :: t_th(:,:,:)     !! 温位に対応する気温(K)
  real(kind=r4b), allocatable :: z_th(:,:,:)     !! 温位に対応するジオポテンシャル高度(m)
  !c+++ [work]
  integer(kind=i4b)           :: i, j, k         !! loop variables
  integer(kind=i4b)           :: it, ivar        !! loop variables
  integer(kind=i4b)           :: imax2           !! x-axis sizes for zonal mean calc.
! character(len=ncc*2)        :: htitlz          !! title
  real(kind=r8b)              :: pi              !! pi
  integer(kind=i4b)           :: ios             !! end code
  integer(kind=i4b)           :: ilevs(nvar)     !! levels of upper boundary
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_u         !! for zonal wind file (input)
  integer(kind=i4b)           :: jfile_v         !! for meridional wind file (input)
  integer(kind=i4b)           :: jfile_th        !! for potential temp. file (input)
  integer(kind=i4b)           :: jfile_p         !! for pressure file (input)
  integer(kind=i4b)           :: jfile_ps        !! for ps file (input)
  integer(kind=i4b)           :: jfile_o(nvar)   !! for output file
  !c+++ for zonal mean (-zmean t )
  real(kind=r4b), allocatable :: pv_thpv_zm(:,:) !! 帯状平均 温位面上のPV (K m2/s/kg)
  real(kind=r4b), allocatable :: p_th_zm(:,:)    !! 帯状平均 温位に対応する気圧面(hPa)
  real(kind=r4b), allocatable :: t_th_zm(:,:)    !! 帯状平均 温位に対応する気温(K)
  !c+++ [internal switch]
  logical                     :: osig = .false.  !! enable sigma-lev.
  logical                     :: oeta = .false.  !! enable eta-lev.
  logical                     :: ozlv = .false.  !! enable z-lev.
  logical                     :: opin = .false.  !! enable pressure input

!c
!c prepare
!c===
  pi = 4.d0 * atan(1.d0)
  !c+++ read parameters
  call getparms
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev') ozlv = .true.
  if (trim(ip) /= 'NONE') opin = .true.
  if (ozlv.and. .not.opin) osig = .true. !! for Ps input

  !c+++ constants
  call setup_cnst

  do ivar = 1, nvar
    ilevs(ivar) = 1
    do k = 1, ithlev
      if (thlev(k) >= levs(ivar)) then
        ilevs(ivar) = k
        exit
      endif
    enddo !! k 
  enddo !! ivar
  write(6, *) 'output levs: ', ilevs(1:nvar)
  
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
  allocate(lat(jmax), rlat(jmax), p(kmax))
  allocate(pres(imax,jmax,kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
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
      enddo !! j
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

    !c+++ read input u data
    call rgt_r4(jfile_u, imax, jmax, kmax, head, uu, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_u, ios)
    !c+++ get time & unit from gtool header
    call get_hdtime(time, tdur, htunit, jdate, head)

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

    !c+++ dummy
    zz(1:imax,1:jmax,1:kmax) = rmiss
    qq(1:imax,1:jmax,1:kmax) = rmiss

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

      do j = 1, jmax
        do i = 1, imax
          if (rlat(j) >= 0.0.and.pv_thpv(i,j,k) <= 0.0) then
            pv_thpv(i,j,k) = rmiss
          else if (rlat(j) < 0.0.and.pv_thpv(i,j,k) >= 0.0) then
            pv_thpv(i,j,k) = rmiss
          else if (abs(pv_thpv(i,j,k)) >= 100.0) then
            pv_thpv(i,j,k) = rmiss
          endif
        enddo !! i
      enddo !! j
    enddo !! k

    if (ozm) then
      imax2 = 1
      !c+++ zonal mean case
      allocate(pv_thpv_zm(jmax,ithlev))
      allocate(p_th_zm(jmax,ithlev), t_th_zm(jmax,ithlev))
      !c+++ theta-levs
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), pv_thpv, pv_thpv_zm)
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), t_th, t_th_zm)
      call ZONALmean(imax, jmax, ithlev, dble(rmiss), p_th, p_th_zm)
    else
      imax2 = imax
    endif

    if (ozm) then
      pv_thpv(1,1:jmax,1:ithlev) = pv_thpv_zm(1:jmax,1:ithlev)
      p_th(1,1:jmax,1:ithlev) = p_th_zm(1:jmax,1:ithlev)
      t_th(1,1:jmax,1:ithlev) = t_th_zm(1:jmax,1:ithlev)
    endif

    !c+++ set output header
!   write(head(31), '(i16)') imax2
!   head(35) = 'SFC1'
!   write(head(37), '(i16)') 1
!   write(head(64), '(i16)') imax2*jmax
    !c+++ set output data
    allocate(dout(imax2,jmax,nvar))
    do ivar = 1, nvar
      dout(1:imax2,1:jmax,ivar) = pv_thpv(1:imax2,1:jmax,ilevs(ivar)) !! PV (PVU) at ilevs
    enddo !! ivar

    !c+++ write output data
    do ivar = 1, nvar
!     head(3)  = hitem(ivar)
!     htitlz   = htitl(ivar)
!     head(14) = htitlz(1:16)
!     head(15) = htitlz(17:32)
!     head(16) = hunit(ivar)
!     call wgthdd(jfile_o(ivar), imax2, jmax, 1, head, hdfmt, dout, ios)
      !write(jfile_o(ivar)) head
      !write(jfile_o(ivar)) dout(1:imax2,1:jmax,ivar)
      call wgt_r4(jfile_o(ivar), imax2, jmax, 1, dble(rmiss), dout(1:imax2,1:jmax,ivar), &
&       hitem(ivar), htitl(ivar), hunit(ivar), &
&       hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, 'SFC1', ios)
      if (ios /= 0) call ioerror(jfile_o(ivar), ios)
    enddo !! iv

    !c+++ deallocate
    deallocate(dout)
    if (ozm) deallocate(pv_thpv_zm, p_th_zm, t_th_zm)
    if (it == iend) exit
  enddo !! it

  !c+++ close files
  call gtclose(jfile_u, ios)
  call gtclose(jfile_v, ios)
  call gtclose(jfile_th, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  do ivar = 1, nvar
    call gtclose(jfile_o(ivar), ios)
  enddo
  !c+++ deallocate
  deallocate(uu, vv, tt, th, zz, qq, ps, pv_th, pv_thpv, p_th, t_th, z_th, mf_th)
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
  call get_parms('u', 'u_P', iu, ios)
  call get_parms('v', 'v_P', iv, ios)
  call get_parms('th', 'THETA_P', ith, ios)
  call get_parms('t', 'T_P', itmp, ios)
  call get_parms('p', 'NONE', ip, ios)
  call get_parms('ps', 'Ps', ips, ios)
  !c+++ theta/temp
  otinp = .false. !! mode (th-inp)
  call get_parms('tinp', 'theta', hdump, ios)
  if (trim(hdump) == 'temp') otinp = .true. !! mode (t-inp)
  !c+++ output files
  call get_parms('o', 'PV_350K,PV_330K,PV_310K', ofiles, ios)
  call read_opt(nvar, ofiles, ofile)
  !c+++ items
  call get_parms('item', 'PV,PV,PV', hdump, ios)
  call read_opt(nvar, hdump, hitem)
  !c+++ titles
  call get_parms('titl', 'PV on 350K,PV on 330K,PV on 310K', hdump, ios)
  call read_opt(nvar, hdump, htitl)
  !c+++ units
  call get_parms('unit', 'PVU,PVU,PVU', hdump, ios)
  call read_opt(nvar, hdump, hunit)
  !c+++ levs
  call get_parms('lev', '350.0,330.0,310.0', hdump, ios)
  call read_opt(nvar, hdump, hlevs)
  do ivar = 1, nvar
    read(hlevs(ivar), *) levs(ivar)
  enddo !! ivar
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ zonal mean
  call get_parms('zmean', 'f', hval, ios)
  call c2var(ozm, '(l1)', hval)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  !c+++ axis
  call get_parms('zax', 'plev', zax, ios)
  if (ios /= 0) call xabort

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
    write(6, '(a, i2, a, f5.0)') 'getparms: lev  (', ivar, ') = ', levs(ivar)
  enddo !! ivar

! 'PV_350K,PV_330K,PV_310K'
! 'PV,PV,PV'
! 'PV on 350K,PV on 330K,PV on 310K'
! 'PVU,PVU,PVU'
! '350.0,330.0,310.0'

! !c+++
! ofile( 1) = 'PV_350K'                !! output file name
! hitem( 1) = 'PV'                     !! item
! htitl( 1) = 'PV on 350K'             !! title
! hunit( 1) = 'PVU'                    !! unit
! levs ( 1) = 350.0                    !! th-level
! !c+++
! ofile( 2) = 'PV_330K'                !! output file name
! hitem( 2) = 'PV'                     !! item
! htitl( 2) = 'PV on 330K'             !! title
! hunit( 2) = 'PVU'                    !! unit
! levs ( 2) = 330.0                    !! th-level
! !c+++
! ofile( 3) = 'PV_310K'                !! output file name
! hitem( 3) = 'PV'                     !! item
! htitl( 3) = 'PV on 310K'             !! title
! hunit( 3) = 'PVU'                    !! unit
! levs ( 3) = 310.0                    !! th-level

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: gtpvsfc'
  write(6, '(a)') '-o output-file-1,output-file-2,output-file-3'
  write(6, '(a)') '-item item-1,item-2,.. -titl title-1,title-2,... -unit unit-1,unit-2,...'
  write(6, '(a)') '-lev 350.0,330.0,310.0 (K, theta-levels)'
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
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-p input-p-file (pressure, optional)'
  write(6, '(a)') '  (default, NONE)'
  write(6, '(a)') '-ps input-ps-file (surface pressure, optional)'
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
  write(6, '(a)') '-zmean f/t (zonal mean calculation)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') '-apnd f/t'
  stop 2
end subroutine xabort

!c---------------------------------------------------------------------c

!c=====================================================================c

end program gtpvsfc
