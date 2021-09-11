!c
!c  program gtpvont (old name: pvont)
!c  [history]
!c  2001/04/02 subroutine pvseek by S. Sugata (pvont.f, now in pvsub)
!c  2001/04/03 add theta axis file output to pvseek by S. Sugata (pvont.f)
!c  2013/03/03 Yamashita: f77==>f90
!c  2013/03/03 Yamashita: use gtool header to determine array size
!c  2013/03/11 Yamashita: use common_args
!c  2013/03/11 Yamashita: add subroutine getyaxis, getzaxis, getetaaxis, get_axname
!c  2013/03/11 Yamashita: replace theta axis file output by subroutine putzaxis
!c  2013/03/11 Yamashita: add subroutine wmsg
!c  2013/03/28 Yamashita: add option for p==>theta, eta==>theta, sig==>theta
!c  2013/03/30 Yamashita: add subroutine set_jdaten, set_user
!c  2013/10/22 Yamashita: add 480K and 840K levels
!c  2013/10/22 Yamashita: add gtopen
!c  2021/08/18 Yamashita: modify getparms & I/O 
!c
!c  温位面上のPVを書き出すプログラム
!c
!c  internal: module common_pv
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module common_typedef
!c  external: module util
!c            subroutines get_axname, getyaxis, getzaxis, putzaxis, set_plevs
!c  external: module calculate
!c            subroutine shift
!c  external: module util_pvont
!c            subroutine pvseek
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, 
!c            rgt_r4, wgthdd, get_etacoef
!c  external: error_handler
!c            subroutine ioerror, werr
!c
!c=====================================================================c
module common_pv
  use common_args, only: ncc
  use common_typedef, only: i4b, r4b

  !c+++ [parameter]
  !c+++ internal parameters
  integer(kind=i4b), parameter :: ithlev = 32
  character(len=ncc), parameter :: caxis = 'THETA32'

  real(kind=r4b) :: thlev(ithlev) !! reference theta level
  data thlev/  330.,  340.,  350.,  380.,  400., &
&              425.,  450.,  480.,  500.,  550., &
&              600.,  650.,  700.,  750.,  800., &
&              840.,  850.,  900.,  950., 1000., &
&             1100., 1200., 1400., 1600., 1800., &
&             2000., 2200., 2400., 2600., 2900., &
&             3300., 3800./

end module common_pv

!c=====================================================================c

program gtpvont
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_pv, only: ithlev, caxis, thlev
  use util, only: get_axname, getyaxis, getzaxis, putzaxis, set_plevs
  use calculate, only: shift
  use util_pvont, only: pvseek
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgthdd, get_etacoef
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r4b), allocatable :: uu(:,:,:)      !! u
  real(kind=r4b), allocatable :: vv(:,:,:)      !! v
  real(kind=r4b), allocatable :: th(:,:,:)      !! theta
  real(kind=r4b), allocatable :: ps(:,:)        !! Ps
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header (ps)
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax           !! x-size
  integer(kind=i4b)           :: jmax           !! y-sizes
  integer(kind=i4b)           :: kmax           !! z-size
  real(kind=r4b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisx         !! x-axis name
  character(len=ncc)          :: haxisy         !! y-axis name
  character(len=ncc)          :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  character(len=nfiln)        :: iu             !! input zonal wind file name
  character(len=nfiln)        :: iv             !! input meridional wind file name
  character(len=nfiln)        :: ith            !! input potential temp. file name
  character(len=nfiln)        :: ips            !! input ps file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  character(len=ncc)          :: zax            !! zaxis plev/zlev/siglv/etalv
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: pv(:,:,:)      !! 温位面上のPV
  !c+++ [work]
  integer(kind=i4b)           :: it, j, k       !!
  character(len=ncc*2)        :: htitlz         !! title
  real(kind=r8b)              :: pi
  integer(kind=i4b)           :: ios            !! end code
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_u        !! for zonal wind file
  integer(kind=i4b)           :: jfile_v        !! for meridional wind file
  integer(kind=i4b)           :: jfile_th       !! for potential temp. file
  integer(kind=i4b)           :: jfile_ps       !! for ps file
  integer(kind=i4b)           :: jfile_o        !! for output file
  !c+++ y-, z- axis
  real(kind=r4b), allocatable :: rlat(:)        !! latitude (rad)
  real(kind=r4b), allocatable :: plev(:,:,:)    !! 3-D p(hPa)
  !c+++ y-, z- axis from file
  real(kind=r8b), allocatable :: lat(:)         !! latutude (deg)
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ [internal switch]
  logical                     :: osig = .false. !! enable sigma-lev.
  logical                     :: oeta = .false. !! enable eta-lev.

!c
!c prepare
!c===
  pi = 4.d0 * atan(1.d0)
  !c+++ read parameters
  call getparms
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.

  !c+++ open input zonal wind file
  write(6, *) 'open input zonal wind file: ', trim(iu)
  call gtopen(trim(iu), 'r', jfile_u, ios)
  if (ios /= 0) call ioerror(jfile_u, ios)

  !c+++ open input meridional wind file
  write(6, *) 'open input meridional wind file: ', trim(iv)
  call gtopen(trim(iv), 'r', jfile_v, ios)
  if (ios /= 0) call ioerror(jfile_v, ios)

  !c+++ open input potential temp. file
  write(6, *) 'open input potential temp. file: ', trim(ith)
  call gtopen(trim(ith), 'r', jfile_th, ios)
  if (ios /= 0) call ioerror(jfile_th, ios)

  !c+++ open input surface pressure file
  if (osig.or.oeta) then
    write(6, *) 'open input surface pressure file: ', trim(ips)
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
  endif

  !c+++ open output file
  write(6, *) 'open output file: ', trim(ofile)
  if (oapnd) then
    call gtopen(trim(ofile), 'a', jfile_o, ios)
  else
    call gtopen(trim(ofile), 'w', jfile_o, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_u, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_u, ios)
  call gtrewind(jfile_u, ios)
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)
  !c+++ get axis names
  call get_axname(haxisx, haxisy, haxisz, head)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  write(6, *) 'hdfmt = ', trim(hdfmt)

  !c+++ allocate
  allocate(uu(imax,jmax,kmax))
  allocate(vv(imax,jmax,kmax))
  allocate(th(imax,jmax,kmax))
  allocate(ps(imax,jmax))
  allocate(pv(imax,jmax,ithlev))
  allocate(lat(jmax), rlat(jmax), p(kmax))
  allocate(plev(imax,jmax,kmax))
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
    !c+++ read z-axis file (for p-lev & sig-lev)
    call getzaxis(kmax, haxisz, p)
  else
    !c+++ read z-axis file (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  endif

  !c+++ write theta-axis file
  call putzaxis(ithlev, haxisz, caxis, 'theta', 'K', thlev)

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
      cycle
    endif

    !c+++ read u data
    call rgt_r4(jfile_u, imax, jmax, kmax, head, uu, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_u, ios)

    !c+++ read v data
    call rgt_r4(jfile_v, imax, jmax, kmax, head2, vv, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_v, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')

    !c+++ read theta data
    call rgt_r4(jfile_th, imax, jmax, kmax, head2, th, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_th, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')

    !c+++ read surface pressure data
    if (osig.or.oeta) then
      call rgt_r4(jfile_ps, imax, jmax, 1, head2, ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head(16)) == 'Pa') then
        call shift(imax, jmax, 1, dble(rmiss), ps, ps, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps(1,1), ' (hPa)'
      endif
    endif

    !c+++ set pres
    call set_plevs(imax, jmax, kmax, dble(rmiss), p, ps, eta_fa, eta_fb, plev, osig, oeta)

    !c+++ calculate PV
    do k = 1, ithlev
      call pvseek(uu, vv, th, thlev(k), plev, rlat, pv(1,1,k), rmiss, imax, jmax, kmax)
    enddo !! k

    !c+++ set output header
    head(3)  = hitem
    htitlz   = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit
    head(35) = caxis
    write(head(37), '(i16)') ithlev
    write(head(64), '(i16)') imax*jmax*ithlev

    !c+++ write replace data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, pv, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo !! it

  !c+++ close files
  call gtclose(jfile_u, ios)
  call gtclose(jfile_v, ios)
  call gtclose(jfile_th, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  call gtclose(jfile_o, ios)
  !c+++ deallocate
  deallocate(uu, vv, th, ps, pv, lat, rlat, p, plev)
  deallocate(eta_fa, eta_fb)

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
  !c+++ input/output file
  call get_parms('u', 'u_P', iu, ios)
  call get_parms('v', 'v_P', iv, ios)
  call get_parms('th', 'THETA_P', ith, ios)
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'PV', hitem, ios)
  call get_parms('titl', 'Potential vort. on theta', htitl, ios)
  call get_parms('unit', '1/s', hunit, ios)
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

subroutine xabort
  write(6, '(a)') 'Usage: gtpvont'
  write(6, '(a)') '-o output-PV-file'
  write(6, '(a)') '-u input-u-file'
  write(6, '(a)') '-v input-v-file'
  write(6, '(a)') '-th input-theta-file'
  write(6, '(a)') ' '
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-ps input-ps-file (surface pressure [hPa or Pa], optional)'
  write(6, '(a)') '  (default, Ps)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-zax plev/siglv/etalv'
  write(6, '(a)') 'plev: assume p-levels (default), and ignore -ps option'
  write(6, '(a)') 'siglv: assume sigma-levels, etalv: assume eta-levels'
  write(6, '(a)') '(-ps input-ps-file) for siglv/etalv'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtpvont
