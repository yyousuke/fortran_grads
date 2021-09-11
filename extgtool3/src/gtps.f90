!c
!c  program gtps
!c  [history]
!c  2017/04/19 Yamashita: first ver.
!c  2017/10/08 Yamashita: modify getparms
!c  2017/10/09 Yamashita: use common_typedef & common_const
!c
!c  output Ps [hPa] from T, Z for p-, z-, sig-, and eta-levels
!c
!c  internal: module common_ps
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module common_typedef
!c  external: module util
!c            subroutines get_axname, getzaxis, set_plevs
!c  external: module calculate
!c            subroutine shift
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, 
!c            rgt_r4, wgt_r4, get_hdtime, get_etacoef
!c  external: error_handler
!c            subroutine ioerror
!c
!c======================================================================c
module common_ps
  use common_typedef, only: r8b
  use common_const, only: cnst_lapl, cnst_h, cnst_egrav, cnst_rair, const_setup

  !c+++ [parameter]
  real(kind=r8b), parameter :: lapl = cnst_lapl !! lapse rate (K/m)
  !c+++ [save, enable modication]
  real(kind=r8b), save      :: h                !! scale height [m]
  real(kind=r8b), save      :: g                !! gravitational acceleration (m/s2)
  real(kind=r8b), save      :: Rd               !! gas constant of dry air [J/K/kg]
  real(kind=r8b), save      :: ggg              !!

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    h   = cnst_h
    g   = cnst_egrav
    Rd  = cnst_rair
    ggg = g * rd * lapl
  end subroutine setup_cnst
!c----------------------------------------------------------------------c

end module common_ps

!c=====================================================================c

program gtps
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_ps, only: setup_cnst, h
  use util, only: get_axname, getzaxis, set_plevs
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgt_r4, get_hdtime, get_etacoef
  use calculate, only: shift
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r4b), allocatable :: t(:,:,:)       !! t (K)
  real(kind=r4b), allocatable :: z(:,:,:)       !! z (m)
  real(kind=r4b), allocatable :: zs(:,:)        !! zs (m)
  !c+++ input from file header
  integer(kind=i4b)           :: imax           !! x-axis sizes
  integer(kind=i4b)           :: jmax           !! y-axis sizes
  integer(kind=i4b)           :: kmax           !! z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisx         !! x-axis names
  character(len=ncc)          :: haxisy         !! y-axis names
  character(len=ncc)          :: haxisz         !! z-axis names
  character(len=ncc)          :: hdset          !! dataset name
  character(len=ncc)          :: htunit         !! unit of time
  real(kind=r8b)              :: time           !! time
  real(kind=r8b)              :: tdur           !! time step
  integer(kind=i4b)           :: jdate(6)       !! time array (year,mon,day,hour,minute,second)
  !c+++ input from getparms
  character(len=nfiln)        :: itmp           !! input temp. file name
  character(len=nfiln)        :: iz             !! input geopotential height file name
  character(len=nfiln)        :: izs            !! input surface height file name
  character(len=nfiln)        :: ip             !! input pressure file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  logical                     :: ozint          !! true: vertical intrp. on
  character(len=ncc)          :: hdfmt          !! data format
  character(len=ncc)          :: zax            !! zaxis plev/zlev/siglv/etalv
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ y-, z- axis from file
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: ps(:,:)        !! surface pressure (hPa)
  !c+++ [internal work]
  integer(kind=i4b)           :: it, k          !!
  character(len=ncc)          :: head(ndc)      !!
  real(kind=r4b), allocatable :: pres(:,:,:)    !! 3-D p(hPa)
  integer(kind=i4b)           :: ios            !! end code
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_t        !! for temp. file (input)
  integer(kind=i4b)           :: jfile_z        !! for geopotential height file (input)
  integer(kind=i4b)           :: jfile_zs       !! for surface height file (input)
  integer(kind=i4b)           :: jfile_p        !! for pressure file (input)
  integer(kind=i4b)           :: jfile_o        !! for surface pressure file (output)
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

  !c+++ open input temperature file
  write(6, *) 'open input temp. file: ', trim(itmp)
  write(6, *) 'open input potential temp. file: ', trim(itmp)
  call gtopen(trim(itmp), 'r', jfile_t, ios)
  if (ios /= 0) call ioerror(jfile_t, ios)

  !c+++ open input geopotential height file
  write(6, *) 'open input geopotential height file: ', trim(iz)
  call gtopen(trim(iz), 'r', jfile_z, ios)
  if (ios /= 0) call ioerror(jfile_z, ios)

  !c+++ open input surface height file
  write(6, *) 'open input surface height file: ', trim(izs)
  call gtopen(trim(izs), 'r', jfile_zs, ios)
  if (ios /= 0) call ioerror(jfile_zs, ios)

  !c+++ open input pressure file
  if (opin) then
    write(6, *) 'open input pressure file: ', trim(ip)
    call gtopen(trim(ip), 'r', jfile_p, ios)
    if (ios /= 0) call ioerror(jfile_p, ios)
  endif

  !c+++ open output file (surface pressure)
  write(6, '(4a)') 'open output file: ', trim(ofile), ', title = ', trim(htitl)
  if (oapnd) then
    call gtopen(trim(ofile), 'a', jfile_o, ios)
  else
    call gtopen(trim(ofile), 'w', jfile_o, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_t, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_t, ios)
  call gtrewind(jfile_t, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ set axis names & dset
  call get_axname(haxisx, haxisy, haxisz, head)
  hdset = head(2)
  write(6, *) 'haxisx = ', trim(haxisx)
  write(6, *) 'haxisy = ', trim(haxisy)
  write(6, *) 'haxisz = ', trim(haxisz)
  write(6, *) 'hdset = ', trim(hdset)
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(t(imax,jmax,kmax))
  allocate(z(imax,jmax,kmax))
  allocate(zs(imax,jmax))
  allocate(pres(imax,jmax,kmax))
  allocate(ps(imax,jmax))
  allocate(p(kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
  eta_fa(1:kmax) = rmiss
  eta_fb(1:kmax) = rmiss

  !c+++ z-axis
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
  !c+++ set pres by set_plevs (for sig- and eta-levs. Ps = 1000 is assumed)
  ps(1:imax,1:jmax) = 1000.d0
  call set_plevs(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pres, osig, oeta)

  !c+++ read input zs data
  call rgt_r4(jfile_zs, imax, jmax, 1, head, zs, ios)
  if (ios /= 0) call ioerror(jfile_zs, ios)

!c
!c surface pressure
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_t, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_t, ios)
      call gtskip(jfile_z, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_z, ios)
      if (opin) then
        call gtskip(jfile_p, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_p, ios)
      endif
      cycle
    endif

    !c+++ read input t data
    call rgt_r4(jfile_t, imax, jmax, kmax, head, t, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_t, ios)

    !c+++ read input z data
    call rgt_r4(jfile_z, imax, jmax, kmax, head, z, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_z, ios)

    !c+++ modify pres for p-input
    if (opin) then
      !c+++ read input pressure data
      call rgt_r4(jfile_p, imax, jmax, kmax, head, pres, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_p, ios)
      !c+++ Pa ==> hPa
      if (trim(head(16)) == 'Pa') then
        call shift(imax, jmax, kmax, rmiss, pres, pres, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'pres(1,1,1) = ', pres(1,1,1), ' (hPa)'
      endif
    endif
    !c+++ get time & unit from gtool header
    call get_hdtime(time, tdur, htunit, jdate, head)

    !c+++ surface pressure
    call calc_ps(imax, jmax, kmax, rmiss, ozint, pres, t, z, zs, ps)

    !c+++ write 3-D P
    call wgt_r4(jfile_o, imax, jmax, 1, rmiss, ps(1:imax,1:jmax), &
&     hitem, htitl, hunit, hdset, hdfmt, time, tdur, htunit, jdate, &
&     haxisx, haxisy, haxisz, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo

  !c+++ close files
  call gtclose(jfile_t, ios)
  call gtclose(jfile_z, ios)
  call gtclose(jfile_zs, ios)
  if (opin) call gtclose(jfile_p, ios)
  call gtclose(jfile_o, ios)

  !c+++ deallocate
  deallocate(t, z, zs, pres, ps, p)
  deallocate(eta_fa, eta_fb)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine calc_ps
!c  calculate surface pressure
!c
!c=====
subroutine calc_ps(imax, jmax, kmax, rmiss, ozint, pres, t, z, zs, ps)
  use common_ps
  use intplt, only: intpltz
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-sizes
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  logical, intent(in)    :: ozint                !! true: vertical intrp. on
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! p (hPa)
  real(kind=r4b), intent(in)    :: t(imax,jmax,kmax)    !! T (K)
  real(kind=r4b), intent(in)    :: z(imax,jmax,kmax)    !! z (m)
  real(kind=r4b), intent(in)    :: zs(imax,jmax)        !! surface height (m)
  !c+++ [output]
  real(kind=r4b), intent(out)   :: ps(imax,jmax)        !! surface pressure (hPa)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k
  integer(kind=i4b)             :: kk(1)
  real(kind=r4b)                :: pp(kmax)             !! p (hPa)
  real(kind=r4b)                :: tt(kmax)             !! T (K)
  real(kind=r4b)                :: zz(kmax)             !! z (m)
!c
!c Ps
!c===
  do j = 1, jmax
    do i = 1, imax
      if (zs(i,j) == rmiss) then
        ps(i,j) = rmiss
        exit
      endif
      if (ozint) then
        !c+++ vertical interpolation: p
        call intpltz(kmax, kmax, rmiss, pres(i,j,1:kmax), pres(i,j,1:kmax), &
&         ps(i,j), h, 1.d0, 0.d0, .true., .true., .true., pres(i,j,1:kmax), pp(1:kmax))
        !c+++ vertical interpolation: t
        call intpltz(kmax, kmax, rmiss, pres(i,j,1:kmax), pres(i,j,1:kmax), &
&         ps(i,j), h, 1.d0, 0.d0, .true., .false., .true., t(i,j,1:kmax), tt(1:kmax))
        !c+++ vertical interpolation: z
        call intpltz(kmax, kmax, rmiss, pres(i,j,1:kmax), pres(i,j,1:kmax), &
&         ps(i,j), h, 1.d0, 0.d0, .true., .false., .true., z(i,j,1:kmax), zz(1:kmax))
      else
        pp(1:kmax) = pres(i,j,1:kmax)
        tt(1:kmax) = t(i,j,1:kmax)
        zz(1:kmax) = z(i,j,1:kmax)
      endif
      !c+++ ps
      kk = minloc(abs(zs(i,j) - zz(:)), mask=zs(i,j)<zz(:))
      k = kk(1)
      if (tt(k) == rmiss.or.pp(k) == rmiss.or.zz(k) == rmiss) then
        ps(i,j) = rmiss
      else
        ps(i,j) = pp(k) * (1.d0 - lapl * (zs(i,j) - zz(k)) / tt(k))**ggg
      endif
    enddo !! i
  enddo !! j

  return
end subroutine calc_ps

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
  call get_parms('t', 'T_P', itmp, ios)
  call get_parms('z', 'z_P', iz, ios)
  call get_parms('zs', 'zs', izs, ios)
  call get_parms('p', 'NONE', ip, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'GAP', hitem, ios)
  call get_parms('titl', 'pressure', htitl, ios)
  call get_parms('unit', 'hPa', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ zint
  call get_parms('zint', 'f', hval, ios)
  call c2var(ozint, '(l1)', hval)
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
  write(6, '(a)') 'Usage: gtps -o output-file'
  write(6, '(a)') '-t input-t-file (temperature)'
  write(6, '(a)') '-z input-z-file (geopotential height)'
  write(6, '(a)') '-zs input-zs-file (surface height)'
  write(6, '(a)') '-p input-p-file (pressure, optional)'
  write(6, '(a)') '  (default, NONE)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-zint f/t (vertical interpolation of t, z, p data)'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-zax plev/zlev/siglv/etalv'
  write(6, '(a)') 'plev: assume p-levels (default)'
  write(6, '(a)') 'zlev: assume z-levels'
  write(6, '(a)') 'siglv: assume sigma-levels, etalv: assume eta-levels'
  write(6, '(a)') '(for siglv and etalv: ps = 1000 hPa is assumed'
  write(6, '(a)') '------------------------------------------'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtps
