!c
!c  gtthetae_p.f90
!c  [history]
!c  2016/01/28 Yamashita: first ver. from gttheta_p (2016/01/28 Yamashita)
!c  2016/11/29 Yamashita: add Pa ==> hPa conversion
!c  2016/11/29 Yamashita: add rgt, rgthd
!c  2016/11/29 Yamashita: bug fix of large rmiss
!c  2017/06/09 Yamashita: modify getparms & I/O 
!c  2017/06/09 Yamashita: use common_typedef & common_const
!c  2017/08/04 Yamashita: use setup_cnst
!c
!c  T, Q ==>THETAe conversion for p-, sig-, and eta-levels
!c
!c  internal: module common_thetae
!c            subroutine setup_cnst
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  internal subroutine conv_t2the
!c  external: module common_args
!c  external: module common_typedef
!c  external: module common_const
!c  external: module util
!c            subroutines getzaxis, set_plevs
!c  external: module rwgtool
!c            subroutine gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd, get_etacoef
!c  external: module dcalclate
!c            subroutine dshift
!c  eaternal: error_handler
!c            subroutine ioerror, werr
!c
!c======================================================================c

module common_thetae
  use common_typedef, only: r8b
  use common_const, only: cnst_pre00, cnst_kappa, cnst_h, cnst_cp, cnst_lh0, const_setup
  real(kind=r8b), parameter  :: P00  = cnst_pre00 * 1d-2 !! reference surface pressure [hPa]
  real(kind=r8b), save       :: rkappa                   !! RAIR / CP
  real(kind=r8b), save       :: h                        !! scale height [m]
  real(kind=r8b), save       :: CP                       !! specific heat at constant pressure of air (J/K/kg)
  !use common_thetae, only: rkappa, h, CP, EL
  real(kind=r8b), save       :: EL                       !! latent heat for evap.

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    rkappa = cnst_kappa
    h = cnst_h
    CP = cnst_cp
    EL = cnst_lh0
  end subroutine setup_cnst
!c----------------------------------------------------------------------c
end module common_thetae

!c======================================================================c

program gtthetae_p
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_thetae, only: setup_cnst, h
  use util, only: getzaxis, set_plevs
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd, get_etacoef
  use dcalculate, only: dshift
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r8b), allocatable :: din(:,:,:)     !! input data
  real(kind=r8b), allocatable :: ps(:,:)        !! input Ps [hPa]
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header (ps)
  !c+++ input from gtool3 header (rgthd)
  integer(kind=i4b)           :: imax           !! x-size
  integer(kind=i4b)           :: jmax           !! y-sizes
  integer(kind=i4b)           :: kmax           !! z-size
  real(kind=r8b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  character(len=nfiln)        :: ifile          !! input T file name
  character(len=nfiln)        :: iq             !! input Q file name
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
  !c+++ output data
  real(kind=r4b), allocatable :: dout(:,:,:)    !! output data
  !c+++ [internal work]
  integer(kind=i4b)           :: it, k
  integer(kind=i4b)           :: ios            !! end code
  character(len=ncc*2)        :: htitlz         !!
  !c+++ data
  real(kind=r4b), allocatable :: ps4(:,:)       !! Ps [hPa]
  real(kind=r4b), allocatable :: pr4(:,:,:)     !! [hPa]
  real(kind=r8b), allocatable :: pres(:,:,:)    !! [hPa]
  real(kind=r8b), allocatable :: TH(:,:,:)      !! THETA [K]
  real(kind=r8b), allocatable :: T(:,:,:)       !! T [K]
  real(kind=r8b), allocatable :: Q(:,:,:)       !! Q [kg/kg]
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input T/TH file
  integer(kind=i4b)           :: jfile_q        !! I/O unit for input q file
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

  !c+++ open input ps file for sig-lev. and eta-lev.
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

  !c+++ open input q file
  call gtopen(trim(iq), 'r', jfile_q, ios)
  if (ios /= 0) call ioerror(jfile_q, ios)

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
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ set z-axis name
  haxisz = head(35)
  write(6, *) 'haxisz = ', haxisz
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(Ps(imax,jmax), p(kmax))
  allocate(din(imax,jmax,kmax), dout(imax,jmax,kmax))
  allocate(T(imax,jmax,kmax), Q(imax,jmax,kmax), TH(imax,jmax,kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
  allocate(ps4(imax,jmax), pr4(imax,jmax,kmax), pres(imax,jmax,kmax))
  ps4(1:imax,1:jmax) = rmiss
  eta_fa(1:kmax) = rmiss
  eta_fb(1:kmax) = rmiss

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
  else
    !c+++ read z-axis file (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  endif

!c
!c T,P to theta
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      call gtskip(jfile_q, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_q, ios)
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
    endif

    !c+++ read temperature
    call rgt(jfile_i, imax, jmax, kmax, head, din, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    T(1:imax,1:jmax,1:kmax) = din(1:imax,1:jmax,1:kmax)

    !c+++ read Q
    call rgt(jfile_q, imax, jmax, kmax, head, din, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_q, ios)
    Q(1:imax,1:jmax,1:kmax) = din(1:imax,1:jmax,1:kmax)

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
      ps4(1:imax,1:jmax) = real(ps(1:imax,1:jmax))
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
      call set_plevs(imax, jmax, kmax, rmiss, p, ps4, eta_fa, eta_fb, pr4, osig, oeta)
      pres(1:imax,1:jmax,1:kmax) = dble(pr4(1:imax,1:jmax,1:kmax))
    endif

    !c+++ convert: T, P, Q ==> THETA
    call conv_t2the(imax, jmax, kmax, rmiss, T, Q, pres, TH)
    dout(1:imax,1:jmax,1:kmax) = TH(1:imax,1:jmax,1:kmax)

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit

    !c+++ write potential temperature
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo
  !c+++ close files
  call gtclose(jfile_i, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  if (opin) call gtclose(jfile_p, ios)
  call gtclose(jfile_q, ios)
  call gtclose(jfile_o, ios)
  !c+++ deallocate
  deallocate(ps, p, din, dout, T, TH, ps4, pr4, pres)
  deallocate(eta_fa, eta_fb)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine conv_t2the
!c
!c=====
subroutine conv_t2the(imax, jmax, kmax, rmiss, T, Q, pres, THE)
  use common_thetae, only: P00, CP, EL, rkappa
  implicit none
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-sizes
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r8b), intent(in)    :: T(imax,jmax,kmax)    !! T (K)
  real(kind=r8b), intent(in)    :: Q(imax,jmax,kmax)    !! q (kg/kg)
  real(kind=r8b), intent(in)    :: pres(imax,jmax,kmax) !! p (hPa)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: THE(imax,jmax,kmax)  !! THETAe (K)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k

  !c+++ T, P, Q ==> THe
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        !c+++ Te = T + EL/CP*Q
        !c+++ THe = Te * (Ps/p)**KAP
        if (T(i,j,k) /= rmiss.and.Q(i,j,k) /= rmiss.and.Q(i,j,k) > 0.d0 &
&        .and.pres(i,j,k) /= rmiss) then
          THE( I,J,K ) = (T(i,j,k) + EL / CP * Q(i,j,k)) &
&                     * (P00 / pres(i,j,k)) ** rkappa
        else
          THE(i,j,k) = rmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine conv_t2the

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
  call get_parms('t', 'T_P', ifile, ios)
  call get_parms('q', 'q_P', iq, ios)
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('p', 'NONE', ip, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'THETAE', hitem, ios)
  call get_parms('titl', 'Equiv. Pot. Temp.', htitl, ios)
  call get_parms('unit', 'K', hunit, ios)
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
  write(6, '(a)') 'Usage: gtthetae_p'
  write(6, '(a)') '-o output-file'
  write(6, '(a)') '-t input-T-file'
  write(6, '(a)') '-q input-Q-file'
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

end program gtthetae_p
