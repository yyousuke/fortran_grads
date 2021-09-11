!c
!c  program gtdobl
!c  [history]
!c  2013/09/21 Yamashita: first ver.
!c  2017/05/08 Yamashita: modify getparms & I/O 
!c  2017/05/09 Yamashita: use common_typedef & common_const
!c  2017/08/04 Yamashita: use setup_cnst
!c
!c  output total ozone [DU] from O3 vmr for p-, sig-, and eta-levels
!c  with tropopause data
!c
!c  internal: module common_dob
!c            subroutine setup_cnst
!c  external: module common_args
!c  external: module common_typedef
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  internal: subroutine dobout
!c    internal: subroutine vint
!c  external: module util
!c            subroutines getzaxis, set_plevs
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd,
!c            rgt, rgt_r4, wgthdd, get_etacoef
!c  eaternal: error_handler
!c            subroutine ioerror
!c
!c======================================================================c
module common_dob
  use common_typedef, only: r8b
  use common_const, only: cnst_NA, cnst_mair, cnst_DU, cnst_egrav, cnst_h, const_setup

  !c+++ [parameter]
  !real(kind=r8b), parameter     :: GR = 9.80616d0       !! (m/s2)
  !real(kind=r8b), parameter     :: NA = 6.0221415d23    !! (num/mol)
  !real(kind=r8b), parameter     :: mair = 2.8966d-2     !! (kg/mol)
  !real(kind=r8b), parameter     :: DU = 2.6868d20       !! (num/m2)
  real(kind=r8b), parameter     :: NA = cnst_NA     !! Avogadro number (num/mol)
  real(kind=r8b), parameter     :: mair = cnst_mair !! molecular weight of air (kg/mol)
  real(kind=r8b), parameter     :: DU = cnst_DU     !! definition of Dobson unit (num/m2)
  real(kind=r8b), save          :: GR               !! grav. acceleration of the Earth (m/s2)
  real(kind=r8b), save          :: h                !! scale height [m]

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    GR = cnst_egrav
    h = cnst_h
  end subroutine setup_cnst
!c----------------------------------------------------------------------c

end module common_dob

!c=====================================================================c

program gtdobl
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_dob, only: setup_cnst, h
  use calculate, only: shift
  use dcalculate, only: dshift
  use util, only: getzaxis, set_plevs
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, rgt_r4, wgthdd, get_etacoef
  use error_handler, only: werr, ioerror
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r4b), allocatable :: gdo3(:,:,:)    !! [vmr]
  real(kind=r4b), allocatable :: ktp(:,:)       !! tropopause grid-id []
  real(kind=r8b), allocatable :: ps(:,:)        !! Ps[hPa]
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax           !! x-axis sizes
  integer(kind=i4b)           :: jmax           !! y-axis sizes
  integer(kind=i4b)           :: kmax           !! z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: ik             !! input tropopause file name
  character(len=nfiln)        :: ips            !! input ps file name
  character(len=nfiln)        :: ip             !! input pressure file name
  character(len=nfiln)        :: ofile_s        !! output file name (strat.)
  character(len=nfiln)        :: ofile_t        !! output file name (trop.)
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  character(len=ncc)          :: zax            !! zaxis plev/zlev/siglv/etalv
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: dout(:,:,:)    !! [DU]
  !c+++ [internal work]
  integer(kind=i4b)           :: it, k          !!
  integer(kind=i4b)           :: ios            !! end code
  character(len=ncc*2)        :: htitlz         !!
  !c+++ data
  real(kind=r4b), allocatable :: pres(:,:,:)    !! [hPa]
  real(kind=r4b), allocatable :: ps4(:,:)       !! Ps[hPa]
  real(kind=r4b), allocatable :: dobsons(:,:)   !! strat. [DU]
  real(kind=r4b), allocatable :: dobsont(:,:)   !! trop. [DU]
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input file
  integer(kind=i4b)           :: jfile_k        !! for input tropopause file
  integer(kind=i4b)           :: jfile_ps       !! for input ps file
  integer(kind=i4b)           :: jfile_p        !! for input pressure file
  integer(kind=i4b)           :: jfile_os       !! for output file
  integer(kind=i4b)           :: jfile_ot       !! for output file
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
  !c+++ set switches
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev' ) ozlv = .true.
  if (trim(ip) /= 'NONE' ) opin = .true.
  if (ozlv.and. .not.opin) osig = .true. !! for Ps input

  !c+++ constants
  call setup_cnst

  !c+++ open input file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input tropopause file
  write(6, *) 'open input file: ', trim(ik)
  call gtopen(trim(ik), 'r', jfile_k, ios)
  if (ios /= 0) call ioerror(jfile_k, ios)

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

  !c+++ open output gtool file (strat.)
  write(6, *) 'open output file: ', trim(ofile_s)
  if (oapnd) then
    call gtopen(trim(ofile_s), 'a', jfile_os, ios)
  else
    call gtopen(trim(ofile_s), 'w', jfile_os, ios)
  endif
  if (ios /= 0) call ioerror(jfile_os, ios)
  !c+++ open output gtool file (.rop)
  write(6, *) 'open output file: ', trim(ofile_t)
  if (oapnd) then
    call gtopen(trim(ofile_t), 'a', jfile_ot, ios)
  else
    call gtopen(trim(ofile_t), 'w', jfile_ot, ios)
  endif
  if (ios /= 0) call ioerror(jfile_ot, ios)


  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)
  call gtrewind(jfile_i, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ z-axis name
  haxisz = head(35)
  write(6, *) 'haxisz = ', haxisz
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(ps(imax,jmax), ps4(imax,jmax))
  allocate(p(kmax), gdo3(imax,jmax,kmax), ktp(imax,jmax))
  allocate(pres(imax,jmax,kmax), dobsons(imax,jmax), dobsont(imax,jmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
  ps(1:imax,1:jmax) = rmiss
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
!c 3-D P output
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

    !c+++ read gdo3
    call rgt_r4(jfile_i, imax, jmax, kmax, head, gdo3, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    !c+++ read tropopause grid-id
    call rgt_r4(jfile_k, imax, jmax, 1, head2, ktp, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_k, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
    !c+++ read input Ps data
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
      call rgt_r4(jfile_p, imax, jmax, kmax, head2, pres, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_p, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa 
      if (trim(head2(16)) == 'Pa') then
        call shift(imax, jmax, kmax, rmiss, pres, pres, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'pres(1,1,1) = ', pres(1,1,1), ' (hPa)'
      endif
    else
      !c+++ set pres by set_plevs
      call set_plevs(imax, jmax, kmax, rmiss, p, ps4, eta_fa, eta_fb, pres, osig, oeta)
    endif

    !c+++ GDO3 ==> dobson
     call dobout(imax, jmax, kmax, rmiss, gdo3, ktp, pres, dobsons, dobsont)

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
    dout(1:imax,1:jmax,1) = dobsons(1:imax,1:jmax)
    call wgthdd(jfile_os, imax, jmax, 1, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_os, ios)
    dout(1:imax,1:jmax,1) = dobsont(1:imax,1:jmax)
    call wgthdd(jfile_ot, imax, jmax, 1, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_ot, ios)
    deallocate(dout)

    if (it == iend) exit
  enddo

  !c+++ deallocate
  deallocate(ps, ps4, p, gdo3, pres, dobsons, dobsont)
  deallocate(eta_fa, eta_fb)
  !c+++ close files
  call gtclose(jfile_i, ios)
  call gtclose(jfile_k, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  if (opin) call gtclose(jfile_p, ios)
  call gtclose(jfile_os, ios)
  call gtclose(jfile_ot, ios)
  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine dobout
!c=====
subroutine dobout(imax, jmax, kmax, rmiss, gdo3, ktp, pres, dobsons, dobsont)
  use common_dob, only: GR, NA, mair, DU
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r4b), intent(in)    :: gdo3(imax,jmax,kmax) !! (vmr)
  real(kind=r4b), intent(in)    :: ktp(imax,jmax)       !! ()
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! (hPa)
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dobsons(imax,jmax)   !! strat. (DU)
  real(kind=r4b), intent(out)   :: dobsont(imax,jmax)   !! trop. (DU)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j                 !!
  integer(kind=i4b)             :: k_l, k_h             !! min./max. level
  real(kind=r8b)                :: fact                 !!
  real(kind=r8b)                :: p(imax,jmax,kmax)    !!

  p(1:imax,1:jmax,1:kmax) = pres(1:imax,1:jmax,1:kmax) * 100.d0 !! hPa ==> Pa

  !c+++ vertical integration
  fact = NA / mair / DU
  do j = 1, jmax
    do i = 1, imax
      if (int(ktp(i,j)) > 0) then
        !c+++ strat.
        k_l = int(ktp(i,j))
        k_h = kmax
        call vint(kmax, k_l, k_h, rmiss, gdo3(i,j,1:kmax), p(i,j,1:kmax), dobsons(i,j), GR, fact)
        !c+++ trop.
        k_l = 1
        k_h = int(ktp(i,j)-1)
        call vint(kmax, k_l, k_h, rmiss, gdo3(i,j,1:kmax), p(i,j,1:kmax), dobsont(i,j), GR, fact)
      else
        dobsons(i,j) = rmiss
        dobsont(i,j) = rmiss
      endif
    enddo !! i
  enddo !! j

  return
end subroutine dobout

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine vint
!c=====
subroutine vint(kmax, k_l, k_h, rmiss, din, p, dout, GR, fact)
  implicit none
  !c+++ [input]
  integer(kind=i4b), intent(in) :: kmax                 !! z-size
  integer(kind=i4b), intent(in) :: k_l, k_h             !! min./max. of integral
  real(kind=r4b), intent(in)    :: din(kmax)            !!
  real(kind=r8b), intent(in)    :: p(kmax)              !!
  real(kind=r8b), intent(in)    :: GR, rmiss, fact      !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout                 !!
  !c+++ [internal work]
  integer(kind=i4b)             :: k, k1, k2            !!
  integer(kind=i4b)             :: isum                 !!
  real(kind=r8b)                :: DP                   !!

  !c+++ initialize
  dout = 0.d0
  isum = 0
  !c+++ summation
  do k = k_l, k_h
    k1 = k 
    k2 = k + 1
    if (k == kmax) cycle
    DP = - (p(k2) - p(k1)) / GR
    if (din(k1) /= rmiss.and.din(k2) /= rmiss) then
      dout = dout + 0.5d0 * (din(k1) + din(k2)) * DP
      isum = isum + 1
    endif
  enddo !! k
  !c+++ scaling
  if (isum /= 0) then
    dout = dout * fact
  else
    dout = rmiss
  endif

  return
end subroutine vint

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
  call get_parms('k', 'ktp', ik, ios)
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('p', 'NONE', ip, ios)
  call get_parms('os', 'dobsons', ofile_s, ios)
  call get_parms('ot', 'dobsont', ofile_t, ios)
  !c+++ 
  call get_parms('item', 'DOBSON', hitem, ios)
  call get_parms('titl', 'total ozone', htitl, ios)
  call get_parms('unit', 'DU', hunit, ios)
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
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtdobl '
  write(6, '(a)') '-ot output-dobson-file (trop.)'
  write(6, '(a)') '-os output-dobson-file (strat.)'
  write(6, '(a)') '-i input-gdo3-file'
  write(6, '(a)') '-k input-tropopause-file'
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
  write(6, '(a)') 'ex. strat./trop. column ozone for p-levels: '
  write(6, '(a)') '    gtdobl -i xo3_P -k ktp -ot dobsont -os dobsons'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtdobl
