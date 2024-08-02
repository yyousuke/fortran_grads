!c
!c  program gtdob
!c  [history]
!c  2013/09/21 Yamashita: first ver.
!c  2017/05/08 Yamashita: modify getparms & I/O 
!c  2017/05/08 Yamashita: use common_typedef & common_const
!c  2017/08/04 Yamashita: use setup_cnst
!c
!c  output total ozone [DU] from O3 vmr for p-, sig-, and eta-levels
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
!c  external: module calculate
!c            subroutines shift
!c  external: module dcalculate
!c            subroutines dshift
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

program gtdob
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
  !c+++ input data (rgt, rgt_r4)
  real(kind=r4b), allocatable :: gdo3(:,:,:)    !! [vmr]
  real(kind=r8b), allocatable :: ps(:,:)        !! Ps[hPa]
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
  character(len=ncc)          :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)           :: ista           !! start record
  integer(kind=i4b)           :: iend           !! end record
  integer(kind=i4b)           :: k1, k2         !! min./max. level
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
  !c+++ output data
  real(kind=r4b), allocatable :: dout(:,:,:)    !! [DU]
  !c+++ [internal work]
  integer(kind=i4b)           :: it, k          !!
  integer(kind=i4b)           :: ios            !! end code
  character(len=ncc*2)        :: htitlz         !!
  !c+++ data
  real(kind=r4b), allocatable :: pres(:,:,:)    !! [hPa]
  real(kind=r4b), allocatable :: ps4(:,:)       !! Ps[hPa]
  real(kind=r4b), allocatable :: dobson(:,:)    !! [DU]
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input file
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
  !c+++ set z-axis name
  haxisz = head(35)
  write(6, *) 'haxisz = ', haxisz
  !c+++ vertical range
  if (k1 == -1) k1 = 1
  if (k2 == -1) k2 = kmax
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(ps(imax,jmax), ps4(imax,jmax))
  allocate(p(kmax), gdo3(imax,jmax,kmax))
  allocate(pres(imax,jmax,kmax), dobson(imax,jmax))
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
    call dobout(imax, jmax, kmax, k1, k2, rmiss, gdo3, pres, dobson)

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
    dout(1:imax,1:jmax,1) = dobson(1:imax,1:jmax)
    call wgthdd(jfile_o, imax, jmax, 1, head, hdfmt, dout, ios)
    deallocate(dout)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo

  !c+++ deallocate
  deallocate(ps, ps4, p, gdo3, pres, dobson)
  deallocate(eta_fa, eta_fb)
  !c+++ close files
  call gtclose(jfile_i, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  if (opin) call gtclose(jfile_p, ios)
  call gtclose(jfile_o, ios)
  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine dobout
!c=====
subroutine dobout(imax, jmax, kmax, k_l, k_h, rmiss, gdo3, pres, dobson)
  use common_dob, only: GR, NA, mair, DU
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: k_l, k_h             !! min./max. level
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r4b), intent(in)    :: gdo3(imax,jmax,kmax) !! (vmr)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! (hPa)
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dobson(imax,jmax)    !! (DU)
  !c+++ [internal work]
  real(kind=r8b)                :: fact                 !!
  real(kind=r8b)                :: p(imax,jmax,kmax)    !!

  p(1:imax,1:jmax,1:kmax) = pres(1:imax,1:jmax,1:kmax) * 100.d0 !! hPa ==> Pa

  !c+++ vertical integration
  fact = NA / mair / DU
  call vint(imax, jmax, kmax, k_l, k_h, rmiss, gdo3, p, dobson, GR, fact)

  return
end subroutine dobout

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine vint
!c=====
subroutine vint(imax, jmax, kmax, k_l, k_h, rmiss, din, p, dout, GR, fact)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, x-, z-sizes
  integer(kind=i4b), intent(in) :: k_l, k_h             !! min./max. of integral
  real(kind=r4b), intent(in)    :: din(imax,jmax,kmax)  !!
  real(kind=r8b), intent(in)    :: p(imax,jmax,kmax)    !!
  real(kind=r8b), intent(in)    :: GR, rmiss, fact      !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(imax,jmax)      !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, k1, k2      !!
  integer(kind=i4b)             :: isum(imax,jmax)      !!
  real(kind=r8b)                :: DP                   !!

  !c+++ initialize
  dout(1:imax,1:jmax) = 0.d0
  isum(1:imax,1:jmax) = 0
  !c+++ summation
  do k = k_l, k_h
    k1 = k 
    k2 = k + 1
    if (k == kmax) cycle
    do j = 1, jmax
      do i = 1, imax
        DP = - (p(i,j,k2) - p(i,j,k1)) / GR
        if (din(i,j,k1) /= rmiss.and.din(i,j,k2) /= rmiss) then
          dout(i,j) = dout(i,j) + 0.5d0 * (din(i,j,k1) + din(i,j,k2)) * DP
          isum(i,j) = isum(i,j) + 1
        endif
      enddo !! i
    enddo !! j
  enddo !! k
  !c+++ scaling
  do j = 1, jmax
    do i = 1, imax
      if (isum(i,j) /= 0) then
        dout(i,j) = dout(i,j) * fact
      else
        dout(i,j) = rmiss
      endif
    enddo !! i
  enddo !! j

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
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('p', 'NONE', ip, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'DOBSON', hitem, ios)
  call get_parms('titl', 'total ozone', htitl, ios)
  call get_parms('unit', 'DU', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ min./max. levs.
  call get_parms('k1', '-1', hval, ios)
  call c2var(k1, '(i4)', hval)
  call get_parms('k2', '-1', hval, ios)
  call c2var(k2, '(i4)', hval)
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
  write(6, '(a)') 'gtdob -o output-dobson-file'
  write(6, '(a)') '-i input-gdo3-file'
  write(6, '(a)') '-k1 min-lev -k2 max-lev'
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
  write(6, '(a)') 'ex. Total ozone for p-levels: '
  write(6, '(a)') '    gtdob -i xo3_P -o dobson'
  write(6, '(a)') ' '
  write(6, '(a)') 'ex. Ozone partial column for p-levels:'
  write(6, '(a)') '    gtdob -i xo3_P -o dobson -k1 9 -k2 10'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtdob
