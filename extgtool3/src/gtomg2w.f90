!c
!c  program gtomg2w
!c  [history]
!c  2013/10/10 Yamashita: first ver.
!c  2016/01/28 Yamashita: modify gtopen and remove seek_iounit
!c  2017/07/26 Yamashita: modify getparms & I/O 
!c  2017/07/26 Yamashita: use common_typedef & common_const
!c  2017/08/04 Yamashita: use setup_cnst
!c
!c  omg ==> w conversion
!c
!c  internal: module common_omg2w
!c            subroutine setup_cnst
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module calculate: subroutine shift
!c  external: module util
!c            subroutines omgp2w, getzaxis, set_plevs, set_user, set_jdaten
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgthdd, get_etacoef
!c  external: module  error_handler
!c            subroutines ioerror, werr
!c
!c=====================================================================c
module common_omg2w
  use common_typedef, only: r8b
  use common_const, only: cnst_h, const_setup
  !c+++ [parameter]
  real(kind=r8b), save          :: h                !! scale height [m]

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    h = cnst_h
  end subroutine setup_cnst
!c----------------------------------------------------------------------c

end module common_omg2w

!c=====================================================================c

program gtomg2w
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_omg2w, only: setup_cnst, h
  use calculate, only: shift
  use util, only: omgp2w, getzaxis, set_plevs, set_user, set_jdaten
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgthdd, get_etacoef
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd & rgt_r4)
  real(kind=r4b), allocatable :: omg(:,:,:)     !! omg [hPa/s]
  real(kind=r4b), allocatable :: ps(:,:)        !! Ps[hPa]
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header (ps)
  !c+++ input from file (get_etacoef)
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax           !! x-axis sizes
  integer(kind=i4b)           :: jmax           !! y-axis sizes
  integer(kind=i4b)           :: kmax           !! z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)           :: ista, iend     !! start/end record
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: ips            !! input ps file name
  character(len=nfiln)        :: ip             !! input pressure file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  character(len=ncc)          :: zax            !! zaxis plev/zlv/siglv/etalv
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  real(kind=r4b), allocatable :: w(:,:,:)       !!
  !c+++ [work]
  integer(kind=i4b)           :: it, k          !!
  character(len=ncc*2)        :: htitlz         !!
  real(kind=r4b), allocatable :: pres(:,:,:)    !! [hPa]
  !c+++ I/O unit for input/output files
  integer(kind=i4b)           :: jfile_i        !! I/O unit for inputfile
  integer(kind=i4b)           :: jfile_ps       !! I/O unit for input ps file
  integer(kind=i4b)           :: jfile_p        !! I/O unit for input pressure file
  integer(kind=i4b)           :: jfile_o        !! I/O unit for output file
  integer(kind=i4b)           :: ios            !! end code
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

  !c+++ open output w file
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
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(omg(imax,jmax,kmax), w(imax,jmax,kmax))
  allocate(pres(imax,jmax,kmax))
  allocate(ps(imax,jmax), p(kmax))
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

    !c+++ read input omg data
    call rgt_r4(jfile_i, imax, jmax, kmax, head, omg, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ read input Ps data
    if (osig.or.oeta) then
      call rgt_r4(jfile_ps, imax, jmax, 1, head2, ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call shift(imax, jmax, 1, rmiss, ps, ps, 0.01d0, 0.d0)
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
        call shift(imax, jmax, kmax, rmiss, pres, pres, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'pres(1,1,1) = ', pres(1,1,1), ' (hPa)'
      endif
    else
      !c+++ set pres by set_plevs
      call set_plevs(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pres, osig, oeta)
    endif

    !c+++ omg[hPa/s] ==> w[m/s]
    call omgp2w(imax, jmax, kmax, rmiss, omg, w, pres, h)

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit

    !c+++ write gtool3 header & data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, w, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo !! it

  !c+++ deallocate
  deallocate(omg, w, pres, ps, p, eta_fa, eta_fb)
  !c+++ close files
  call gtclose(jfile_i, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  if (opin) call gtclose(jfile_p, ios)
  call gtclose(jfile_o, ios)
  stop

contains

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
  call get_parms('item', 'W', hitem, ios)
  call get_parms('titl', 'w-velocity', htitl, ios)
  call get_parms('unit', 'm/s', hunit, ios)
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
  write(6, '(a)') 'gtomg2w -i input-file -o output-file'
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

end program gtomg2w
