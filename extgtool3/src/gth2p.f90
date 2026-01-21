!c
!c  program gth2p
!c  [history]
!c  2026/01/18 Yamashita: first ver. from gtomg2w
!c
!c  eta ==> p conversion
!c
!c  internal: module common_gth2p
!c            subroutine setup_cnst
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module calculate: subroutine dshift
!c  external: module util
!c            subroutines conv_zaxis, getzaxis, set_user, set_jdaten
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd, 
!c                        get_zaxsize,  get_etacoef
!c  external: module  error_handler
!c            subroutines ioerror, werr, werr2
!c
!c=====================================================================c
module common_gth2p
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

end module common_gth2p

!c=====================================================================c

program gth2p
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_gth2p, only: setup_cnst, h
  use dcalculate, only: dshift
  use util, only: conv_zaxis, getzaxis, set_user, set_jdaten
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd, get_zaxsize, get_etacoef
  use error_handler, only: ioerror, werr, werr2
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd & rgt_r4)
  real(kind=r8b), allocatable :: d0(:,:,:)      !! input data
  real(kind=r4b), allocatable :: d(:,:,:)       !! input data
  real(kind=r8b), allocatable :: ps0(:,:)       !! Ps (hPa)
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header
  !c+++ input from file (get_etacoef)
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax           !! x-axis sizes
  integer(kind=i4b)           :: jmax           !! y-axis sizes
  integer(kind=i4b)           :: kmax           !! z-axis sizes
  integer(kind=i4b)           :: kmax0          !! input z- axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  character(len=ncc)          :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)           :: ista, iend     !! start/end record
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: ips            !! input ps file name
  character(len=ncc)          :: ipout          !! output z-axis name
  character(len=ncc)          :: itype          !! intplt type (l: linear, s: spline)
  logical                     :: omiss          !! t: enable extrapolation, f: disable
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
  !c+++ I/O unit for input/output files
  integer(kind=i4b)           :: jfile_i        !! I/O unit for inputfile
  integer(kind=i4b)           :: jfile_ps       !! I/O unit for input ps file
  integer(kind=i4b)           :: jfile_o        !! I/O unit for output file
  integer(kind=i4b)           :: ios            !! end code
  !c+++ internal switch
  logical                     :: osig = .false. !! enable sigma-lev.
  logical                     :: oeta = .false. !! enable eta-lev.
  logical                     :: ozlv = .false. !! enable z-lev.

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  !c+++ set switches
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev' ) ozlv = .true.

  !c+++ constants
  call setup_cnst

  !c+++ open input file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input ps file
  if (osig.or.oeta.or.ozlv) then
    write(6, *) 'open input ps file: ', trim(ips)
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
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
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)
  !c+++ set item/title/unit of output data
  if (hitem == 'NULL') hitem = head(3)
  if (hunit == 'NULL') hunit = head(16)
  if (htitl == 'NULL') then
    htitlz(1:16) = head(14)
    htitlz(17:32) = head(15)
    htitl = htitlz
  endif 

  !c+++ set z-axis name
  haxisz = head(35)
  write(6, *) 'haxisz (input) = ', haxisz
  !c+++ input for sig-, eta-, and z-levs
  kmax0 = kmax
  if (osig.or.oeta.or.ozlv) then      
    if (ipout == 'NULL') call werr2('must be specified -pout option')
    call get_zaxsize(ipout, kmax) !! replace kmax by z-axis size of ipout
  else
    if (ipout == 'NULL') ipout = haxisz !! set output z-axis name
    call get_zaxsize(ipout, kmax)
  endif

  !c+++ allocate
  allocate(d0(imax,jmax,kmax), d(imax,jmax,kmax))
  allocate(ps0(imax,jmax), p(kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))
  ps0(1:imax,1:jmax) = rmiss
  eta_fa(1:kmax) = rmiss
  eta_fb(1:kmax) = rmiss

  !c+++ z-coefs.
  if (osig.or.oeta.or.ozlv) then      
    call getzaxis(kmax, ipout, p)
    !c+++ z ==> sig (for z-lev)
    if (ozlv) then
      !c+++ z = - h log(sig), -z/h = log(sig), sig = exp(-z/h)
      do k = 1, kmax
        p(k) = exp(- p(k) / h)
      enddo !! j
    endif
  else
    call getzaxis(kmax, ipout, p) !! for p-lev
  endif
  if (oeta) then
    !c+++ read z-axis file (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  endif
  write(6, *) 'haxisz (output) = ', ipout
  write(6, *) 'kmax = ', kmax
  write(6, *) 'output pressure lev = ', p

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
      cycle
    endif

    !c+++ read input Ps data
    if (osig.or.oeta) then
      call rgt(jfile_ps, imax, jmax, 1, head2, ps0, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, 1, rmiss, ps0, ps0, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps0(1,1), ' (hPa)'
      endif
    endif
 
    !c+++ read input data
    call rgt(jfile_i, imax, jmax, kmax0, head, d0, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    !c+++ v0 ==> v (interpolate or copy)
    if (osig.or.oeta) then !! for sig- and eta-levs
      call conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, d0, d, &
&       itype, omiss, osig, oeta)
    else !! for p-levs
      d(1:imax,1:jmax,1:kmax) = d0(1:imax,1:jmax,1:kmax)
    endif

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit
    head(35) = ipout
    write(head(37), '(i16)') kmax
    write(head(64), '(i16)') imax*jmax*kmax

    !c+++ write gtool3 header & data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, d, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo !! it

  !c+++ deallocate
  deallocate(d, d0, ps0, eta_fa, eta_fb)
  !c+++ close files
  call gtclose(jfile_i, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
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
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
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
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  !c+++ axis
  call get_parms('zax', 'etalv', zax, ios)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine xabort
!c=====
subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gth2p -i input-file -o output-file'
  write(6, '(a)') '(-ps input-ps-file, -pout output-z-axis-name)'
  write(6, '(a)') '(-pint linear/spline, -pintext t/f)'
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
  write(6, '(a)') '-zax etalv/siglv/zlev/plev'
  write(6, '(a)') 'etalv: assume eta-levels'
  write(6, '(a)') 'siglv: assume sigma-levels'
  write(6, '(a)') 'zlev: assume z-levels'
  write(6, '(a)') '(-ps input-ps-file) for etalv/siglv/zlev/plev'

  write(6, '(a)') '-zax plev: assume p-levels (default), and ignore -ps, -pout options'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-pint linear: linear interpolation'
  write(6, '(a)') '-pint spline: spline interpolation'
  write(6, '(a)') '-pintext t: enable extrapolation'
  write(6, '(a)') '-pintext f: disable'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gth2p
