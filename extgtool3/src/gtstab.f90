!c
!c  gtstab.f90
!c  [history]
!c  2013/09/21 Yamashita: first ver.
!c  2016/01/28 Yamashita: add gtopen
!c  2017/07/25 Yamashita: modify getparms & I/O 
!c  2017/07/25 Yamashita: use common_typedef & common_const
!c
!c  THETA(p1)-THETA(p2) for p-, sig-, and eta-levels
!c
!c======================================================================c
program gtstab
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use dcalculate, only: dshift
  use util, only: conv_zaxis
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt, wgthdd
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [internal parameters]
  integer(kind=i4b), parameter :: oplev = 5      !! num. of p-levs.
  !c+++ [input]
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)           :: head(ndc)      !! gtool3 header
  character(len=ncc)           :: head2(ndc)     !! gtool3 header (ps)
  real(kind=r8b), allocatable  :: TH0(:,:,:)     !! TH [K]
  real(kind=r8b), allocatable  :: Ps0(:,:)       !! Ps [hPa]
  !c+++ input from gtool3 header
  integer(kind=i4b)            :: imax           !! x-axis sizes
  integer(kind=i4b)            :: jmax           !! y-axis sizes
  integer(kind=i4b)            :: kmax           !! z-axis sizes
  real(kind=r8b)               :: rmiss          !! missing value
  character(len=ncc)           :: haxisz         !! z-axis name
  !c+++ input from getparms
  integer(kind=i4b)            :: ista, iend     !! start/end record
  character(len=nfiln)         :: ifile          !! input file name
  character(len=nfiln)         :: ips            !! input ps file name
  character(len=nfiln)         :: ofile          !! output file name
  character(len=ncc)           :: hitem          !! item
  character(len=ncc*2)         :: htitl          !! title
  character(len=ncc)           :: hunit          !! unit
  character(len=ncc)           :: hdfmt          !! data format
  character(len=ncc)           :: itype          !! intplt type (l:linear, s:spline)
  character(len=ncc)           :: zax            !! zaxis plev/siglv/etalv
  real(kind=r8b)               :: p1, p2         !! TH(p1)-TH(p2)
  logical                      :: oapnd          !! t: append, f: replace
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable  :: dout(:,:,:)    !! TH(700hPa)-TH(1000hPa) [K]
  !c+++ [internal work]
  integer(kind=i4b)            :: i, j, k, it    !! loop variables
  integer(kind=i4b)            :: k1, k2         !! k-levs.
  character(len=ncc*2)         :: htitlz         !!
  real(kind=r4b), allocatable  :: TH(:,:,:)      !! TH [K]
  real(kind=r4b), allocatable  :: stab(:,:)      !! TH(700hPa)-TH(1000hPa) [K]
  !c+++ I/O unit for input/output files
  integer(kind=i4b)            :: jfile_i        !! I/O unit for inputfile
  integer(kind=i4b)            :: jfile_ps       !! I/O unit for input ps file
  integer(kind=i4b)            :: jfile_o        !! I/O unit for output file
  integer(kind=i4b)            :: ios            !! end code
  !c+++ internal switch
  logical                      :: osig = .false. !! enable sigma-lev.
  logical                      :: oeta = .false. !! enable eta-lev.
  !c+++ reference p-levels
  real(kind=r8b)               :: plev(oplev)    !!
  data plev / 1000.d0, 850.d0, 700.d0, 500.d0, 200.d0 /

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  !c+++ set switches
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.

  !c+++ set levs
  k1 = 0
  k2 = 0
  do k = 1, oplev
    if (plev(k) == p1) k1 = k
    if (plev(k) == p2) k2 = k
  enddo !! k
  write(6, *) 'k1, k2 = ', k1, k2
  if (k1 == 0.or.k2 == 0) stop 2

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
  allocate(TH0(imax,jmax,kmax), Ps0(imax,jmax))
  allocate(TH(imax,jmax,oplev))
  allocate(stab(imax,jmax))

!c
!c theta(k1) - theta(k2)
!c===
  ps0(1:imax,1:jmax) = 0.0
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
      cycle
    endif

    !c+++ read temperature
    call rgt(jfile_i, imax, jmax, kmax, head, TH0, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ read surface pressure for sig-lev. and eta-lev.
    if (osig.or.oeta) then
      call rgt(jfile_ps, imax, jmax, 1, head2, Ps0, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, 1, rmiss, Ps0, Ps0, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps0(1,1), ' (hPa)'
      endif
    endif

    !c+++ TH0 ==> TH (interpolate)
    call conv_zaxis(imax, jmax, kmax, oplev, rmiss, haxisz, plev, Ps0, TH0, TH, &
&     itype, .false., osig, oeta)

    !c+++ theta(k1) - theta(k2)
    do j = 1, jmax
      do i = 1, imax
        if (TH(i,j,k1) /= rmiss.and.TH(i,j,k2) /= rmiss) then
          stab(i,j) = TH(i,j,k1) - TH(i,j,k2)
        else
          stab(i,j) = rmiss
        endif
      enddo !! i
    enddo !! j

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit
    head(35) = 'SFC1'
    write(head(37), '(i16)') 1
    write(head(64), '(i16)') imax*jmax

    !c+++ write potential temperature diff.
    allocate(dout(imax,jmax,1))
    dout(1:imax,1:jmax,1) = stab(1:imax,1:jmax)
    call wgthdd(jfile_o, imax, jmax, 1, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)
    deallocate(dout)

    if (it == iend) exit
  enddo

  !c+++ deallocate
  deallocate(TH0, Ps0, TH, stab)
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
  character(len=nfiln)          :: ipint

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output files
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'THETA', hitem, ios)
  call get_parms('titl', 'potentital temp.', htitl, ios)
  call get_parms('unit', 'K', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ intplt type
  call get_parms('pint', 'spline', ipint, ios)
  if (ipint == 'linear') itype = 'l' !! intplt type; l: linear
  if (ipint == 'spline') itype = 's' !! intplt type; s: spline
  !c+++ fact & offset
  call get_parms('p1', '700.d0', hval, ios)
  call c2var(p1, '(1pe12.5)', hval)
  call get_parms('p2', '1000.d0', hval, ios)
  call c2var(p2, '(1pe12.5)', hval)
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
  write(6, '(a)') 'gtstab -i input-file -o output-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-p1 plev1 -p2 plev2'
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
  write(6, '(a)') '-pint linear/spline'
  write(6, '(a)') '-pint linear: linear interpolation'
  write(6, '(a)') '-pint spline: spline interpolation'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-zax plev/siglv/etalv'
  write(6, '(a)') 'plev: assume p-levels (default), and ignore -ps option'
  write(6, '(a)') 'siglv: assume sigma-levels, etalv: assume eta-levels'
  write(6, '(a)') '------------------------------------------'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtstab
