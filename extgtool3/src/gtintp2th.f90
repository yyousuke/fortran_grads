!c
!c  program gtintp2th (old name: thintp)
!c  [history]
!c  2010/07/28 Yamashita: first ver. with p==>theta (from thiintp.f by Sugata & Yoshiki)
!c  2013/03/03 Yamashita: f77==>f90
!c  2013/03/03 Yamashita: use gtool header to determine array size
!c  2013/03/11 Yamashita: use modules
!c  2013/10/22 Yamashita: add 480K and 840K levels
!c  2013/10/22 Yamashita: remove subroutine varont and use module util
!c  2013/10/22 Yamashita: add gtopen, set_user, set_jdaten
!c
!c  output variables on theta-lev. with theta-file input
!c  input: 
!c         th: 3-D theta input (K)
!c         uu: variables (zaxis must be the same as theta file)
!c  output: 
!c         u2: variables on theta-lev.
!c
!c
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module util_pvont
!c            subroutine varont
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtskip, gtrewind, rgt_r4, rgthd, wgthdd
!c  external: module  error_handler
!c            subroutines ioerror, werr
!c
!c=====================================================================c
module common_p2th
  use common_args, only: ncc, ndc
  use common_typedef, only: i4b, r4b

  !c+++ [parameters]
  integer(kind=i4b), parameter  :: ithini = 32
  character(len=ncc), parameter :: caxis = 'THETA32'

  real(kind=r4b)                :: thlevi(ithini) !! reference theta level
  data thlevi /  330.,  340.,  350.,  380.,  400., &
&               425.,  450.,  480.,  500.,  550., &
&               600.,  650.,  700.,  750.,  800., &
&               840.,  850.,  900.,  950., 1000., &
&              1100., 1200., 1400., 1600., 1800., &
&              2000., 2200., 2400., 2600., 2900., &
&              3300., 3800./

end module common_p2th

!c=====================================================================c

program gtintp2th
  use common_args, only: ncc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_p2th
  use util_pvont, only: varont, varonp
  use rwgtool, only: gtopen, gtclose, gtskip, gtrewind, rgt_r4, rgthd, wgthdd, get_axis
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  character(len=nfiln)        :: ifile        !! input file name
  character(len=nfiln)        :: ith          !! input potential temp. file name
  character(len=nfiln)        :: ofile        !! output file name
  character(len=ncc)          :: hdfmt        !! data format
  character(len=ncc)          :: hitem        !! new item of output data
  character(len=ncc*2)        :: htitl        !! new title of output data
  character(len=ncc)          :: hunit        !! new unit of output data
  character(len=ncc)          :: thaxis       !! new axis name of output data
  integer(kind=i4b)           :: ithlev       !! theta-axis size
  integer(kind=i4b)           :: ista         !! start record
  integer(kind=i4b)           :: iend         !! end record
  logical                     :: oapnd        !! t: append, f: replace
  logical                     :: oth2p        !! f: p->theta, t: theta->p
  !c+++ input from file (rgthd & rgt_r4)
  character(len=ncc)          :: head(ndc)    !! gtool3 header
  character(len=ncc)          :: headth(ndc)  !! gtool3 header of theta
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax         !! x-axis sizes
  integer(kind=i4b)           :: jmax         !! y-axis sizes
  integer(kind=i4b)           :: kmax         !! z-axis sizes
  real(kind=r8b)              :: rmiss        !! missing value
  !c+++ input data (rgt_r4)
  real(kind=r4b), allocatable :: uu(:,:,:)    !! din
  real(kind=r4b), allocatable :: th(:,:,:)    !! theta (K)
  !c+++ input data (get_axis)
  real(kind=r4b), allocatable :: thlev(:)     !! reference theta level
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: u2(:,:,:)    !! 温位面上のu
  !c+++ [work]
  integer(kind=i4b)           :: it, k
  integer(kind=i4b)           :: ios          !! end code
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i      !! for input file
  integer(kind=i4b)           :: jfile_o      !! for output file
  integer(kind=i4b)           :: jfile_th     !! I/O unit for input theta file
  !c+++ title
  character(len=ncc*2)        :: htitlz       !!


!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  !c+++ set output theta axis
  if (trim(thaxis) == 'NONE' ) then
    allocate(thlev(ithini))
    thaxis = caxis
    thlev(1:ithini) = thlevi(1:ithini)
  else
    allocate(thlev(ithlev))
    call get_axis(ithlev, thaxis, thlev)
  endif

  !c+++ open input file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input potential temp. file
  write(6, *) 'open input potential temp. file: ', trim(ith)
  call gtopen(trim(ith), 'r', jfile_th, ios)
  if (ios /= 0) call ioerror(jfile_th, ios)

  !c+++ open output file
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
  !c+++ allocate
  allocate(uu(imax,jmax,kmax))
  allocate(th(imax,jmax,kmax))
  allocate(u2(imax,jmax,ithlev))

!c
!c p ==> theta
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      call gtskip(jfile_th, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_th, ios)
      cycle
    endif

    !c+++ read input data
    call rgt_r4(jfile_i, imax, jmax, kmax, head, uu, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ read input theta data
    call rgt_r4(jfile_th, imax, jmax, kmax, headth, th, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_th, ios)
    if (head(27) /= headth(27)) call werr(head(27), headth(27), 'Dates are not match!')
    if (head(35) /= headth(35)) call werr(head(35), headth(35), 'z-axis is not match!')

    if (.not. oth2p) then
      !c+++ _P ==> _TH
      do k = 1, ithlev
        call varont(uu, th, thlev(k), u2(1,1,k), real(rmiss), imax, jmax, kmax)
      enddo !! k
    else
      !c+++ _TH ==> _P
      do k = 1, ithlev
        call varonp(uu, th, thlev(k), u2(1,1,k), real(rmiss), imax, jmax, kmax)
      enddo !! k
    endif

    !c+++ set data format of output data
    if (hdfmt == 'NULL') hdfmt = head(38)
    !c+++ set unit of output data
    if (hunit /= 'NULL') head(16) = hunit
    !c+++ set item of output data
    if (hitem /= 'NULL') head(3) = hitem
    !c+++ set title of output data
    if (htitl /= 'NULL') then
      htitlz = trim(head(14))//trim(head(15))//' (THETA)'
      head(14) = htitlz(1:16)
      head(15) = htitlz(17:32)
    endif
    !c+++ set output header (axis)
    head(35) = thaxis
    write(head(37), '(i16)') ithlev
    write(head(64), '(i16)') imax*jmax*ithlev

    !c+++ write output data
    call wgthdd(jfile_o, imax, jmax, ithlev, head, hdfmt, u2, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo

  !c+++ close files
  call gtclose(jfile_i, ios)
  call gtclose(jfile_th, ios)
  call gtclose(jfile_o, ios)
  !c+++ deallocate
  deallocate(uu, th, u2)
  stop

contains

!c=====================================================================c

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
  !c+++ input/output file
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('th', 'THETA_P', ith, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  !c+++ theta axis
  call get_parms('ath', 'NONE', thaxis, ios)
  call get_parms('thlev', '32', hval, ios)
  call c2var(ithlev, '(i4)', hval)
  !c+++ theta -> p 
  call get_parms('th2p', 'f', hval, ios)
  call c2var(oth2p, '(l1)', hval)
  if (oth2p) then
    call get_parms('ap', 'GPLV31', thaxis, ios)
    call get_parms('plev', '31', hval, ios)
    call c2var(ithlev, '(i4)', hval)
  endif
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: gtintp2th'
  write(6, '(a)') '-i input-file -o output-file'
  write(6, '(a)') '-th input-theta-file (THETA on P)'
  write(6, '(a)') ' '
  write(6, '(a)') 'p -> theta conversion'
  write(6, '(a)') 'z-axis of input-file must be the same as input-theta-file'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'optional: '
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'optional: output theta-level'
  write(6, '(a)') '-ath theta-axis-name of output (default: THETA32)'
  write(6, '(a)') '-thlev theta-axis-size of output (default: 32)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'optional: theta -> p conversion'
  write(6, '(a)') '-th2p t (default: f)'
  write(6, '(a)') '-ap theta-axis-name of output (default: GPLV31)'
  write(6, '(a)') '-th input-theta-file (P on THETA)'
  write(6, '(a)') '-plev theta-axis-size of output (default: 31)'
  write(6, '(a)') '------------------------------------------'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtintp2th
