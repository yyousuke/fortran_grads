!c
!c  module util
!c  [history]
!c  2013/09/21 Yamashita: first ver.
!c  2013/09/26 Yamashita: add area_ave, werr
!c  2013/09/27 Yamashita: add get_time, werr2
!c  2013/10/18 Yamashita: add putzaxis
!c  2013/10/18 Yamashita: add get_axis and modify getyaxis, getzaxis
!c  2013/10/18 Yamashita: add conv_r8_yrev and conv_r8_zrev
!c  2016/11/09 Yamashita: add shift_xgrids
!c  2016/12/06 Yamashita: modify wmsg
!c  2017/04/17 Yamashita: add conv_r4_endian, conv_r4_endian
!c  2017/04/18 Yamashita: shift_xgrids to shift_r4_xgrids, shift_r8_xgrids 
!c  2017/04/18 Yamashita: shift_yrev to shift_r4_yrev, shift_r8_yrev
!c  2017/04/18 Yamashita: shift_zrev to shift_r4_zrev, shift_r8_zrev
!c  2017/04/18 Yamashita: add interfaces (shift_xgrids, shift_yrev, shift_zrev, conv_endian)
!c  2017/05/10 Yamashita: remove werr, werr2
!c  2017/05/13 Yamashita: remove wmsg
!c  2017/05/15 Yamashita: area_ave into area_ave, darea_ave
!c  2017/05/15 Yamashita: remove get_zaxsize, get_axis, get_etacoef
!c  2017/05/31 Yamashita: use common_typedef
!c  2017/07/15 Yamashita: add getxaxis
!c  2017/07/25 Yamashita: add get_data from util_gt2nc.f90 (2013/07/17 get_data)
!c  2017/07/25 Yamashita: add get_data2 from util_gt2nc.f90 (2015/02/04 get_date2)
!c  2017/07/25 Yamashita: add repmis_r4, repmis_r8 from util_gt2nc.f90 (2015/02/04 repmis)
!c  2017/07/26 Yamashita: bug fix of subroutine conv_zaxis (save)
!c  2017/07/26 Yamashita: add omgp2w
!c  2018/04/05 Yamashita: add set_pres, set_plev_r4, set_plev_r8
!c
!c
!c  public: interface set_pres: set plevs from input p-/z-/sig-/eta-levels
!c    internal subroutine: set_pres_r4: float
!c      external get_etacoef: get eta_fa and eta_fb from eta-axis information file
!c      internal subroutine: set_plev_r4: float ver. of set_plevs
!c    internal subroutine: set_pres_r8: double
!c      external get_etacoef: get eta_fa and eta_fb from eta-axis information file
!c      internal subroutine: set_plev_r8: double ver. of set_plevs
!c
!c  public: set_plevs: set plevs from input p-/sig-/eta-levels (old style)
!c  public: area_ave: area average (float)
!c  public: darea_ave: area average (double)
!c  public: omgp2w: convert from omg (hPa/s), p(hPa) into w (m/s)
!c  public: conv_zaxis: convert from sig-/eta-levels into p-levels
!c    external get_etacoef: get eta_fa and eta_fb from eta-axis information file
!c  public: get_axname: get x-, y-, z-axis names from gtool3 header
!c  public: getxaxis: get x-axis points from axis information file
!c    external get_axis: get axis points from axis information file
!c  public: getyaxis: get y-axis points from axis information file
!c    external get_axis: get axis points from axis information file
!c  public: getzaxis: get z-axis points from axis information file
!c    external get_axis: get axis points from axis information file
!c  public: get_time: get_time: read gtool3 header and get time
!c  public: putzaxis: write new z-axis information file
!c  public: get_date: set current time (YYYY-MM-DD HH:MM:SS)
!c  public: get_date2: set current time (YYYY-MM-DD-THH:MM:SSZ)
!c  public: subroutine: repmis_r4: replace missing values (float)
!c  public: subroutine: repmis_r8: replace missing values (double)
!c  public: set_jdaten: set new time header information
!c  public: set_user: modify user information
!c
!c  public: interface shift_xgrids: shift x-grids
!c    internal subroutine: shift_r4_xgrids: float
!c    internal subroutine: shift_r8_xgrids: double
!c  public: interface conv_yrev: reverse y-grids
!c    internal subroutine: conv_r4_yrev: float
!c    internal subroutine: conv_r8_yrev: double
!c  public: interface conv_zrev: reverse z-grids
!c    internal subroutine: conv_r4_zrev: float
!c    internal subroutine: conv_r8_zrev: double
!c  public: conv_endian: change endian
!c    internal subroutine: conv_r4_endian: float
!c      external: convend: change endian
!c    internal subroutine: conv_r8_endian: double
!c      external: convend: change endian
!c
!c======================================================================c
module util
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i2b, i4b, r4b, r8b
  use ysys
  implicit none
  private
  public :: set_pres, set_plevs, area_ave, darea_ave, omgp2w, conv_zaxis
  public :: get_axname, getxaxis, getyaxis, getzaxis, putzaxis
  public :: get_time, get_date, get_date2, repmis_r4, repmis_r8
  public :: set_jdaten, set_user
  public :: shift_xgrids, conv_yrev, conv_zrev, conv_endian

!c======================================================================c

!c---------------------------------------------------------------------c
!c  interface set_pres
!c  set plevs from input p-/z-/sig-/eta-levels
interface set_pres
  module procedure &
&   set_pres_r4, set_pres_r8
end interface set_pres

!c---------------------------------------------------------------------c
!c  interface shift_xgrids
!c  shift x-grids
!c=====
interface shift_xgrids
  module procedure &
&   shift_r4_xgrids, shift_r8_xgrids
end interface shift_xgrids

!c---------------------------------------------------------------------c
!c  interface conv_yrev
!c  reverse y-grids
!c=====
interface conv_yrev
  module procedure &
&   conv_r4_yrev, conv_r8_yrev
end interface conv_yrev 

!c---------------------------------------------------------------------c
!c  interface conv_zrev
!c  reverse z-grids
!c=====
interface conv_zrev
  module procedure &
&   conv_r4_zrev, conv_r8_zrev
end interface conv_zrev 

!c---------------------------------------------------------------------c
!c  interface conv_endian
!c  reverse z-grids
!c=====
interface conv_endian
  module procedure &
&   conv_r4_endian, conv_r8_endian
end interface conv_endian

!c======================================================================c

  !c+++ [common]
  !c+++ [internal parameter]
  real(kind=r8b), parameter, private :: eps = 1d-10

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine set_pres_r4
!c  set plevs from input p-/z-/sig-/eta-levels
!c=====
subroutine set_pres_r4(imax, jmax, kmax, rmiss, haxisz, h, ps, pres, osig, oeta, ozlv)
  use rwgtool, only: get_etacoef
  use dcalculate, only: dshift
  !c+++ [input]
  integer(kind=i4b), intent(in)     :: imax                 !! x-axis size
  integer(kind=i4b), intent(in)     :: jmax                 !! y-axis size
  integer(kind=i4b), intent(in)     :: kmax                 !! output z-axis size
  real(kind=r4b), intent(in)        :: rmiss                !! undefined value
  character(len=*), intent(in)      :: haxisz               !! z-axis name
  real(kind=r4b), intent(in)        :: h                    !! scale height [m]
  real(kind=r4b), intent(in)        :: ps(imax,jmax)        !! input surface pressure data (hPa)
  logical, intent(in)               :: osig                 !! enable sigma-lev.
  logical, intent(in)               :: oeta                 !! enable eta-lev.
  logical, intent(in)               :: ozlv                 !! enable z-lev.
  !c+++ [output]
  real(kind=r4b), intent(out)       :: pres(imax,jmax,kmax) !! [hPa]
  !c+++ [internal work]
  integer(kind=i4b)                 :: k                    !!
  !c+++ [internal save]
  logical, save                     :: ofirst = .true.      !!
  !c+++ vertical axis
  real(kind=r8b), allocatable, save :: p(:)                 !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable, save :: eta_fa(:)            !! input eta half lev.
  real(kind=r8b), allocatable, save :: eta_fb(:)            !! input eta half lev.

  !c+++ one time
  if (ofirst) then
    ofirst = .false.
    allocate(p(kmax), eta_fa(kmax), eta_fb(kmax))
    p(1:kmax) = rmiss
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
      write(6, *) 'p = ', p
    else
      !c+++ read z-axis file (for eta-lev)
      call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
      write(6, *) 'eta_fa = ', eta_fa
      write(6, *) 'eta_fb = ', eta_fb
    endif
  endif

  !c+++  set plevs from input p-/sig-/eta-levels
  call set_plev_r4(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pres, osig, oeta)

  return
end subroutine set_pres_r4

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine set_pres_r8
!c  set plevs from input p-/z-/sig-/eta-levels
!c=====
subroutine set_pres_r8(imax, jmax, kmax, rmiss, haxisz, h, ps, pres, osig, oeta, ozlv)
  use rwgtool, only: get_etacoef
  use dcalculate, only: dshift
  !c+++ [input]
  integer(kind=i4b), intent(in)     :: imax                 !! x-axis size
  integer(kind=i4b), intent(in)     :: jmax                 !! y-axis size
  integer(kind=i4b), intent(in)     :: kmax                 !! output z-axis size
  real(kind=r8b), intent(in)        :: rmiss                !! undefined value
  character(len=*), intent(in)      :: haxisz               !! z-axis name
  real(kind=r8b), intent(in)        :: h                    !! scale height [m]
  real(kind=r8b), intent(in)        :: ps(imax,jmax)        !! input surface pressure data (hPa)
  logical, intent(in)               :: osig                 !! enable sigma-lev.
  logical, intent(in)               :: oeta                 !! enable eta-lev.
  logical, intent(in)               :: ozlv                 !! enable z-lev.
  !c+++ [output]
  real(kind=r8b), intent(out)       :: pres(imax,jmax,kmax) !! [hPa]
  !c+++ [internal work]
  integer(kind=i4b)                 :: k                    !!
  !c+++ [internal save]
  logical, save                     :: ofirst = .true.      !!
  !c+++ vertical axis
  real(kind=r8b), allocatable, save :: p(:)                 !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable, save :: eta_fa(:)            !! input eta half lev.
  real(kind=r8b), allocatable, save :: eta_fb(:)            !! input eta half lev.

  !c+++ one time
  if (ofirst) then
    ofirst = .false.
    allocate(p(kmax), eta_fa(kmax), eta_fb(kmax))
    p(1:kmax) = rmiss
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
      write(6, *) 'p = ', p
    else
      !c+++ read z-axis file (for eta-lev)
      call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
      write(6, *) 'eta_fa = ', eta_fa
      write(6, *) 'eta_fb = ', eta_fb
    endif
  endif

  !c+++  set plevs from input p-/sig-/eta-levels
  call set_plev_r8(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pres, osig, oeta)

  return
end subroutine set_pres_r8

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine set_plev_r4
!c  set plevs from input p-/sig-/eta-levels (float ver. of set_plevs)
!c=====
subroutine set_plev_r4(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pres, osig, oeta)
  !c+++ [modify]
  !c+++ p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), intent(inout) :: p(kmax)              !! 
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax                 !! x-axis size
  integer(kind=i4b), intent(in) :: jmax                 !! y-axis size
  integer(kind=i4b), intent(in) :: kmax                 !! output z-axis size
  real(kind=r4b), intent(in)    :: rmiss                !! undefined value
  real(kind=r4b), intent(in)    :: ps(imax,jmax)        !! input surface pressure data (hPa)
  real(kind=r8b), intent(in)    :: eta_fa(kmax)         !! for eta half lev.
  real(kind=r8b), intent(in)    :: eta_fb(kmax)         !! for eta half lev.
  logical, intent(in)           :: osig                 !! enable sigma-lev.
  logical, intent(in)           :: oeta                 !! enable eta-lev.
  !c+++ [output]
  real(kind=r4b), intent(out)   :: pres(imax,jmax,kmax) !! [hPa]
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k              !!

  if (oeta) then
    !c+++ for eta-level data
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          !c+++ eta to p[hPa]
          p(k) = eta_fa(k) + eta_fb(k) * ps(i,j)
          pres(i,j,k) = p(k) !! pres = p
        enddo !! i
      enddo !! j
    enddo !! k
  else if (osig) then
    !c+++ for sigma-level data
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          !c+++ p
          if (ps(i,j) /= rmiss) then
            pres(i,j,k) = ps(i,j) * p(k) !! pres = ps * sigma
          else
            pres(i,j,k) = rmiss
          endif
        enddo  !! i
      enddo !! j
    enddo !! k
  else
    !c+++ for p-level data
    do k = 1, kmax
      pres(1:imax,1:jmax,k) = p(k) !! pres = p
    enddo !! k
  endif

  return
end subroutine set_plev_r4

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine set_plev_r8
!c  set plevs from input p-/sig-/eta-levels (double ver. of set_plevs)
!c=====
subroutine set_plev_r8(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pres, osig, oeta)
  !c+++ [modify]
  !c+++ p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), intent(inout) :: p(kmax)              !! 
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax                 !! x-axis size
  integer(kind=i4b), intent(in) :: jmax                 !! y-axis size
  integer(kind=i4b), intent(in) :: kmax                 !! output z-axis size
  real(kind=r8b), intent(in)    :: rmiss                !! undefined value
  real(kind=r8b), intent(in)    :: ps(imax,jmax)        !! input surface pressure data (hPa)
  real(kind=r8b), intent(in)    :: eta_fa(kmax)         !! for eta half lev.
  real(kind=r8b), intent(in)    :: eta_fb(kmax)         !! for eta half lev.
  logical, intent(in)           :: osig                 !! enable sigma-lev.
  logical, intent(in)           :: oeta                 !! enable eta-lev.
  !c+++ [output]
  real(kind=r8b), intent(out)   :: pres(imax,jmax,kmax) !! [hPa]
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k              !!

  if (oeta) then
    !c+++ for eta-level data
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          !c+++ eta to p[hPa]
          p(k) = eta_fa(k) + eta_fb(k) * ps(i,j)
          pres(i,j,k) = p(k) !! pres = p
        enddo !! i
      enddo !! j
    enddo !! k
  else if (osig) then
    !c+++ for sigma-level data
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          !c+++ p
          if (ps(i,j) /= rmiss) then
            pres(i,j,k) = ps(i,j) * p(k) !! pres = ps * sigma
          else
            pres(i,j,k) = rmiss
          endif
        enddo  !! i
      enddo !! j
    enddo !! k
  else
    !c+++ for p-level data
    do k = 1, kmax
      pres(1:imax,1:jmax,k) = p(k) !! pres = p
    enddo !! k
  endif

  return
end subroutine set_plev_r8

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine set_plevs
!c  set plevs from input p-/sig-/eta-levels
!c=====
subroutine set_plevs(imax, jmax, kmax, rmiss, p, ps, eta_fa, eta_fb, pres, osig, oeta)
  !c+++ [modify]
  !c+++ p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), intent(inout) :: p(kmax)              !! 
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax                 !! x-axis size
  integer(kind=i4b), intent(in) :: jmax                 !! y-axis size
  integer(kind=i4b), intent(in) :: kmax                 !! output z-axis size
  real(kind=r8b), intent(in)    :: rmiss                !! undefined value
  real(kind=r4b), intent(in)    :: ps(imax,jmax)        !! input surface pressure data (hPa)
  real(kind=r8b), intent(in)    :: eta_fa(kmax)         !! for eta half lev.
  real(kind=r8b), intent(in)    :: eta_fb(kmax)         !! for eta half lev.
  logical, intent(in)           :: osig                 !! enable sigma-lev.
  logical, intent(in)           :: oeta                 !! enable eta-lev.
  !c+++ [output]
  real(kind=r4b), intent(out)   :: pres(imax,jmax,kmax) !! [hPa]
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k              !!

  if (oeta) then
    !c+++ for eta-level data
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          !c+++ eta to p[hPa]
          p(k) = eta_fa(k) + eta_fb(k) * ps(i,j)
          pres(i,j,k) = p(k) !! pres = p
        enddo !! i
      enddo !! j
    enddo !! k
  else if (osig) then
    !c+++ for sigma-level data
    do k = 1, kmax
      do j = 1, jmax
        do i = 1, imax
          !c+++ p
          if (ps(i,j) /= rmiss) then
            pres(i,j,k) = ps(i,j) * p(k) !! pres = ps * sigma
          else
            pres(i,j,k) = rmiss
          endif
        enddo  !! i
      enddo !! j
    enddo !! k
  else
    !c+++ for p-level data
    do k = 1, kmax
      pres(1:imax,1:jmax,k) = p(k) !! pres = p
    enddo !! k
  endif

  return
end subroutine set_plevs

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine area_ave
!c
!c=====
subroutine area_ave(imax, jmax, nxmin, nxmax, nymin, nymax, rmiss, &
&  din, lat, mean, owgtlat)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax     !! x-, y-grid sizes
  integer(kind=i4b), intent(in) :: nxmin, nxmax   !! min./max. limit of average
  integer(kind=i4b), intent(in) :: nymin, nymax   !! min./max. limit of average
  real(kind=r8b), intent(in)    :: rmiss          !! missing value
  real(kind=r4b), intent(in)    :: din(imax,jmax) !! (vmr)
  real(kind=r8b), intent(in)    :: lat(jmax)      !! latitude(deg)
  logical, intent(in)           :: owgtlat        !! on/off latitude weight
  !c+++ [output]
  real(kind=r4b), intent(out)   :: mean           !! area mean
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j           !!
  real(kind=r8b)                :: phi(jmax)      !! lat (rad)
  real(kind=r8b)                :: cs(jmax)       !! cos(lat)
  real(kind=r8b)                :: ave, rsum      !!
  real(kind=r8b)                :: pi             !!
  pi = 4.d0 * datan(1.d0)

  do j = 1, jmax
    phi(j) = lat(j) * pi / 180.d0
    cs(j) = cos(phi(j))
  enddo !! j

  !c+++ medid. mean: ==> dout(1:nx,1:np)
  ave = 0.d0
  rsum = 0.d0
  if (owgtlat) then
    do j = nymin, nymax
      do i = nxmin, nxmax
        if (din(i,j) /= rmiss) then
          ave = ave + dble(din(i,j)) * cs(j)
          rsum = rsum + cs(j)
        endif
      enddo !! i
    enddo !! j
  else
    do j = nymin, nymax
      do i = nxmin, nxmax
        if (din(i,j) /= rmiss) then
          ave = ave + dble(din(i,j))
          rsum = rsum + 1.d0
        endif
      enddo !! i
    enddo !! j
  endif

  if (rsum  > eps) then
    mean = ave / rsum
  else
    mean = rmiss
  endif

  return
end subroutine area_ave

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine darea_ave
!c
!c=====
subroutine darea_ave(imax, jmax, nxmin, nxmax, nymin, nymax, rmiss, &
&  din, lat, mean, owgtlat)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax     !! x-, y-grid sizes
  integer(kind=i4b), intent(in) :: nxmin, nxmax   !! min./max. limit of average
  integer(kind=i4b), intent(in) :: nymin, nymax   !! min./max. limit of average
  real(kind=r8b), intent(in)    :: rmiss          !! missing value
  real(kind=r8b), intent(in)    :: din(imax,jmax) !! (vmr)
  real(kind=r8b), intent(in)    :: lat(jmax)      !! latitude(deg)
  logical, intent(in)           :: owgtlat        !! on/off latitude weight
  !c+++ [output]
  real(kind=r8b), intent(out)   :: mean           !! area mean
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j           !!
  real(kind=r8b)                :: phi(jmax)      !! lat (rad)
  real(kind=r8b)                :: cs(jmax)       !! cos(lat)
  real(kind=r8b)                :: ave, rsum      !!
  real(kind=r8b)                :: pi             !!
  pi = 4.d0 * datan(1.d0)

  do j = 1, jmax
    phi(j) = lat(j) * pi / 180.d0
    cs(j) = cos(phi(j))
  enddo !! j

  !c+++ medid. mean: ==> dout(1:nx,1:np)
  ave = 0.d0
  rsum = 0.d0
  if (owgtlat) then
    do j = nymin, nymax
      do i = nxmin, nxmax
        if (din(i,j) /= rmiss) then
          ave = ave + dble(din(i,j)) * cs(j)
          rsum = rsum + cs(j)
        endif
      enddo !! i
    enddo !! j
  else
    do j = nymin, nymax
      do i = nxmin, nxmax
        if (din(i,j) /= rmiss) then
          ave = ave + dble(din(i,j))
          rsum = rsum + 1.d0
        endif
      enddo !! i
    enddo !! j
  endif

  if (rsum  > eps) then
    mean = ave / rsum
  else
    mean = rmiss
  endif

  return
end subroutine darea_ave

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine omgp2w
!c  convert from omg (hPa/s), p(hPa) into w (m/s)
!c  w = -H/p * omg
!c=====
subroutine omgp2w(imax, jmax, kmax, rmiss, omg, w, p, h)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax
  real(kind=r8b), intent(in)    :: rmiss               !! undefined value
  real(kind=r4b), intent(in)    :: omg(imax,jmax,kmax) !! input omg(hPa/s)
  real(kind=r4b), intent(in)    :: p(imax,jmax,kmax)   !! p (hPa)
  real(kind=r8b), intent(in)    :: h                   !! scale height H(m)
  !c+++ [output]
  real(kind=r4b), intent(out)   :: w(imax,jmax,kmax)   !! output w(m/s)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k

  !c+++ w  = -H/p * omg
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (omg(i,j,k) == rmiss.or.p(i,j,k) == rmiss) then
          w(i,j,k) = rmiss
        else
          w(i,j,k) = - h / p(i,j,k) / 100. * omg(i,j,k) * 100. ! omg[hPa/s] for CCMV, p[hPa]->w[m/s]
          !ccc w(i,j,k) = -h / p(k) / 100. * omg(i,j,k)       ! omg[Pa/s] for NCEP, ERA40, JRA25 p[hPa]->w[m/s]
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine omgp2w

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine conv_zaxis
!c  convert from input p-/sig-/eta-levels into output p-levels
!c=====
subroutine conv_zaxis(imax, jmax, kmax0, kmax, rmiss, haxisz, p, ps0, d0, d, &
& itype, omiss, osig, oeta)
  use intplt, only: intplt_s2p, intplt_h2p, intplt_p
  use rwgtool, only: get_etacoef
  !c+++ [input]
  integer(kind=i4b), intent(in)     :: imax                  !! x-axis size
  integer(kind=i4b), intent(in)     :: jmax                  !! y-axis size
  integer(kind=i4b), intent(in)     :: kmax0                 !! input z-axis size
  integer(kind=i4b), intent(in)     :: kmax                  !! output z-axis size
  real(kind=r8b), intent(in)        :: rmiss                 !! undefined value
  character(len=*), intent(in)      :: haxisz                !! input z-axis name
  real(kind=r8b), intent(in)        :: p(kmax)               !! output pressure levels p(hPa)
  real(kind=r8b), intent(in)        :: ps0(imax,jmax)        !! input surface pressure data (hPa)
  real(kind=r8b), intent(in)        :: d0(imax,jmax,kmax0)   !! input data for conversion
  character(len=*), intent(in)      :: itype                 !! intplt type (l: linear, s: spline)
  logical, intent(in)               :: omiss                 !! t: enable extrapolation, f: disable
  logical, intent(in)               :: osig                  !! enable sigma-lev.
  logical, intent(in)               :: oeta                  !! enable eta-lev.
  !c+++ [output]
  real(kind=r4b), intent(out)       :: d(imax,jmax,kmax)     !! converted data
  !c+++ [internal work]
  !c+++ input data for intplt
  real(kind=r8b)                    :: ips(imax,jmax)        !! input surface pressure data (hPa)
  real(kind=r8b)                    :: ival(imax,jmax,kmax0) !! input data
  !c+++ output data from intplt
  real(kind=r8b)                    :: oval(imax,jmax,kmax)  !! output data
  !c+++ [internal save]
  logical, save                     :: ofirst = .true.       !!
  !c+++ vertical axis
  real(kind=r8b), allocatable, save :: ip(:)                 !! input p-axis data (hPa)
  real(kind=r8b), allocatable, save :: sig(:)                !! input sig-axis data ()
  real(kind=r8b), allocatable, save :: eta_fa(:)             !! input eta half lev.
  real(kind=r8b), allocatable, save :: eta_fb(:)             !! input eta half lev.
  real(kind=r8b), allocatable, save :: op(:)                 !! output p-axis data (hPa)
  
  !c+++ one time
  if (ofirst) then
    ofirst = .false.
    allocate(ip(kmax0), sig(kmax0), eta_fa(kmax0), eta_fb(kmax0), op(kmax))
    if (osig) then
      !c+++ for sig-axis
      call getzaxis(kmax0, haxisz, sig) !! ==> sig()
    else if (oeta) then
      !c+++ for eta-axis
      call get_etacoef(kmax0, haxisz, eta_fa, eta_fb) !! ==> eta_fa, eta_fb
    else
      !c+++ for p-axis
      call getzaxis(kmax0, haxisz, ip) !! ==> p(hPa)
    endif
  endif

  op(1:kmax) = p(1:kmax)
  ips(1:imax,1:jmax) = ps0(1:imax,1:jmax)
  ival(1:imax,1:jmax,1:kmax0) = d0(1:imax,1:jmax,1:kmax0)

  if (osig) then
    call intplt_s2p(imax, jmax, kmax0, kmax, rmiss, sig, op, itype, omiss, ips, ival, oval)
  else if (oeta) then
    call intplt_h2p(imax, jmax, kmax0, kmax, rmiss, eta_fa, eta_fb, op, itype, omiss, ips, ival, oval)
  else
    call intplt_p(imax, jmax, kmax0, kmax, rmiss, ip, op, itype, omiss, ival, oval)
  endif
  d(1:imax,1:jmax,1:kmax) = oval(1:imax,1:jmax,1:kmax)

  return
end subroutine conv_zaxis

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_axname
!c  get x-, y-, z-axis names from gtool3 header
!c=====
subroutine get_axname(haxisx, haxisy, haxisz, head)
  !c+++ [input]
  character(len=*), intent(in)  :: head(ndc) !! header
  !c+++ [output]
  character(len=*), intent(out) :: haxisx    !! x-axis name
  character(len=*), intent(out) :: haxisy    !! y-axis name
  character(len=*), intent(out) :: haxisz    !! z-axis name

  haxisx = head(29)
  haxisy = head(32)
  haxisz = head(35)

  return
end subroutine get_axname

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine getxaxis
!c  get x-axis points from axis information file
!c=====
subroutine getxaxis(imax, haxisx, lon)
  use rwgtool, only: get_axis
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax      !! x-axis size
  character(len=*), intent(in)  :: haxisx    !! x-axis name
  !c+++ [output]
  real(kind=r8b), intent(out)   :: lon(imax) !! longitude

  !c+++ read x-axis file
  call get_axis(imax, haxisx, lon)

  return
end subroutine getxaxis

!c---------------------------------------------------------------------c
!c  subroutine getyaxis
!c  get y-axis points from axis information file
!c=====
subroutine getyaxis(jmax, haxisy, lat)
  use rwgtool, only: get_axis
  !c+++ [input]
  integer(kind=i4b), intent(in) :: jmax      !! y-axis size
  character(len=*), intent(in)  :: haxisy    !! y-axis name
  !c+++ [output]
  real(kind=r8b), intent(out)   :: lat(jmax) !! latitude

  !c+++ read y-axis file
  call get_axis(jmax, haxisy, lat)

  return
end subroutine getyaxis

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine getzaxis
!c  get z-axis points from axis information file
!c=====
subroutine getzaxis(kmax, haxisz, p)
  use rwgtool, only: get_axis
  !c+++ [input]
  integer(kind=i4b), intent(in) :: kmax    !! z-axis size
  character(len=*), intent(in)  :: haxisz  !! z-axis name
  !c+++ [output]
  real(kind=r8b), intent(out)   :: p(kmax) !! p[hPa] for p-lev, sig[] for sig-lev

  !c+++ read z-axis file
  call get_axis(kmax, haxisz, p)

  return
end subroutine getzaxis

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine putzaxis
!c  get z-axis information & write new z-axis information file
!c=====
subroutine putzaxis(kmax, haxiszref, haxisz, htitl, hunit, p)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: kmax
  character(len=*), intent(in)  :: haxiszref !! z-axis name for read
  character(len=*), intent(in)  :: haxisz    !! z-axis name for write
  character(len=*), intent(in)  :: htitl     !! title
  character(len=*), intent(in)  :: hunit     !! unit
  real(kind=r4b), intent(in)    :: p(kmax)   !!
  !c+++ [internal work]
  integer(kind=i4b)             :: ios       !! end code
  character(len=ncc)            :: head(ndc) !!
  character(len=nfiln)          :: gtax      !! GTAXDIR (input from getenv)
  character(len=nfiln)          :: filename  !!
  character(len=ncc)            :: hitem     !!
  character(len=ncc*2)          :: htitlz    !!

  hitem = haxisz

  !c+++ get parameter
  call getenv('GTAXDIR', gtax)
  write(6, *) 'GTAXDIR=', trim(gtax)

  !c+++ open z-axis file
  filename = trim(gtax)//'/GTAXLOC.'//trim(haxiszref)
  write(6, *) 'open ', trim(filename)
  open(10, file=trim(filename), form='unformatted', iostat=ios)
  if (ios /= 0) then
    write(6, *) 'Error: open ', trim(filename)
    stop 2
  endif

  !c+++ read z-axis file
  read(10) head
  close(10)

  !c+++ set new header
  head(3)  = hitem
  htitlz   = htitl
  head(14) = htitlz(1:16)
  head(15) = htitlz(17:32)
  head(16) = hunit
  head(29) = haxisz
  write(head(31), '(i16)') kmax
  write(head(44), '(i16)') 1 !! styp = 1
  !c+++ set modify time & user
  call set_jdaten(head) !! set create & modify time (head(60), head(62))
  call set_user(head)   !! set create & modify user (head(61), head(63))

  !c+++ write new z-axis file
  !ccc filename = trim(gtax)//'/GTAXLOC.'//trim(haxisz)
  filename = './GTAXLOC.'//trim(haxisz)
  write(6, *) 'open ', trim(filename)
  open(10, file=trim(filename), form='unformatted', iostat=ios)
  if (ios /= 0) then
    write(6, *) 'Error: open ', trim(filename)
    stop 2
  endif
  write(10) head
  write(10) p(1:kmax)
  close(10)

  return
end subroutine putzaxis

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_time
!c  read gtool3 header and get time
!c=====
subroutine get_time(head, jdate, ios)
  !c+++ [input]
  character(len=*), intent(in)   :: head(ndc) !! header
  !c+++ [output]
  integer(kind=i4b), intent(out) :: jdate(6)  !! yy, mm, dd, hh, mn, ss
  integer(kind=i4b), intent(out) :: ios       !! end code

  !c+++ get time
  read(head(27), '(i4.4, i2.2, i2.2, 1x, i2.2, i2.2, i2.2)', iostat=ios) jdate(1:6)
  if (ios /= 0) write(6, *) 'Error: in get_time'

  return
end subroutine get_time

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine get_date
!c  set current time (YYYY-MM-DD HH:MM:SS)
!c=====
subroutine get_date(hdate)
  implicit none
  !c+++ [output]
  character(len=*), intent(out) :: hdate     !!
  !c+++ [internal work]
  integer(kind=i2b)             :: jdates(7) !! input from getdat, gettim
  integer(kind=i4b)             :: jdate(6)  !! year, month, day, hour, minute, second

  !c+++ get time
  call getdat(jdates(1), jdates(2), jdates(3))
  call gettim(jdates(4), jdates(5), jdates(6), jdates(7))
  jdate(1:6) = jdates(1:6)

  !c+++ set ctime
  !ccc write(hdate, '(i4.4, 2i2.2, 1x, 3i2.2)') jdate(1:6)
  write(hdate, '(i4.4, 2(a, i2.2), 1x, 3(i2.2, a))') &
&  jdate(1), '-', jdate(2), '-', jdate(3), jdate(4), ':', jdate(5), ':', jdate(6)

  return
end subroutine get_date

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine get_date2
!c  set current time (YYYY-MM-DD-THH:MM:SSZ)
!c  use gmtime to get UTC
!c=====
subroutine get_date2(hdate)
  implicit none
  !c+++ [output]
  character(len=*), intent(out) :: hdate     !! UTC format
  !c+++ [internal work]
  integer(kind=i4b)             :: time      !! function
  integer(kind=i4b)             :: jdate(9)  !! input from gmtime
  integer(kind=i4b)             :: jdaten(6) !! year, month, day, hour, minute, second

  !c+++ get time (UTC)
  call gmtime(time(), jdate)

  !c+++ set jdaten
  jdaten(1) = jdate(6) + 1900
  jdaten(2) = jdate(5) + 1
  jdaten(3) = jdate(4)
  jdaten(4) = jdate(3)
  jdaten(5) = jdate(2)
  jdaten(6) = jdate(1)

  !c+++ set ctime
  write(hdate, '(i4.4, 5(a, i2.2), a)') &
&  jdaten(1), '-', jdaten(2), '-', jdaten(3), '-T', jdaten(4), ':', jdaten(5), ':', jdaten(6), 'Z'

  return
end subroutine get_date2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: repmis_r4
!c  replace missing values
!c
!c  MODIFY
!c       real(kind=r4b) d(nx,ny,np)
!c  INPUT
!c    integer(kind=i4b) imax, jmax, kmax
!c       real(kind=r4b) dmiss, dmisr
!c=====
subroutine repmis_r4(imax, jmax, kmax, dmiss, dmisr, d)
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: d(imax,jmax,kmax) !! data
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax              !! x-axis size
  integer(kind=i4b), intent(in) :: jmax              !! y-axis size
  integer(kind=i4b), intent(in) :: kmax              !! z-axis size
  real(kind=r4b), intent(in)    :: dmiss             !! missing value (input)
  real(kind=r4b), intent(in)    :: dmisr             !! missing value (replace)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k           !!

  !c+++ replace dmiss by dmisr
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (d(i,j,k) == dmiss) then
          d(i,j,k) = dmisr
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine repmis_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: repmis_r8
!c  replace missing values
!c
!c  MODIFY
!c       real(kind=r8b) d(nx,ny,np)
!c  INPUT
!c    integer(kind=i4b) imax, jmax, kmax
!c       real(kind=r8b) dmiss, dmisr
!c=====
subroutine repmis_r8(imax, jmax, kmax, dmiss, dmisr, d)
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: d(imax,jmax,kmax) !! data
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax              !! x-axis size
  integer(kind=i4b), intent(in) :: jmax              !! y-axis size
  integer(kind=i4b), intent(in) :: kmax              !! z-axis size
  real(kind=r8b), intent(in)    :: dmiss             !! missing value (input)
  real(kind=r8b), intent(in)    :: dmisr             !! missing value (replace)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k           !!

  !c+++ replace dmiss by dmisr
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (d(i,j,k) == dmiss) then
          d(i,j,k) = dmisr
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine repmis_r8

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine set_jdaten
!c  set new time header information
!c=====
subroutine set_jdaten(head)
  !c+++ [modify]
  character(len=*), intent(inout) :: head(ndc) !! header
  !c+++ [internal work]
  integer(kind=i2b)               :: jdates(7) !! input from getdat, gettim
  integer(kind=i4b)               :: jdaten(6) !! year, month, day, hour, minute, second

  !c+++ get time
  call getdat(jdates(1), jdates(2), jdates(3))
  call gettim(jdates(4), jdates(5), jdates(6), jdates(7))
  jdaten(1:6) = jdates(1:6)

  !c+++ set ctime
  write(head(60), '(i4.4, 2i2.2, 1x, 3i2.2)') jdaten(1:6)
  !c+++ set mtime
  write(head(62), '(i4.4, 2i2.2, 1x, 3i2.2)') jdaten(1:6)

  return
end subroutine set_jdaten

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine set_user
!c  modify user information
!c=====
subroutine set_user(head)
  !c+++ [modify]
  character(len=*), intent(inout) :: head(ndc) !! header
  !c+++ [internal work]
  character(len=ncc)              :: huser     !! input from getenv

  call getenv('USER', huser)

  !c+++ set cuser
  head(61) = huser
  !c+++ set muser
  head(63) = huser

  return
end subroutine set_user

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine shift_r4_xgrids
!c  shift x-grids (real*4)
!c=====
subroutine shift_r4_xgrids(nx, ny, np, ix_shift, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: ix_shift    !! num. of x-shift
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i           !!
  real(kind=r4b), allocatable   :: work(:,:,:) !!

  if (ix_shift < 0.or.ix_shift > nx) then
    write(6, *) 'Warn: invalid range of ix_shift, do nothing'
    write(6, *) 'ix_shift = ', ix_shift
    return
  endif
  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do i = 1, nx-ix_shift
    work(i+ix_shift,1:ny,1:np) = d(i,1:,1:np)
  enddo !! k
  do i = 1, ix_shift
    work(i,1:ny,1:np) = d(nx-ix_shift+i,1:,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine shift_r4_xgrids

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine shift_r8_xgrids
!c  shift x-grids (real*8)
!c=====
subroutine shift_r8_xgrids(nx, ny, np, ix_shift, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: ix_shift    !! num. of x-shift
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i           !!
  real(kind=r8b), allocatable   :: work(:,:,:) !!

  if (ix_shift < 0.or.ix_shift > nx) then
    write(6, *) 'Warn: invalid range of ix_shift, do nothing'
    write(6, *) 'ix_shift = ', ix_shift
    return
  endif
  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do i = 1, nx-ix_shift
    work(i+ix_shift,1:ny,1:np) = d(i,1:,1:np)
  enddo !! k
  do i = 1, ix_shift
    work(i,1:ny,1:np) = d(nx-ix_shift+i,1:,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine shift_r8_xgrids

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r4_yrev
!c  reverse y-grids
!c=====
subroutine conv_r4_yrev(nx, ny, np, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: j           !!
  real(kind=r4b), allocatable   :: work(:,:,:) !!

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r4_yrev

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r8_yrev
!c  reverse y-grids
!c=====
subroutine conv_r8_yrev(nx, ny, np, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: j           !!
  real(kind=r8b), allocatable   :: work(:,:,:) !!

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r8_yrev

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r4_zrev
!c  reverse z-grids
!c=====
subroutine conv_r4_zrev(nx, ny, np, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: k           !!
  real(kind=r4b), allocatable   :: work(:,:,:) !!

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    work(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r4_zrev

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r8_zrev
!c  reverse z-grids
!c=====
subroutine conv_r8_zrev(nx, ny, np, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: k           !!
  real(kind=r8b), allocatable   :: work(:,:,:) !!

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    work(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r8_zrev

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine conv_r4_endian
!c  change endian
!c  [history]
!c  2017/04/17 Yamashita
!c=====
subroutine conv_r4_endian(nx, ny, np, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k     !!
  real(kind=r4b)                :: din, dout   !!
  real(kind=r4b), allocatable   :: work(:,:,:) !!

  allocate(work(nx,ny,np))
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        din = d(i,j,k)
        call convend(dout, din)
!        call convend(dout, din, 4)
        work(i,j,k) = dout
      enddo !! i
    enddo !! j
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r4_endian

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine conv_r8_endian
!c  change endian
!c  [history]
!c  2017/04/17 Yamashita
!c=====
subroutine conv_r8_endian(nx, ny, np, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k     !!
  real(kind=r8b)                :: din, dout   !!
  real(kind=r8b), allocatable   :: work(:,:,:) !!

  allocate(work(nx,ny,np))
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        din = d(i,j,k)
!        call convend(dout, din, 8)
        call convend(dout, din)
        work(i,j,k) = dout
      enddo !! i
    enddo !! j
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r8_endian

!c----------------------------------------------------------------------c

!c======================================================================c

end module util
