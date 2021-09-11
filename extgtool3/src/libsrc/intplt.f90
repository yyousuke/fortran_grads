!c
!c  module intplt
!c  [history]
!c  2013/03/09 Yamashita: first ver.
!c  2013/03/30 Yamashita: add intplt_s2p, intplt_h2p
!c  2013/10/18 Yamashita: add intplt_t
!c  2017/04/20 Yamashita: add intplt_z
!c  2017/06/05 Yamashita: use common_typedef
!c  interpolation for 3D variables
!c
!c  public:
!c  subroutine intplt_y: y-axis ==> y-axis
!c  subroutine intplt_p: p-axis ==> p-axis
!c  subroutine intplt_z: z-axis ==> z-axis
!c  subroutine intpltz: z-axis ==> z-axis (1-d data)
!c  subroutine intplt_s2p: sig-axis ==> p-axis
!c  subroutine intplt_h2p: eta-axis ==> p-axis
!c  subroutine intplt_p2s: p-axis ==> sig-axis
!c  subroutine intplt_p2h: p-axis ==> eta-axis
!c  subroutine intplt_t: t-axis ==> t-axis
!c======================================================================c
module intplt
  use common_typedef, only: i4b, r4b, r8b
  use splines, only: spline
  use linear, only: intrp
  implicit none
  private
  public :: intplt_y, intplt_p, intplt_z, intpltz
  public :: intplt_s2p, intplt_h2p
  public :: intplt_p2s, intplt_p2h
  public :: intplt_t

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c 
!c  subroutine intplt_y
!c  meridional interpolation for 3D variable
!c=====
subroutine intplt_y(imax, ijmax, ojmax, kmax, undef, iax_y, oax_y, itype, omiss, ival, oval)
  !c+++ [input]
  !c+++ size
  integer(kind=i4b), intent(in) :: imax                  !! number of x-grids
  integer(kind=i4b), intent(in) :: ijmax                 !! number of y-grids for input
  integer(kind=i4b), intent(in) :: ojmax                 !! number of y-grids for output
  integer(kind=i4b), intent(in) :: kmax                  !! number of z-grids
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                 !! 
  !c+++ axis
  real(kind=r8b), intent(in)    :: iax_y(ijmax)          !! input y-axis data
  real(kind=r8b), intent(in)    :: oax_y(ojmax)          !! output y-axis data
  !c+++ switch
  character(len=*), intent(in)  :: itype                 !! intplt type; l: linear, s: spline
  logical, intent(in)           :: omiss                 !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ival(imax,ijmax,kmax) !! input variable
  !c+++ [output]
  !c+++ output data
  real(kind=r8b), intent(out)   :: oval(imax,ojmax,kmax) !! output variable
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, ij           !!
  real(kind=r8b)                :: wkx (ijmax)           !! input axis
  real(kind=r8b)                :: wky (ijmax)           !! input data
  real(kind=r8b)                :: wkxx(ojmax)           !! output axis
  real(kind=r8b)                :: wkyy(ojmax)           !! output data
  real(kind=r8b)                :: latitude              !!
  logical                       :: olinear               !! true: linear, false: spline

  if (itype == 'l') then
    olinear = .true.  !! true: linear
  else if (itype == 's') then
    olinear = .false. !! false: spline
  else
    write(6, *) 'Error: invalid input itype = ', itype
    stop 2
  endif
!c
!c meridional interpolation
!c===
  do k = 1, kmax
    do i = 1, imax
      ij = 0
      do j = 1, ijmax
        !c+++ set input data ==> wkx, wky
        if (ival(i,j,k) /= undef) then
          ij = ij + 1
          latitude = iax_y(j)
          wkx(ijmax-ij+1) = - latitude
          wky(ijmax-ij+1) = ival(i,j,k)
        endif
      enddo !! j
      !c+++ set output axis ==> wkxx
      do j = 1, ojmax
        latitude = oax_y(j)
        wkxx(ojmax-j+1) = - latitude
      enddo !! j

      if (ij /= 0) then
        if (olinear) then
          !c+++ linear ==> wkyy
          call intrp(wky(ijmax-ij+1:ijmax), wkx(ijmax-ij+1:ijmax), ij, &
&              wkyy, &
&              wkxx, ojmax, omiss, undef)
        else
          !c+++ spline ==> wkyy
          call spline(wky(ijmax-ij+1:ijmax), wkx(ijmax-ij+1:ijmax), ij, &
&              wkyy, &
&              wkxx, ojmax, omiss, undef)
        endif
      else
        wkyy(1:ojmax) = undef
      endif

      do j = 1, ojmax
        oval(i,j,k) = wkyy(ojmax-j+1)
      enddo !! j
    enddo !! i
  enddo !! k

  return
end subroutine intplt_y

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c 
!c  subroutine intplt_p
!c  vertical interpolation for 3D variable
!c  input: p-levs, output: p-levs
!c=====
subroutine intplt_p(imax, jmax, ikmax, okmax, undef, ip, op, itype, omiss, ival, oval)
  !c+++ [input]
  !c+++ size
  integer(kind=i4b), intent(in) :: imax                  !! number of x-grids
  integer(kind=i4b), intent(in) :: jmax                  !! number of y-grids
  integer(kind=i4b), intent(in) :: ikmax                 !! number of z-grids for input
  integer(kind=i4b), intent(in) :: okmax                 !! number of z-grids for output
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                 !!
  !c+++ axis
  real(kind=r8b), intent(in)    :: ip(ikmax)             !! input z-axis data [hPa]
  real(kind=r8b), intent(in)    :: op(okmax)             !! output z-axis data [hPa]
  !c+++ switch
  character(len=*), intent(in)  :: itype                 !! intplt type; l: linear, s: spline
  logical, intent(in)           :: omiss                 !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ival(imax,jmax,ikmax) !! input variable (p-levs)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: oval(imax,jmax,okmax) !! output variable (p-levs)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, ik           !!
  real(kind=r8b)                :: wkx (ikmax)           !! input axis
  real(kind=r8b)                :: wky (ikmax)           !! input data
  real(kind=r8b)                :: wkxx(okmax)           !! output axis
  real(kind=r8b)                :: wkyy(okmax)           !! output data
  real(kind=r8b)                :: pressure              !!
  logical                       :: olinear               !! true: linear, false: spline

  if (itype == 'l') then
    olinear = .true.  !! true: linear
  else if (itype == 's') then
    olinear = .false. !! false: spline
  else
    write(6, *) 'Error: invalid input itype = ', itype
    stop 2
  endif

!c
!c vertical interpolation
!c===
  do j = 1, jmax
    do i = 1, imax
      ik = 0
      do k = 1, ikmax
        !c+++ set input data ==> wkx (axis), wky (data)
        if (ival(i,j,k) /= undef) then
          ik = ik + 1
          pressure = ip(k)
          wkx(ikmax-ik+1) = - pressure
          wky(ikmax-ik+1) = ival(i,j,k)
        endif
      enddo !! k
      !c+++ output p ==> wkxx
      do k=1, okmax
        pressure = op(k)
        wkxx(okmax-k+1) = - pressure
      enddo !! k

      if (ik /= 0) then
        if (olinear) then
          !c+++ linear ==> wkyy
          call intrp(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        else
          !c+++ spline ==> wkyy
          call spline(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        endif
      else
        wkyy(1:okmax) = undef
      endif

      do k = 1, okmax
        oval(i,j,k) = wkyy(okmax-k+1)
      enddo !! k
    enddo !! i
  enddo !! j

  return
end subroutine intplt_p

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine intplt_z
!c  vertical interpolation 
!c  [history]
!c  2017/02/15 Yamashita
!c  2017/04/20 Yamashita: 3-d data
!c=====
subroutine intplt_z(imax, jmax, ikmax, okmax, undef, ip, op, &
&  ps, h, fact, offset, olinear, ologl, omiss, ival, oval)
  !c+++ [input]
  !c+++ size
  integer(kind=i4b), intent(in) :: imax                  !! x-size
  integer(kind=i4b), intent(in) :: jmax                  !! y-size
  integer(kind=i4b), intent(in) :: ikmax                 !! input z-size
  integer(kind=i4b), intent(in) :: okmax                 !! output z-size
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                 !! undefined values
  !c+++ axis
  real(kind=r4b), intent(in)    :: ip(ikmax)             !! input p-levs. (hPa)
  real(kind=r4b), intent(in)    :: op(okmax)             !! output p-levs (hPa)
  !c+++ constants
  real(kind=r4b), intent(in)    :: ps                    !! surface pressure (hPa)
  real(kind=r8b), intent(in)    :: h                     !! scale height (m)
  real(kind=r8b), intent(in)    :: fact, offset          !! factor, offset
  !c+++ switch
  logical, intent(in)           :: olinear               !! true: linear, false: spline
  logical, intent(in)           :: ologl                 !! true: log-linear, false: linear
  logical, intent(in)           :: omiss                 !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ival(imax,jmax,ikmax) !! input variable (p-levs)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: oval(imax,jmax,okmax) !! output variable (p-levs)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k               !! loop variable
  integer(kind=i4b)             :: ik, kk                !! z-grid id
  real(kind=r8b)                :: wkx (ikmax)           !! input axis
  real(kind=r8b)                :: wky (ikmax)           !! input data
  real(kind=r8b)                :: wkxx(okmax)           !! output axis
  real(kind=r8b)                :: wkyy(okmax)           !! output data
  real(kind=r8b)                :: height, hs, he        !! height (m)
  real(kind=r8b)                :: wxsgn                 !! sign of axis data
  logical                       :: orinp                 !! true: reverse input data, false: disable
  logical                       :: orout                 !! true: reverse output data, false: disable
!c
!c axis, input data
!c===
  !c+++ input axis
  hs = - h * log(ip(1) / ps)
  he = - h * log(ip(ikmax) / ps)
  orinp = .false.
  if (hs < he) orinp = .true.
  !c+++ output axis
  hs = - h * log(op(1) / ps)
  he = - h * log(op(okmax) / ps)
  orout = .false.
  if (hs < he) orout = .true.
  wxsgn = 1.d0

!c
!c vertical interpolation
!c===
  do j = 1, jmax
    do i = 1, imax
      ik = 0
      do k = 1, ikmax
        !c+++ set input data ==> wkx, wky
        if (ival(i,j,k) /= undef) then
          ik = ik + 1
          if (orinp) then
            !c+++ reverse
            kk = ikmax - ik + 1
          else
            !c+++ original
            kk = ik
          endif
          height = - h * log(ip(k) / ps)
          wkx(kk) = wxsgn * height
          if (ologl) then
            !c+++ log-linear
            wky(kk) = log(ival(i,k,k) * fact + offset)
          else
            !c+++ normal
            wky(kk) = ival(i,j,k) * fact + offset
          endif
        endif
      enddo !! k
    
      !c+++ output p ==> wkxx
      do k = 1, okmax
        if (orout) then
          !c+++ reverse
          kk = okmax - k + 1
        else
          !c+++ original
          kk = k
        endif
        height = - h * log(op(k) / ps)
        wkxx(kk) = wxsgn * height
      enddo !! k

      !c+++ vertical interpolation
      if (ik /= 0) then
        if (olinear) then
          !c+++ linear==> wkyy
          call intrp(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        else
          !c+++ spline ==> wkyy
          call spline(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        endif
        if (ologl) then
          !c+++ log-linear
          wkyy(1:okmax) = exp(wkyy(1:okmax))
        endif
      else
        wkyy(1:okmax) = undef
      endif
    
      !c+++ output data
      do k = 1, okmax
        if (orout) then
          !c+++ reverse
          kk = okmax - k + 1
        else
          !c+++ original
          kk = k
        endif
        oval(i,j,k) = wkyy(kk)
      enddo !! k
    enddo !! i
  enddo !! j

  return
end subroutine intplt_z

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine intpltz
!c  vertical interpolation (1-d data)
!c  [history]
!c  2017/02/15 Yamashita
!c=====
subroutine intpltz(ikmax, okmax, undef, ip, op, & 
&  ps, h, fact, offset, olinear, ologl, omiss, ival, oval)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: ikmax, okmax   !! input/output z-sizes
  real(kind=r8b), intent(in)    :: undef          !! undefined values
  real(kind=r4b), intent(in)    :: ip(ikmax)      !! input p-levs. (hPa)
  real(kind=r4b), intent(in)    :: op(okmax)      !! output p-levs (hPa)
  real(kind=r4b), intent(in)    :: ps             !! surface pressure (hPa)
  real(kind=r8b), intent(in)    :: h              !! scale height (m)
  real(kind=r8b), intent(in)    :: fact, offset   !! factor, offset
  logical, intent(in)           :: olinear        !! true: linear, false: spline
  logical, intent(in)           :: ologl          !! true: log-linear, false: linear
  logical, intent(in)           :: omiss          !! true: enable extrapolation, false: disable
  real(kind=r4b), intent(in)    :: ival(ikmax)    !! input variable
  !c+++ [output]
  real(kind=r4b), intent(out)   :: oval(okmax)    !! output variable
  !c+++ [internal work]
  integer(kind=i4b)             :: k              !! loop variable
  integer(kind=i4b)             :: ik, kk         !! z-grid id
  real(kind=r8b)                :: wkx (ikmax)    !! input axis
  real(kind=r8b)                :: wky (ikmax)    !! input data
  real(kind=r8b)                :: wkxx(okmax)    !! output axis
  real(kind=r8b)                :: wkyy(okmax)    !! output data
  real(kind=r8b)                :: height, hs, he !! height (m)
  real(kind=r8b)                :: wxsgn          !! sign of axis data
  logical                       :: orinp          !! true: reverse input data, false: disable
  logical                       :: orout          !! true: reverse output data, false: disable
!c
!c axis, input data
!c===
  !c+++ input axis
  hs = - h * log(ip(1) / ps)
  he = - h * log(ip(ikmax) / ps)
  orinp = .false.
  if (hs < he) orinp = .true.
  !c+++ output axis
  hs = - h * log(op(1) / ps)
  he = - h * log(op(okmax) / ps)
  orout = .false.
  if (hs < he) orout = .true.
  wxsgn = 1.d0

  ik = 0
  do k = 1, ikmax
    !c+++ set input data ==> wkx, wky
    if (ival(k) /= undef) then
      ik = ik + 1
      if (orinp) then
        !c+++ reverse
        kk = ikmax - ik + 1
      else
        !c+++ original
        kk = ik
      endif
      height = - h * log(ip(k) / ps)
      wkx(kk) = wxsgn * height
      if (ologl) then
        !c+++ log-linear
        wky(kk) = log(ival(k) * fact + offset)
      else
        !c+++ normal
        wky(kk) = ival(k) * fact + offset
      endif
    endif
  enddo !! k
  !ccc write(6, *) 'orinp = ', orinp, 'orout = ', orout
  !ccc write(6, *) 'ps = ', ps, 'h = ', h
  !ccc write(6, *) 'pinp = ', ip(1:ikmax)
  !ccc write(6, *) 'zinp = ', wkx(1:ikmax)
  !ccc write(6, *) 'ival = ', wky(1:ikmax)

  !c+++ output p ==> wkxx
  do k = 1, okmax
    if (orout) then
      !c+++ reverse
      kk = okmax - k + 1
    else
      !c+++ original
      kk = k
    endif
    height = - h * log(op(k) / ps)
    wkxx(kk) = wxsgn * height
  enddo !! k
  !ccc write(6, *) 'pout = ', op(1:ikmax)
  !ccc write(6, *) 'zout = ', wkxx(1:okmax)

!c
!c vertical interpolation
!c===
  if (ik /= 0) then
    if (olinear) then
      !c+++ linear==> wkyy
      call intrp(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&          wkyy, &
&          wkxx, okmax, omiss, undef)
    else
      !c+++ spline ==> wkyy
      call spline(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&          wkyy, &
&          wkxx, okmax, omiss, undef)
    endif
    if (ologl) then
      !c+++ log-linear
      wkyy(1:okmax) = exp(wkyy(1:okmax))
    endif
  else
    wkyy(1:okmax) = undef
  endif
  !ccc write(6, *) 'oval = ', wkyy(1:okmax)

!c
!c output data
!c===
  do k = 1, okmax
    if (orout) then
      !c+++ reverse
      kk = okmax - k + 1
    else
      !c+++ original
      kk = k
    endif
    oval(k) = wkyy(kk)
  enddo !! k

  return
end subroutine intpltz

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c 
!c  subroutine intplt_s2p
!c  vertical interpolation for 3D variable
!c  input: sig-levs, output: p-levs
!c=====
subroutine intplt_s2p(imax, jmax, ikmax, okmax, undef, ip, op, itype, omiss, ips, ival, oval)
  !c+++ [input]
  !c+++ size
  integer(kind=i4b), intent(in) :: imax                  !! number of x-grids
  integer(kind=i4b), intent(in) :: jmax                  !! number of y-grids
  integer(kind=i4b), intent(in) :: ikmax                 !! number of z-grids for input
  integer(kind=i4b), intent(in) :: okmax                 !! number of z-grids for output
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                 !!
  !c+++ axis
  real(kind=r8b), intent(in)    :: ip(ikmax)             !! input sig-axis data []
  real(kind=r8b), intent(in)    :: op(okmax)             !! output p-axis data [hPa]
  !c+++ switch
  character(len=*), intent(in)  :: itype                 !! intplt type; l: linear, s: spline
  logical, intent(in)           :: omiss                 !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ips(imax,jmax)        !! input surface pressure, ps [hPa]
  real(kind=r8b), intent(in)    :: ival(imax,jmax,ikmax) !! input variable (sig-levs)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: oval(imax,jmax,okmax) !! output variable (p-levs)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, ik           !!
  real(kind=r8b)                :: wkx (ikmax)           !! input axis
  real(kind=r8b)                :: wky (ikmax)           !! input data
  real(kind=r8b)                :: wkxx(okmax)           !! output axis
  real(kind=r8b)                :: wkyy(okmax)           !! output data
  real(kind=r8b)                :: pressure              !!
  logical                       :: olinear               !! true: linear, false: spline

  if (itype == 'l') then
    olinear = .true.  !! true: linear
  else if (itype == 's') then
    olinear = .false. !! false: spline
  else
    write(6, *) 'Error: invalid input itype = ', itype
    stop 2
  endif

!c
!c vertical interpolation
!c===
  do j = 1, jmax
    do i = 1, imax
      ik = 0
      do k = 1, ikmax
        !c+++ set input data ==> wkx (axis), wky (data)
        !c+++ sig to p ==> wkx
        if (ival(i,j,k) /= undef.and.ips(i,j) /= undef) then
          ik = ik + 1
          pressure = ip(k) * ips(i,j) !! p = sig * ps
          wkx(ikmax-ik+1) = - pressure
          wky(ikmax-ik+1) = ival(i,j,k)
        endif
      enddo !! k
      !c+++ output p ==> wkxx
      do k=1, okmax
        pressure = op(k)
        wkxx(okmax-k+1) = - pressure
      enddo !! k

      if (ik /= 0) then
        if (olinear) then
          !c+++ linear ==> wkyy
          call intrp(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        else
          !c+++ spline ==> wkyy
          call spline(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        endif
      else
        wkyy(1:okmax) = undef
      endif

      do k = 1, okmax
        oval(i,j,k) = wkyy(okmax-k+1)
      enddo !! k
    enddo !! i
  enddo !! j

  return
end subroutine intplt_s2p

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c 
!c  subroutine intplt_h2p
!c  vertical interpolation for 3D variable
!c  input: eta-levs, output: p-levs
!c=====
subroutine intplt_h2p(imax, jmax, ikmax, okmax, undef, oeta_fa, oeta_fb, op, itype, omiss, ips, ival, oval)
  !c+++ [input]
  !c+++ size
  integer(kind=i4b), intent(in) :: imax                  !! number of x-grids
  integer(kind=i4b), intent(in) :: jmax                  !! number of y-grids
  integer(kind=i4b), intent(in) :: ikmax                 !! number of z-grids for input
  integer(kind=i4b), intent(in) :: okmax                 !! number of z-grids for output
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                 !!
  !c+++ axis
  real(kind=r8b), intent(in)    :: oeta_fa(ikmax)        !! input eta half lev.
  real(kind=r8b), intent(in)    :: oeta_fb(ikmax)        !! input eta half lev.
  real(kind=r8b), intent(in)    :: op(okmax)             !! output p-axis data [hPa]
  !c+++ switch
  character(len=*), intent(in)  :: itype                 !! intplt type; l: linear, s: spline
  logical, intent(in)           :: omiss                 !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ips(imax,jmax)        !! input surface pressure, ps [hPa]
  real(kind=r8b), intent(in)    :: ival(imax,jmax,ikmax) !! input variable (eta-levs)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: oval(imax,jmax,okmax) !! output variable (p-levs)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, ik           !!
  real(kind=r8b)                :: wkx (ikmax)           !! input axis
  real(kind=r8b)                :: wky (ikmax)           !! input data
  real(kind=r8b)                :: wkxx(okmax)           !! output axis
  real(kind=r8b)                :: wkyy(okmax)           !! output data
  real(kind=r8b)                :: pressure              !!
  logical                       :: olinear               !! true: linear, false: spline

  if (itype == 'l') then
    olinear = .true.  !! true: linear
  else if (itype == 's') then
    olinear = .false. !! false: spline
  else
    write(6, *) 'Error: invalid input itype = ', itype
    stop 2
  endif

!c
!c vertical interpolation
!c===
  do j = 1, jmax
    do i = 1, imax
      ik = 0
      do k = 1, ikmax
        !c+++ set input data ==> wkx (axis), wky (data)
        !c+++ eta to p ==> wkx
        if (ival(i,j,k) /= undef) then
          ik = ik + 1
          pressure = oeta_fa(k) + oeta_fb(k) * ips(i,j)
          wkx(ikmax-ik+1) = - pressure
          wky(ikmax-ik+1) = ival(i,j,k)
        endif
      enddo !! k
      !c+++ p ==> wkxx
      do k = 1, okmax
        pressure = op(k)
        wkxx(okmax-k+1) = - pressure
      enddo !! k

      if (ik /= 0) then
        if (olinear) then
          !c+++ linear ==> wkyy
          call intrp(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        else
          !c+++ spline ==> wkyy
          call spline(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        endif
      else
        wkyy(1:okmax) = undef
      endif

      do k = 1, okmax
        oval(i,j,k) = wkyy(okmax-k+1)
      enddo !! k
    enddo !! i
  enddo !! j

  return
end subroutine intplt_h2p

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c 
!c  subroutine intplt_p2s
!c  vertical interpolation for 3D variable
!c  input: p-levs, output: sig-levs
!c=====
subroutine intplt_p2s(imax, jmax, ikmax, okmax, undef, ip, op, itype, omiss, ips, ival, oval)
  !c+++ [input]
  !c+++ size
  integer(kind=i4b), intent(in) :: imax                  !! number of x-grids
  integer(kind=i4b), intent(in) :: jmax                  !! number of y-grids
  integer(kind=i4b), intent(in) :: ikmax                 !! number of z-grids for input
  integer(kind=i4b), intent(in) :: okmax                 !! number of z-grids for output
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                 !!
  !c+++ axis
  real(kind=r8b), intent(in)    :: ip(ikmax)             !! input p-axis data [hPa]
  real(kind=r8b), intent(in)    :: op(okmax)             !! output sig-axis data []
  !c+++ switch
  character(len=*), intent(in)  :: itype                 !! intplt type; l: linear, s: spline
  logical, intent(in)           :: omiss                 !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ips(imax,jmax)        !! input surface pressure, ps [hPa]
  real(kind=r8b), intent(in)    :: ival(imax,jmax,ikmax) !! input variable (p-levs)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: oval(imax,jmax,okmax) !! output variable (sig-levs)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, ik           !!
  real(kind=r8b)                :: wkx (ikmax)           !! input axis
  real(kind=r8b)                :: wky (ikmax)           !! input data
  real(kind=r8b)                :: wkxx(okmax)           !! output axis
  real(kind=r8b)                :: wkyy(okmax)           !! output data
  real(kind=r8b)                :: pressure              !!
  logical                       :: olinear               !! true: linear, false: spline

  if (itype == 'l') then
    olinear = .true.  !! true: linear
  else if (itype == 's') then
    olinear = .false. !! false: spline
  else
    write(6, *) 'Error: invalid input itype = ', itype
    stop 2
  endif

!c
!c vertical interpolation
!c===
  do j = 1, jmax
    do i = 1, imax
      ik = 0
      do k = 1, ikmax
        !c+++ set input data ==> wkx (axis), wky (data)
        if (ival(i,j,k) /= undef.and.ips(i,j) /= undef) then
          ik = ik + 1
          pressure = ip(k)
          wkx(ikmax-ik+1) = - pressure
          wky(ikmax-ik+1) = ival(i,j,k)
        endif
      enddo !! k
      !c+++ sig to p ==> wkxx
      do k=1, okmax
        pressure = op(k) * ips(i,j) !! p = sig * ps
        wkxx(okmax-k+1) = - pressure
      enddo !! k

      if (ik /= 0) then
        if (olinear) then
          !c+++ linear ==> wkyy
          call intrp(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        else
          !c+++ spline ==> wkyy
          call spline(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        endif
      else
        wkyy(1:okmax) = undef
      endif

      do k = 1, okmax
        oval(i,j,k) = wkyy(okmax-k+1)
      enddo !! k
    enddo !! i
  enddo !! j

  return
end subroutine intplt_p2s

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c 
!c  subroutine intplt_p2h
!c  vertical interpolation for 3D variable
!c  input: p-levs, output: eta-levs
!c=====
subroutine intplt_p2h(imax, jmax, ikmax, okmax, undef, ip, oeta_fa, oeta_fb, itype, omiss, ips, ival, oval)
  !c+++ [input]
  !c+++ size
  integer(kind=i4b), intent(in) :: imax                  !! number of x-grids
  integer(kind=i4b), intent(in) :: jmax                  !! number of y-grids
  integer(kind=i4b), intent(in) :: ikmax                 !! number of z-grids for input
  integer(kind=i4b), intent(in) :: okmax                 !! number of z-grids for output
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                 !!
  !c+++ axis
  real(kind=r8b), intent(in)    :: ip(ikmax)             !! input z-axis data
  real(kind=r8b), intent(in)    :: oeta_fa(okmax)        !! output eta half lev.
  real(kind=r8b), intent(in)    :: oeta_fb(okmax)        !! output eta half lev.
  !c+++ switch
  character(len=*), intent(in)  :: itype                 !! intplt type; l: linear, s: spline
  logical, intent(in)           :: omiss                 !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ips(imax,jmax)        !! input surface pressure, ps [hPa]
  real(kind=r8b), intent(in)    :: ival(imax,jmax,ikmax) !! input variable (p-levs)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: oval(imax,jmax,okmax) !! output variable (eta-levs)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, ik           !!
  real(kind=r8b)                :: wkx (ikmax)           !! input axis
  real(kind=r8b)                :: wky (ikmax)           !! input data
  real(kind=r8b)                :: wkxx(okmax)           !! output axis
  real(kind=r8b)                :: wkyy(okmax)           !! output data
  real(kind=r8b)                :: pressure              !!
  logical                       :: olinear               !! true: linear, false: spline

  if (itype == 'l') then
    olinear = .true.  !! true: linear
  else if (itype == 's') then
    olinear = .false. !! false: spline
  else
    write(6, *) 'Error: invalid input itype = ', itype
    stop 2
  endif

!c
!c vertical interpolation
!c===
  do j = 1, jmax
    do i = 1, imax
      ik = 0
      do k = 1, ikmax
        !c+++ set input data ==> wkx (axis), wky (data)
        if (ival(i,j,k) /= undef) then
          ik = ik + 1
          pressure = ip(k)
          wkx(ikmax-ik+1) = - pressure
          wky(ikmax-ik+1) = ival(i,j,k)
        endif
      enddo !! k
      !c+++ eta to p ==> wkxx
      do k = 1, okmax
        pressure = oeta_fa(k) + oeta_fb(k) * ips(i,j)
        wkxx(okmax-k+1) = - pressure
      enddo !! k

      if (ik /= 0) then
        if (olinear) then
          !c+++ linear ==> wkyy
          call intrp(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        else
          !c+++ spline ==> wkyy
          call spline(wky(ikmax-ik+1:ikmax), wkx(ikmax-ik+1:ikmax), ik, &
&              wkyy, &
&              wkxx, okmax, omiss, undef)
        endif
      else
        wkyy(1:okmax) = undef
      endif

      do k = 1, okmax
        oval(i,j,k) = wkyy(okmax-k+1)
      enddo !! k
    enddo !! i
  enddo !! j

  return
end subroutine intplt_p2h

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c 
!c  subroutine intplt_t
!c  time interpolation for 3D variable
!c=====
subroutine intplt_t(imax, jmax, kmax, nt_i, nt_o, undef, t_i, t_o, itype, omiss, ival, oval)
  !c+++ [input]
  !c+++ grid sizes
  integer(kind=i4b), intent(in) :: imax                      !! number of x-grids
  integer(kind=i4b), intent(in) :: jmax                      !! number of y-grids
  integer(kind=i4b), intent(in) :: kmax                      !! number of z-grids
  !c+++ number of records
  integer(kind=i4b), intent(in) :: nt_i                      !! number of input t-axis
  integer(kind=i4b), intent(in) :: nt_o                      !! number of output t-axis
  !c+++ undefined value
  real(kind=r8b), intent(in)    :: undef                     !! undefined values
  !c+++ axis
  real(kind=r8b), intent(in)    :: t_i(nt_i)                 !! input t-axis data
  real(kind=r8b), intent(in)    :: t_o(nt_o)                 !! output t-axis data
  !c+++ switch
  character(len=*), intent(in)  :: itype                     !! intplt type; l: linear, s: spline
  logical, intent(in)           :: omiss                     !! true: enable extrapolation, false: disable
  !c+++ input data
  real(kind=r8b), intent(in)    :: ival(imax,jmax,kmax,nt_i) !! input variable
  !c+++ [output]
  !c+++ output data
  real(kind=r8b), intent(out)   :: oval(imax,jmax,kmax,nt_o) !! output variable
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, it               !! loop variables
  integer(kind=i4b)             :: n                         !!
  real(kind=r8b)                :: wkx (nt_i)                !! input axis
  real(kind=r8b)                :: wky (nt_i)                !! input data
  real(kind=r8b)                :: wkxx(nt_o)                !! output axis
  real(kind=r8b)                :: wkyy(nt_o)                !! output data
  real(kind=r8b)                :: time                      !!
  logical                       :: olinear                   !! true: linear, false: spline

  if (itype == 'l') then
    write(6, *) 'linear interpolation'
    olinear = .true.  !! true: linear
  else if (itype == 's') then
    write(6, *) 'spline interpolation'
    olinear = .false. !! false: spline
  else
    write(6, *) 'Error: invalid input itype = ', itype
    stop 2
  endif
!c
!c time interpolation
!c===
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        n = 0
        do it = 1, nt_i
          !c+++ set input data ==> wkx, wky
          if (ival(i,j,k,it) /= undef) then
            n = n + 1
            time = t_i(it)
            wkx(nt_i-n+1) = time
            wky(nt_i-n+1) = ival(i,j,k,it)
          endif
        enddo !! it
        !c+++ set output axis ==> wkxx
        do it = 1, nt_o
          time = t_o(it)
          wkxx(nt_o-it+1) = time
        enddo !! it
  
        if (n /= 0) then
          if (olinear) then
            !c+++ linear ==> wkyy
            call intrp(wky(nt_i-n+1:nt_i), wkx(nt_i-n+1:nt_i), n, &
  &              wkyy, &
  &              wkxx, nt_o, omiss, undef)
          else
            !c+++ spline ==> wkyy
            call spline(wky(nt_i-n+1:nt_i), wkx(nt_i-n+1:nt_i), n, &
  &              wkyy, &
  &              wkxx, nt_o, omiss, undef)
          endif
        else
          wkyy(1:nt_o) = undef
        endif
  
        do it = 1, nt_o
          oval(i,j,k,it) = wkyy(nt_o-it+1)
        enddo !! it
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine intplt_t

!c----------------------------------------------------------------------c

!c======================================================================c

end module intplt
