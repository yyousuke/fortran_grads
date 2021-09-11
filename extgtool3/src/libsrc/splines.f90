!c
!c  module splines
!c  [history]
!c  2005/10/11 Yamashita: first ver.
!c  2011/11/23 Yamashita: bug fix for extrapolation
!c  2017/06/05 Yamashita: use common_typedef
!c
!c  zin(nzin) ==> zout(nzout) using spline interpolation
!c
!c  public:
!c  subroutine spline: spline interpolation
!c    internal subroutine coef: 
!c
!c======================================================================c
module splines
  use common_typedef, only: i4b, r8b
  implicit none
  private
  public spline

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine spline
!c  spline interpolation
!c=====
subroutine spline(zin, xin, nzin, zout, xout, nzout, omiss, undef)
  !c+++ [input]
  real(kind=r8b), intent(in)    :: zin(nzin)   !! input data
  real(kind=r8b), intent(in)    :: xin(nzin)   !! input data grid
  integer(kind=i4b), intent(in) :: nzin        !! number of grids for input data
  real(kind=r8b), intent(in)    :: xout(nzout) !! output data grid
  integer(kind=i4b), intent(in) :: nzout       !! number of grids for output data
  logical, intent(in)           :: omiss       !! true: enable extrapolation, false: disable
  real(kind=r8b), intent(in)    :: undef       !! undefined value
  !c+++ [output]
  real(kind=r8b), intent(out)   :: zout(nzout) !! output data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j        !!
  real(kind=r8b)                :: x(0:nzin-1) !! grid points
  real(kind=r8b)                :: a(0:nzin-1) !! coefficient a
  real(kind=r8b)                :: b(0:nzin-1) !! coefficient b
  real(kind=r8b)                :: c(0:nzin-1) !! coefficient c
  real(kind=r8b)                :: d(0:nzin-1) !! coefficient d
  
  x(0:nzin-1) = xin(1:nzin)
  a(0:nzin-1) = zin(1:nzin)
  call coef(nzin-1, x, a, b, c, d)
  
  zout(1:nzout) = undef
  do i = 1, nzout
    do j = 0, nzin - 2
      if ( xout(i) <= x(j) .and. xout(i) >= x(j+1) ) then
        zout(i) = a(j) + b(j) * ( xout(i) - x(j) )     &
&                      + c(j) * ( xout(i) - x(j) )**2  &
&                      + d(j) * ( xout(i) - x(j) )**3
      endif 
    enddo  ! j
    if ( omiss ) then
      if ( xout(i) >= x(0) ) then
        zout(i) = a(0) + b(0) * ( xout(i) - x(0) )     &
&                      + c(0) * ( xout(i) - x(0) )**2  &
&                      + d(0) * ( xout(i) - x(0) )**3
      elseif ( xout(i) <= x(nzin-1) ) then
        zout(i) = a(nzin-2) + b(nzin-2) * ( xout(i) - x(nzin-2) )     &
&                           + c(nzin-2) * ( xout(i) - x(nzin-2) )**2  &
&                           + d(nzin-2) * ( xout(i) - x(nzin-2) )**3
      endif
    endif
  enddo  ! i
  
  return
end subroutine spline

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine coef
!c  coefs. of spline interpolation
!c=====
subroutine coef(n, x, a, b, c, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: n          !! 
  real(kind=r8b), intent(in)    :: x(0:n)     !! grid point
  real(kind=r8b), intent(in)    :: a(0:n)     !! coefficient a
  !c+++ [output]
  real(kind=r8b), intent(out)   :: b(0:n)     !! coefficient b
  real(kind=r8b), intent(out)   :: c(0:n)     !! coefficient c
  real(kind=r8b), intent(out)   :: d(0:n)     !! coefficient d
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j       !!
  real(kind=r8b)                :: h(0:n)     !!
  real(kind=r8b)                :: L(0:n)     !!
  real(kind=r8b)                :: mu(0:n)    !!
  real(kind=r8b)                :: z(0:n)     !!
  real(kind=r8b)                :: alpha(0:n) !!
  
  !c+++ (1), (2)
  h(0) = x(1) - x(0)
  do i = 1, n - 1
    h(i) = x(i+1) - x(i)
    alpha(i) = 3.d0 / h(i) * ( a(i+1) - a(i) ) - 3.d0 / h(i-1) * ( a(i) - a(i-1) )
  enddo
  !c+++ (3)
  L(0) = 1.d0
  mu(0) = 0.d0
  z(0) = 0.d0
  
  !c+++ (4)
  do i = 1, n - 1
    L(i) = 2.d0 * ( x(i+1) - x(i-1) ) - h(i-1) * mu(i-1)
    mu(i) = h(i) / L(i)
    z(i) = ( alpha(i) - h(i-1) * z(i-1) ) / L(i)
  enddo
  !c+++ (5)
  L(n) = 1.d0
  z(n) = 0.d0
  c(n) = 0.d0
  
  !c+++ (6)
  do j = n - 1, 0, -1
    c(j) = z(j) - mu(j) * c(j+1)
    b(j) = ( a(j+1) - a(j) ) / h(j) - h(j) * ( c(j+1) + 2.d0 * c(j) ) / 3.d0
    d(j) = ( c(j+1) - c(j) ) / ( 3.d0 * h(j) )
  enddo
  b(n) = b(n-1) + h(n-1) * c(n-1)
  d(n) = d(n-1)
  
  return
end subroutine coef

!c---------------------------------------------------------------------c

!c======================================================================c

end module splines
