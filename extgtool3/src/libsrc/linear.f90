!c
!c  module linear
!c  [history]
!c  2008/06/20 Yamashita: first ver. (from mmean.f90 by Yamashita)
!c  2008/12/12 Yamashita: subroutine version with splines.f90
!c  2011/11/23 Yamashita: bug fix for extrapolation
!c  2017/06/05 Yamashita: use common_typedef
!c
!c  zin(nzin) ==> zout(nzout) using linear interpolation
!c   
!c  y_j = y_(i-1) + a x_j 
!c  a=(y_i - y(i-1))/(x_i - x_(i-1))
!c   
!c  i: original grid, j: new grid
!c
!c  public:
!c  subroutine intrp: linear interpolation
!c=====================================================================c
module linear
  use common_typedef, only: i4b, r8b
  implicit none
  private
  public intrp

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine intrp
!c  linear interpolation
!c=====
subroutine intrp(zin, xin, nzin, zout, xout, nzout, omiss, undef)
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
  
  x(0:nzin-1) = xin(1:nzin)
  a(0:nzin-1) = zin(1:nzin)

  !c+++ coef b = (y_(i+1) - y_i)/(x_(i+1) - x_i)
  do j = 0, nzin-2
    b(j) = (a(j+1) - a(j)) / (x(j+1) - x(j))
  enddo !! i

  !c+++ linear interpolation
  zout(1:nzout) = undef
  do i = 1, nzout
    do j = 0, nzin - 2
      !c+++ y_j = y_i + b * x_j
      if (xout(i) <= x(j).and.xout(i) >= x(j+1)) then
        zout(i) = a(j) + b(j) * (xout(i) - x(j))
      endif 
    enddo !! j
    if (omiss) then
      if (xout(i) >= x(0)) then
        zout(i) = a(0) + b(0) * (xout(i) - x(0))
      elseif (xout(i) <= x(nzin-1)) then
        zout(i) = a(nzin-2) + b(nzin-2) * (xout(i) - x(nzin-2))
      endif
    endif
  enddo !! i

  return
end subroutine intrp

!c---------------------------------------------------------------------c

!c=====================================================================c

end module linear
