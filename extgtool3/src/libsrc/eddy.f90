!c
!c  eddy.f90
!c  [history]
!c  2004/08/05 Yamashita: first ver.
!c  2005/09/27 Yamashita: f77/f90
!c  2005/10/07 Yamashita: 標準コーディングルールに沿うように変更
!c  2010/10/18 Yamashita: フォーマットを整理
!c  2017/06/05 Yamashita: use common_typedef
!c
!c======================================================================c
module eddy
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: eddyt, eddyz

  contains

!c======================================================================c

!c---------------------------------------------------------------------c
!c  subroutine eddyt
!c  calcullate eddy component from time mean data
!c
!c   INPUT; data(nx,ny,np) ; original data
!c          mean(nx,ny,np) ; time mean data
!c  OUTPUT; prime(nx,ny,np)
!c=====  
subroutine eddyt(nx, ny, np, undef, data, mean, prime)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !! x-, y-, z-sizes
  real(kind=r4b), intent(in)    :: data(nx,ny,np)  !! original data
  real(kind=r4b), intent(in)    :: mean(nx,ny,np)  !! mean data
  real(kind=r8b), intent(in)    :: undef           !! undefined values
  !c+++ [output]
  real(kind=r4b), intent(out)   :: prime(nx,ny,np) !! deviation data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k         !!
!c
!c eddy
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data(i,j,k) /= undef.and.mean(i,j,k) /= undef) then
           prime(i,j,k) = data(i,j,k) - DBLE(mean(i,j,k))
        else
           prime(i,j,k) = undef
        endif
      enddo  ! i
    enddo  ! j
  enddo  ! k

  return
end subroutine eddyt

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine eddyz
!c  calcullate eddy component from zonal mean data
!c
!c   INPUT; data(nx,ny,np) ; original data
!c          mean(ny,np)    ; zonal mean data
!c  OUTPUT; prime(nx,ny,np)
!c=====  
subroutine eddyz(nx, ny, np, undef, data, mean, prime)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !! x-, y-, z-sizes
  real(kind=r4b), intent(in)    :: data(nx,ny,np)  !! original data
  real(kind=r4b), intent(in)    :: mean(ny,np)     !! zonal mean data
  real(kind=r8b), intent(in)    :: undef           !! undefined values
  !c+++ [output]
  real(kind=r4b), intent(out)   :: prime(nx,ny,np) !! deviation data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k         !!
!c
!c eddy
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data(i,j,k) /= undef.and.mean(j,k) /= undef) then
          prime(i,j,k) = data(i,j,k) - DBLE(mean(j,k))
        else
          prime(i,j,k) = undef
        endif
      enddo ! i
    enddo ! j
  enddo ! k

  return
end subroutine eddyz

!c---------------------------------------------------------------------c

!c======================================================================c

end module eddy
