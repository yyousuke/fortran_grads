!c
!c  module zmean2d
!c  zonal mean for 2 variables
!c  [history]
!c  2004/08/05 Yamashita
!c  2005/08/27 f77/f90 (Yamashita)
!c  2005/10/07 標準コーディングルールに沿うように変更 (Yamashita)
!c  2017/06/01 Yamashita: use common_typedef
!c
!c  integer nx; number of x-grid  ny; number of y-grid  np; number of p-grid
!c
!c=====================================================================c
module zmean2d
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: ZONALmean2d

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine ZONALmean2d
!c
!c    INPUT; real(kind=r4b) data1(nx,ny,np)  data2(nx,ny,np)
!c
!c   OUTPUT; real(kind=r4b) zonal(ny,np)
!c====
subroutine ZONALmean2d(nx, ny, np, undef, data1, data2, zonal)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r4b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: undef           !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: zonal(ny,np)    !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k         !!
  integer(kind=i4b)             :: iflag           !!

  do k = 1, np
    do j = 1, ny
      zonal(j,k) = 0.0
      iflag = 0
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef) then
          zonal(j,k) = zonal(j,k) + data1(i,j,k) * DBLE(data2(i,j,k))
          iflag = iflag + 1 
        endif
      enddo  !! i
      if (iflag /= 0) then
        zonal(j,k) = zonal(j,k) / DBLE(iflag)
      else
        zonal(j,k) = undef
      endif
    enddo  !! j
  enddo  !! k
  
  return
end subroutine ZONALmean2d

!c----------------------------------------------------------------------c

!c======================================================================c

end module zmean2d
