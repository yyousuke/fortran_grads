!c  module zmean
!c  zonal mean
!c  [history] 
!c  2004/08/05 Yamashita: first ver.
!c  2005/09/27 Yamashita: f77/f90
!c  2005/10/07 Yamashita: 標準コーディングルールに沿うように変更
!c  2017/06/01 Yamashita: use common_typedef
!c
!c  integer nx; number of x-grid  ny; number of y-grid  np; number of p-grid
!c  real(kind=r8b)  undef; undefined value
!c
!c======================================================================c
module zmean
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: ZONALmean, ZONALmean2 

contains

!c======================================================================c

!c---------------------------------------------------------------------c
!c  subroutine ZONALmean
!c    INPUT; real(kind=r4b) data(nx,ny,np)
!c
!c   OUTPUT; real(kind=r4b) zonal(ny,np)
!c=====
subroutine ZONALmean(nx, ny, np, undef, data, zonal)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r4b), intent(in)    :: data(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: undef          !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: zonal(ny,np)   !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: iflag          !!

  do k = 1, np
    do j = 1, ny
      zonal(j,k) = 0.0
      iflag = 0
      do i = 1, nx
        if (data(i,j,k) /= undef) then
          zonal(j,k) = zonal(j,k) + DBLE(data(i,j,k))
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
end subroutine ZONALmean

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine ZONALmean2
!c    INPUT; real(kind=r4b) data(nx,ny,np,nv)
!c
!c   OUTPUT; real(kind=r4b) zonal(ny,np,nv)
!c=====
subroutine ZONALmean2(nx, ny, np, nv, undef, data, zonal)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nv    !!
  real(kind=r4b), intent(in)    :: data(nx,ny,np,nv) !!
  real(kind=r8b), intent(in)    :: undef             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: zonal(ny,np,nv)   !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, iv       !!
  integer(kind=i4b)             :: iflag             !!

  do iv = 1, nv
    do k = 1, np
      do j = 1, ny
        zonal(j,k,iv) = 0.0
        iflag = 0
        do i = 1, nx
          if (data(i,j,k,iv) /= undef) then
            zonal(j,k,iv) = zonal(j,k,iv) + DBLE(data(i,j,k,iv))
            iflag = iflag + 1 
          endif
        enddo  !! i
        if (iflag /= 0) then
           zonal(j,k,iv) = zonal(j,k,iv) / DBLE(iflag)
        else
          zonal(j,k,iv) = undef
        endif
      enddo  !! j
    enddo  !! k
  enddo  !! iv
  
  return
end subroutine ZONALmean2

!c---------------------------------------------------------------------c

!c======================================================================c

end module zmean
