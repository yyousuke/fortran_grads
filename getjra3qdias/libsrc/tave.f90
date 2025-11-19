!c
!c  module tave
!c  time mean
!c   [history]
!c     2004/12/01 Yamashita
!c     2005/09/27 f77/f90 (Yamashita)
!c     2005/10/07 標準コーディングルールに沿うように変更 (Yamashita)
!c     2005/11/11 add TIMEave2d
!c     2006/07/04 bug fix (for nx = 1)
!c     2008/12/25 add TIMEave3d, TIMEave4d
!c
!c  integer nx; number of x-grid  ny; number of y-grid  
!c          np; number of p-grid  nt; number of t-grid
!c          
!c  real*8  undef; undefined value
!c
!c    INPUT; real*4 data(nx,ny,np,nt)
!c
!c   OUTPUT; real*4 tmean(nx,ny,np)  ! caution : not 8 byte
!c
!c=====================================================================c
module tave
  implicit none
  private
  public :: TIMEave, TIMEave2d, TIMEave3d, TIMEave4d

  contains

!c=====================================================================c
subroutine TIMEave(nx, ny, np, nt, undef, data, tmean)
!c+++ input
  integer(4), intent(in) :: nx, ny, np, nt
  real(4), intent(in) :: data(nx,ny,np,nt)
  real(8), intent(in) :: undef
!c+++ output
  real(4), intent(out) :: tmean(nx,ny,np)
!c+++ work
  integer(4) :: i, j, k, it, iflag(nx,ny,np)

  tmean(1:nx,1:ny,1:np) = 0.d0
  iflag(1:nx,1:ny,1:np) = 0
  do it = 1, nt
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          if (data(i,j,k,it) /= undef) then
            tmean(i,j,k) = tmean(i,j,k) + DBLE(data(i,j,k,it))
            iflag(i,j,k) = iflag(i,j,k) + 1 
          endif
        enddo !! i
      enddo !! j
    enddo !! k
  enddo !! it

  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (iflag(i,j,k) /= 0) then
          tmean(i,j,k) = tmean(i,j,k) / DBLE(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine TIMEave

!c----------------------------------------------------------------------c
!c  subroutine TIMEave2d
!c  [history]
!c    2004/08/22 Yamashita
!c    2005/11/11 標準コーディングルールに沿って変更(Yamashita)
!c 
!c    INPUT; real*4 data1(nx,ny,np,nt), data2(nx,ny,np,nt)
!c   OUTPUT; real*4 tmean(nx,ny,np)
!c=====
subroutine TIMEave2d(nx, ny, np, nt, itsta, itend, undef, data1, data2, tmean)
!c+++ input
  integer(4), intent(in) :: nx, ny, np, nt, itsta, itend
  real(4), intent(in) :: data1(nx,ny,np,nt), data2(nx,ny,np,nt)
  real(8), intent(in) :: undef
!c+++ output
  real(4), intent(out) :: tmean(nx,ny,np)
!c+++ work
  integer(4) :: i, j, k, it, iflag(nx,ny,np)
!c
!c sum
!c===
  tmean(1:nx,1:ny,1:np) = 0.d0
  iflag = 0
  do it = itsta, itend
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          if (data1(i,j,k,it) /= undef.and.data2(i,j,k,it) /= undef) then
            tmean(i,j,k) = tmean(i,j,k) + data1(i,j,k,it) * DBLE(data2(i,j,k,it))
            iflag(i,j,k) = iflag(i,j,k) + 1 
          endif
        enddo !! i
      enddo !! j
    enddo !! k
  enddo !! it
!c
!c average
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (iflag(i,j,k) /= 0) then
          tmean(i,j,k) = tmean(i,j,k) / DBLE(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k
      
  return
end subroutine TIMEave2d

!c----------------------------------------------------------------------c
!c  subroutine TIMEave3d
!c  [history]
!c    2008/12/25 Yamashita
!c 
!c    INPUT; real*4 data1(nx,ny,np,nt), data2(nx,ny,np,nt), data3(nx,ny,np,nt)
!c   OUTPUT; real*4 tmean(nx,ny,np)
!c=====
subroutine TIMEave3d(nx, ny, np, nt, itsta, itend, undef, data1, data2, data3, tmean)
!c+++ input
  integer(4), intent(in) :: nx, ny, np, nt, itsta, itend
  real(4), intent(in) :: data1(nx,ny,np,nt), data2(nx,ny,np,nt), data3(nx,ny,np,nt)
  real(8), intent(in) :: undef
!c+++ output
  real(4), intent(out) :: tmean(nx,ny,np)
!c+++ work
  integer(4) :: i, j, k, it, iflag(nx,ny,np)
!c
!c sum
!c===
  tmean(1:nx,1:ny,1:np) = 0.d0
  iflag = 0
  do it = itsta, itend
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          if (data1(i,j,k,it) /= undef.and.data2(i,j,k,it) /= undef &
&           .and.data3(i,j,k,it) /= undef) then
            tmean(i,j,k) = tmean(i,j,k) &
&             + data1(i,j,k,it) * data2(i,j,k,it) * DBLE(data3(i,j,k,it))
            iflag(i,j,k) = iflag(i,j,k) + 1 
          endif
        enddo !! i
      enddo !! j
    enddo !! k
  enddo !! it
!c
!c average
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (iflag(i,j,k) /= 0) then
          tmean(i,j,k) = tmean(i,j,k) / DBLE(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k
      
  return
end subroutine TIMEave3d

!c----------------------------------------------------------------------c
!c  subroutine TIMEave4d
!c  [history]
!c    2008/12/25 Yamashita
!c 
!c    INPUT; real*4 data1(nx,ny,np,nt), data2(nx,ny,np,nt)
!c           real*4 data3(nx,ny,np,nt), data4(nx,ny,np,nt)
!c   OUTPUT; real*4 tmean(nx,ny,np)
!c=====
subroutine TIMEave4d(nx, ny, np, nt, itsta, itend, undef, data1, data2, data3, data4, tmean)
!c+++ input
  integer(4), intent(in) :: nx, ny, np, nt, itsta, itend
  real(4), intent(in) :: data1(nx,ny,np,nt), data2(nx,ny,np,nt)
  real(4), intent(in) :: data3(nx,ny,np,nt), data4(nx,ny,np,nt)
  real(8), intent(in) :: undef
!c+++ output
  real(4), intent(out) :: tmean(nx,ny,np)
!c+++ work
  integer(4) :: i, j, k, it, iflag(nx,ny,np)
!c
!c sum
!c===
  tmean(1:nx,1:ny,1:np) = 0.d0
  iflag = 0
  do it = itsta, itend
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          if (data1(i,j,k,it) /= undef.and.data2(i,j,k,it) /= undef &
&           .and.data3(i,j,k,it) /= undef.and.data4(i,j,k,it) /= undef) then
            tmean(i,j,k) = tmean(i,j,k) + data1(i,j,k,it) * data2(i,j,k,it) &
&             * data3(i,j,k,it) * DBLE(data4(i,j,k,it))
            iflag(i,j,k) = iflag(i,j,k) + 1 
          endif
        enddo !! i
      enddo !! j
    enddo !! k
  enddo !! it
!c
!c average
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (iflag(i,j,k) /= 0) then
          tmean(i,j,k) = tmean(i,j,k) / DBLE(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k
      
  return
end subroutine TIMEave4d
!c----------------------------------------------------------------------c

end module tave
