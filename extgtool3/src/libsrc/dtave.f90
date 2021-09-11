!c
!c  module dtave
!c  time mean
!c  [history]
!c  2010/10/09 Yamashita: double precison ver. from tave.f90
!c  2017/06/05 Yamashita: use common_typedef
!c
!c  integer nx; number of x-grid  ny; number of y-grid  
!c          np; number of p-grid  nt; number of t-grid
!c          
!c  real*8  undef; undefined value
!c
!c    INPUT; real*8 data(nx,ny,np,nt)
!c
!c   OUTPUT; real*8 tmean(nx,ny,np)  ! caution : not 8 byte
!c
!c=====================================================================c
module dtave
  use common_typedef, only: i4b, r8b
  implicit none
  private
  public :: dTIMEave, dTIMEave2d, dTIMEave3d, dTIMEave4d

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine dTIMEave
!c    INPUT; real(kind=r8b) data(nx,ny,np,nt)
!c
!c   OUTPUT; real(kind=r8b) tmean(nx,ny,np)
!c=====
subroutine dTIMEave(nx, ny, np, nt, undef, din, tmean)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt   !!
  real(kind=r8b), intent(in)    :: undef            !!
  real(kind=r8b), intent(in)    :: din(nx,ny,np,nt) !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: tmean(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, it      !!
  integer(kind=i4b)             :: iflag(nx,ny,np)  !!

  tmean(1:nx,1:ny,1:np) = 0.d0
  iflag(1:nx,1:ny,1:np) = 0
  do it = 1, nt
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          if (din(i,j,k,it) /= undef) then
            tmean(i,j,k) = tmean(i,j,k) + din(i,j,k,it)
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
          tmean(i,j,k) = tmean(i,j,k) / dble(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine dTIMEave

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine dTIMEave2d
!c  [history]
!c  2004/08/22 Yamashita
!c  2005/11/11 標準コーディングルールに沿って変更(Yamashita)
!c  2010/10/09 double precison
!c 
!c    INPUT; real*8 data1(nx,ny,np,nt), data2(nx,ny,np,nt)
!c   OUTPUT; real*8 tmean(nx,ny,np)
!c=====
subroutine dTIMEave2d(nx, ny, np, nt, itsta, itend, undef, data1, data2, tmean)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt     !!
  integer(kind=i4b), intent(in) :: itsta, itend       !!
  real(kind=r8b), intent(in)    :: undef              !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np,nt) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np,nt) !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: tmean(nx,ny,np)    !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, it        !!
  integer(kind=i4b)             :: iflag(nx,ny,np)    !!
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
            tmean(i,j,k) = tmean(i,j,k) + data1(i,j,k,it) * data2(i,j,k,it)
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
          tmean(i,j,k) = tmean(i,j,k) / dble(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k
      
  return
end subroutine dTIMEave2d

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine dTIMEave3d
!c  [history]
!c  2008/12/25 Yamashita
!c  2010/10/09 double precison
!c 
!c    INPUT; real*8 data1(nx,ny,np,nt), data2(nx,ny,np,nt), data3(nx,ny,np,nt)
!c   OUTPUT; real*8 tmean(nx,ny,np)
!c=====
subroutine dTIMEave3d(nx, ny, np, nt, itsta, itend, undef, data1, data2, data3, tmean)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt     !!
  integer(kind=i4b), intent(in) :: itsta, itend       !!
  real(kind=r8b), intent(in)    :: undef              !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np,nt) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np,nt) !!
  real(kind=r8b), intent(in)    :: data3(nx,ny,np,nt) !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: tmean(nx,ny,np)    !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, it        !!
  integer(kind=i4b)             :: iflag(nx,ny,np)    !!
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
&             + data1(i,j,k,it) * data2(i,j,k,it) * data3(i,j,k,it)
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
          tmean(i,j,k) = tmean(i,j,k) / dble(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k
      
  return
end subroutine dTIMEave3d

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine dTIMEave4d
!c  [history]
!c  2008/12/25 Yamashita
!c  2010/10/09 double precison
!c 
!c    INPUT; real*8 data1(nx,ny,np,nt), data2(nx,ny,np,nt)
!c           real*8 data3(nx,ny,np,nt), data4(nx,ny,np,nt)
!c   OUTPUT; real*8 tmean(nx,ny,np)
!c=====
subroutine dTIMEave4d(nx, ny, np, nt, itsta, itend, undef, data1, data2, data3, data4, tmean)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt     !!
  integer(kind=i4b), intent(in) :: itsta, itend       !!
  real(kind=r8b), intent(in)    :: undef              !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np,nt) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np,nt) !!
  real(kind=r8b), intent(in)    :: data3(nx,ny,np,nt) !!
  real(kind=r8b), intent(in)    :: data4(nx,ny,np,nt) !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: tmean(nx,ny,np)    !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k, it        !!
  integer(kind=i4b)             :: iflag(nx,ny,np)    !!
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
&             * data3(i,j,k,it) * data4(i,j,k,it)
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
          tmean(i,j,k) = tmean(i,j,k) / dble(iflag(i,j,k))
        else
          tmean(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k
      
  return
end subroutine dTIMEave4d

!c----------------------------------------------------------------------c

!c======================================================================c

end module dtave
