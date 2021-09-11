!c
!c  module calculate
!c  [history]
!c  2005/06/03 Yamashita: first ver.
!c  2005/10/07 Yamashita: 標準コーディングルールに合うように変更
!c  2005/10/14 Yamashita: add spd 
!c  2006/01/09 Yamashita: divで0で割らないように修正
!c  2006/01/20 Yamashita: add square
!c  2008/06/20 Yamashita: add absval
!c  2011/07/15 Yamashita: add shift, maskout
!c  2017/06/05 Yamashita: use common_typedef
!c
!c  integer nx; number of x-grid  ny;number of y-grid
!c          np; number of p-grid
!c
!c  const
!c      real*8 fact1, fact2, ofs1, ofs2; A*fact1+ofs2, B*fact2+ofs2
!c      real*8 undef; undefined value
!c=====================================================================c
module calculate
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: add, sub, mlt, div, square, spd, absval, shift, maskout

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine:  add
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c      real*4 data2(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); (data1*fact1 + ofs1) + (data2*fact2 + ofs2)
!c=====
subroutine add(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r4b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c data1 + data2
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef) then
          dout(i,j,k) = (data1(i,j,k) * fact1 + ofs1)                    &
&                     + (data2(i,j,k) * fact2 + ofs2)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine add

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  sub
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c      real*4 data2(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); (data1*fact1 + ofs1) - (data2*fact2 + ofs2)
!c=====
subroutine sub(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r4b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c data1 - data2
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef) then
          dout(i,j,k) = (data1(i,j,k) * fact1 + ofs1)                    &
&                     - (data2(i,j,k) * fact2 + ofs2)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine sub

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  mlt
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c      real*4 data2(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); (data1*fact1 + ofs1) * (data2*fact2 + ofs2)
!c=====
subroutine mlt(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r4b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c data1 * data2
!c===
   do k = 1, np
     do j = 1, ny
       do i = 1, nx
         if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef) then
           dout(i,j,k) = (data1(i,j,k) * fact1 + ofs1)                   &
&                      * DBLE(data2(i,j,k) * fact2 + ofs2)
         else
           dout(i,j,k) = undef
         endif
       enddo !! i
     enddo !! j
   enddo !! k

  return
end subroutine mlt

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  div
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c      real*4 data2(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); (data1*fact1 + ofs1) / (data2*fact2 + ofs2)
!c=====
subroutine div(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r4b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c data1 / data2
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef             &
&         .and.data2(i,j,k) /= 0) then
          dout(i,j,k) = (data1(i,j,k) * fact1 + ofs1)                    &
&                     / DBLE(data2(i,j,k) * fact2 + ofs2)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine div

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: square
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); SQRT(data1*fact1 + ofs1)
!c=====
subroutine square(nx, ny, np, undef, data1, dout, fact1, ofs1)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1           !!
  real(kind=r8b), intent(in)    :: ofs1            !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c sqrt(data1)
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data1(i,j,k) > 0) then
          dout(i,j,k) = SQRT(data1(i,j,k) * fact1 + ofs1)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine square

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: absval
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); ABS(data1*fact1 + ofs1)
!c=====
subroutine absval(nx, ny, np, undef, data1, dout, fact1, ofs1)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1           !!
  real(kind=r8b), intent(in)    :: ofs1            !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c abs(data1)
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef) then
          dout(i,j,k) = ABS(data1(i,j,k) * fact1 + ofs1)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine absval

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  spd
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c      real*4 data2(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); SQRT[(data1*fact1 + ofs1)**2 + (data2*fact2 + ofs2)**2]
!c=====
subroutine spd(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r4b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c sqrt(data1**2 + data2**2)
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef) then
          dout(i,j,k) = DSQRT(DBLE(data1(i,j,k) * fact1 + ofs1)**2       &
&                     + DBLE(data2(i,j,k) * fact2 + ofs2)**2)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine spd

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: shift
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); data1*fact1 + ofs1
!c=====
subroutine shift(nx, ny, np, undef, data1, dout, fact1, ofs1)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1           !!
  real(kind=r8b), intent(in)    :: ofs1            !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c data1*fact1 + ofs1
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef) then
          dout(i,j,k) = data1(i,j,k) * fact1 + ofs1
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine shift

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: maskout
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c      real*4 mask(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); maskout(data1,mask)
!c=====
subroutine maskout(nx, ny, np, undef, data1, mask, dout)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r4b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r4b), intent(in)    :: mask(nx,ny,np)  !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c maskout(data1,mask)
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.mask(i,j,k) >= 0.0) then
          dout(i,j,k) = data1(i,j,k)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine maskout

!c---------------------------------------------------------------------c

!c======================================================================c

end module calculate
