!c
!c  module dcalculate
!c  [history]
!c  2010/10/09 Yamashita: double precision ver. from calculate.f90
!c  2011/07/15 Yamashita: add dshift, dmaskout
!c  2017/06/05 Yamashita: use common_typedef
!c
!c  integer nx; number of x-grid  ny;number of y-grid
!c          np; number of p-grid
!c
!c  const
!c      real*8 fact1, fact2, ofs1, ofs2; A*fact1+ofs2, B*fact2+ofs2
!c      real*8 undef; undefined value
!c=====================================================================c
module dcalculate
  use common_typedef, only: i4b, r8b
  implicit none
  private
  public :: dadd, dsub, dmlt, ddiv, dsquare, dspd, dabsval, dshift, dmaskout

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine:  dadd
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c      real*8 data2(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); (data1*fact1 + ofs1) + (data2*fact2 + ofs2)
!c=====
subroutine dadd(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
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
end subroutine dadd

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  dsub
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c      real*8 data2(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); (data1*fact1 + ofs1) - (data2*fact2 + ofs2)
!c=====
subroutine dsub(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
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
end subroutine dsub

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  dmlt
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c      real*8 data2(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); (data1*fact1 + ofs1) * (data2*fact2 + ofs2)
!c=====
subroutine dmlt(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
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
&                      * (data2(i,j,k) * fact2 + ofs2)
         else
           dout(i,j,k) = undef
         endif
       enddo !! i
     enddo !! j
   enddo !! k

  return
end subroutine dmlt

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: ddiv
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c      real*8 data2(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); (data1*fact1 + ofs1) / (data2*fact2 + ofs2)
!c=====
subroutine ddiv(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c data1 / data2
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.(data2(i,j,k) /= undef)           &
&         .and.data2(i,j,k) /= 0) then
          dout(i,j,k) = (data1(i,j,k) * fact1 + ofs1)                    &
&                     / (data2(i,j,k) * fact2 + ofs2)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine ddiv

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: dsquare
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); SQRT(data1*fact1 + ofs1)
!c=====
subroutine dsquare(nx, ny, np, undef, data1, dout, fact1, ofs1)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1           !!
  real(kind=r8b), intent(in)    :: ofs1            !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
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
end subroutine dsquare

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: dabsval
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); ABS(data1*fact1 + ofs1)
!c=====
subroutine dabsval(nx, ny, np, undef, data1, dout, fact1, ofs1)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1           !!
  real(kind=r8b), intent(in)    :: ofs1            !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
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
end subroutine dabsval

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: dspd
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c      real*8 data2(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); SQRT[(data1*fact1 + ofs1)**2 + (data2*fact2 + ofs2)**2]
!c=====
subroutine dspd(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: data2(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1, fact2    !!
  real(kind=r8b), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             ::  i, j, k        !!
!c
!c sqrt(data1**2 + data2**2)
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef) then
          dout(i,j,k) = SQRT((data1(i,j,k) * fact1 + ofs1)**2            &
&                     +      (data2(i,j,k) * fact2 + ofs2)**2 )
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine dspd

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: dshift
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); data1*fact1 + ofs1
!c=====
subroutine dshift(nx, ny, np, undef, data1, dout, fact1, ofs1)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: fact1           !!
  real(kind=r8b), intent(in)    :: ofs1            !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
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
end subroutine dshift

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine: dmaskout
!c  INPUT 
!c      real*8 data1(nx,ny,np)
!c      real*8 mask(nx,ny,np)
!c  OUTPUT 
!c      real*8 dout(nx,ny,np); maskout(data1,mask)
!c=====
subroutine dmaskout(nx, ny, np, undef, data1, mask, dout)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np      !!
  real(kind=r8b), intent(in)    :: undef           !!
  real(kind=r8b), intent(in)    :: data1(nx,ny,np) !!
  real(kind=r8b), intent(in)    :: mask(nx,ny,np)  !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nx,ny,np)  !!
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
end subroutine dmaskout

!c---------------------------------------------------------------------c

!c======================================================================c

end module dcalculate
