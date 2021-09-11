!c
!c  io.f90 
!c  [history]
!c  2007/03/23 Yamashita
!c  2008/02/27 add fwrite_r4_yrev, fwrite_r4_yzrev (Yamashita)
!c  2010/10/19 add fread_r4_to_r8, fwrite_r4_from_r8 (Yamashita)
!c  2011/01/01 bug fix of raxis (Yamashita)
!c  2017/05/31 Yamashita: use common_typedef
!c
!c=====================================================================c
module io
  use common_typedef, only: i4b, r4b, r8b
  use rw
  implicit none
  private
  public :: rdopen, wdopen, raxis, rgrads, wgrads
  public :: fwrite_r4_zrev, fwrite_r4_yrev, fwrite_r4_yzrev
  public :: fread_r4_to_r8, fwrite_r4_from_r8

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine rdopen
!c  INPUT 
!c    integer unit; unit number
!c            nlen ; record length [byte]
!c=====
subroutine rdopen(unit, nlen, fname)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit  !! unit number
  integer(kind=i4b), intent(in) :: nlen  !! length of record
  character(len=*), intent(in)  :: fname !! file name

  !c++ open
  call fopen(unit, fname, 'old', 0, nlen) 
 
  return
end subroutine rdopen

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wdopen
!c  INPUT 
!c    integer unit; unit number
!c            nlen ; record length [byte]
!c=====
subroutine wdopen(unit, nlen, fname)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit  !! unit number
  integer(kind=i4b), intent(in) :: nlen  !! length of record
  character(len=*), intent(in)  :: fname !! file name

  !c++ open
  call fopen(unit, fname, 'unknown', 0, nlen) 
 
  return
end subroutine wdopen

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgrads
!c  INPUT 
!c    integer unit; unit number
!c            nx, ny, np ; x, y, z grid length
!c            it; record number
!c    character fname
!c  OUTPUT 
!c    real*4 din
!c=====
subroutine rgrads(unit, nx, ny, np, it, fname, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit        !! unit number
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  character(len=*), intent(in)  :: fname       !! file name
  !c+++ [output]
  real(kind=r4b), intent(out)   :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: nlen        !! record length
  nlen = nx * ny * np * 4

  !c++ open
  call fopen(unit, fname, 'old', 0, nlen) 
  !c+++ write
  call fread_r4(nx, ny, np, it, unit, d)
  call fclose(unit)
 
  return
end subroutine rgrads

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wgrads
!c  INPUT 
!c    integer unit; unit number
!c            nx, ny, np ; x, y, z grid length
!c            it; record number
!c    character fname
!c    real*4 dout
!c=====
subroutine wgrads(unit, nx, ny, np, it, fname, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit        !! unit number
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  character(len=*), intent(in)  :: fname       !! file name
  real(kind=r4b), intent(in)    :: d(nx,ny,np) !!
  !c+++ work
  integer(kind=i4b)             :: nlen        !! record length
  nlen = nx * ny * np * 4

  !c++ open
  call fopen(unit, fname, 'unknown', 0, nlen) 
  !c+++ write
  call fwrite_r4(nx, ny, np, it, unit, d)
  call fclose(unit)

  return
end subroutine wgrads

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r4_zrev
!c  INPUT 
!c    integer unit; unit number
!c            nx, ny, np ; x, y, z grid length
!c            it; record number
!c    character fname
!c    real*4 dout 
!c=====
subroutine fwrite_r4_zrev(nx, ny, np, it, unit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit        !! unit number
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  real(kind=r4b), intent(in)    :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: nlen, k     !!
  real(kind=r4b)                :: w(nx,ny,np) !!
  nlen = nx * ny * np * 4

  !c++ reverse z-axis
  do k = 1, np
    w(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c+++ write
  call fwrite_r4(nx, ny, np, it, unit, w)

  return
end subroutine fwrite_r4_zrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r4_yrev
!c  INPUT 
!c    integer unit; unit number
!c            nx, ny, np ; x, y, z grid length
!c            it; record number
!c    character fname
!c    real*4 dout
!c=====
subroutine fwrite_r4_yrev(nx, ny, np, it, unit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit        !! unit number
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  real(kind=r4b), intent(in)    :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: nlen, j     !!
  real(kind=r4b)                :: w(nx,ny,np) !!
  nlen = nx * ny * np * 4

  !c++ reverse y-axis
  do j = 1, ny
    w(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c+++ write
  call fwrite_r4(nx, ny, np, it, unit, w)

  return
end subroutine fwrite_r4_yrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r4_yzrev
!c  INPUT 
!c    integer unit; unit number
!c            nx, ny, np ; x, y, z grid length
!c            it; record number
!c    character fname
!c    real*4 dout
!c=====
subroutine fwrite_r4_yzrev(nx, ny, np, it, unit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit         !! unit number
  integer(kind=i4b), intent(in) :: nx, ny, np   !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it           !! record number
  real(kind=r4b), intent(in)    :: d(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             :: nlen, j, k   !!
  real(kind=r4b)                :: w1(nx,ny,np) !!
  real(kind=r4b)                :: w2(nx,ny,np) !!
  nlen = nx * ny * np * 4

  !c++ reverse y-axis
  do j = 1, ny
    w1(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ reverse z-axis
  do k = 1, np
    w2(1:nx,1:ny,np-k+1) = w1(1:nx,1:ny,k)
  enddo !! k
  !c+++ write
  call fwrite_r4(nx, ny, np, it, unit, w2)

  return
end subroutine fwrite_r4_yzrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fread_r4_to_r8
!c  read real(kind=r4b) data and convert real(kind=r8b) output data
!c  INPUT 
!c    integer unit; unit number
!c            nx, ny, np ; x, y, z grid length
!c            it; record number
!c    character fname
!c    real(kind=r8b) d
!c  WORK
!c    real(kind=r4b) w
!c=====
subroutine fread_r4_to_r8(nx, ny, np, it, unit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit         !! unit number
  integer(kind=i4b), intent(in) :: nx, ny, np   !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it           !! record number
  !c+++ [output]
  real(kind=r8b), intent(out)   :: d(nx,ny,np)  !!
  !c+++ [internal work]
  integer(kind=i4b)             :: nlen         !!
  real(kind=r4b)                :: w(nx,ny,np)  !!
  nlen = nx * ny * np * 4

  !c+++ read
  call fread_r4(nx, ny, np, it, unit, w)
  !c+++ real(kind=r4b) ==> real(kind=r8b) 
  d(1:nx,1:ny,1:np) = w(1:nx,1:ny,1:np)

  return
end subroutine fread_r4_to_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r8_to_r4
!c  convert real(kind=r8b) input data to real(kind=r4b) value and write real(4) value
!c  INPUT 
!c    integer unit; unit number
!c            nx, ny, np ; x, y, z grid length
!c            it; record number
!c    character fname
!c    real(kind=r4b) d
!c  WORK
!c    real(kind=r8b) w
!c=====
subroutine fwrite_r4_from_r8(nx, ny, np, it, unit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: unit        !! unit number
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  real(kind=r8b), intent(in)    :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: nlen        !!
  real(kind=r4b)                :: w(nx,ny,np) !!
  nlen = nx * ny * np * 4

  !c+++ real(kind=r4b) ==> real(kind=r8b) 
  w(1:nx,1:ny,1:np) = d(1:nx,1:ny,1:np)
  !c+++ write
  call fwrite_r4(nx, ny, np, it, unit, w)

  return
end subroutine fwrite_r4_from_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine raxis
!c=====
subroutine raxis(nx, ny, np, XX, YY, P)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  !c+++ [output]
  real(kind=r8b), intent(out)   :: XX(nx)      !! x-levs.
  real(kind=r8b), intent(out)   :: YY(ny)      !! y-levs.
  real(kind=r8b), intent(out)   :: P(np)       !! z-levs.
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k     !!

  do i = 1, nx
    XX(i) = 360.d0 * (i - 1) / dble(nx)
  enddo !! i
  !c+++ open
  call fopen(10, 'P', 'old', 2, 0) !! P
  call fopen(11, 'Y', 'old', 2, 0) !! Y
!c
!c read
!c===
  do k = 1, np
    read(10, *, err=997) P(k)
!ccc if (MOD(k,10).eq.0) write(6,*) 'P=', P(k)
  enddo

  do j = 1, ny
    read(11, *, err=998) YY(j)
!ccc  if (MOD(j,10).eq.0) write(6,*) 'Y =',YY(j)
  enddo
  call fclose(10) !! P
  call fclose(11) !! Y

  return
  997 write(6,*) 'Error; DO NOT READ P...'
  998 write(6,*) 'Error; DO NOT READ Y...'
end subroutine raxis

!c---------------------------------------------------------------------c

!c======================================================================c

end module io
