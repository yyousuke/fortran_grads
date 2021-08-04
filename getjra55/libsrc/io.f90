!c
!c  io.f90 
!c  [history]
!c  2007/03/23 Yamashita
!c  2008/02/27 Yamashita: add fwrite_r4_yrev, fwrite_r4_yzrev
!c  2010/10/19 Yamashita: add fread_r4_to_r8, fwrite_r4_from_r8
!c  2011/01/01 Yamashita: bug fix of raxis
!c  2014/10/10 Yamashita: add conv_r8_yrev, conv_r8_zrev
!c  2016/11/09 Yamashita: add shift_xgrids (renamed to shift_r8_xgrids)
!c  2017/02/09 Yamashita: add conv_r4_yrev, conv_r4_zrev, shift_r4_xgrids, shift_r8_xgrids
!c  2017/04/18 Yamashita: add conv_i2_yrev, conv_i2_zrev, conv_i4_yrev, conv_i4_zrev
!c  2017/04/18 Yamashita: add shift_r4_xgrids, shift_i2_xgrids, shift_i4_xgrids
!c  2017/04/18 Yamashita: add conv_i8_yrev, conv_i8_zrev, shift_i8_xgrids
!c  2017/04/18 Yamashita: add interfaces (shift_xgrids, conv_yrev, conv_zrev)
!c
!c=====================================================================c
module io
  use rw
  implicit none
  private
  public :: rdopen, wdopen, rgrads, wgrads
  public :: fwrite_r4_zrev, fwrite_r4_yrev, fwrite_r4_yzrev
  public :: fread_r4_to_r8, fwrite_r4_from_r8
  public :: raxis
  public :: shift_xgrids, conv_yrev, conv_zrev
  public :: shift_r4_xgrids, shift_r8_xgrids, shift_i2_xgrids, shift_i4_xgrids, shift_i8_xgrids
  public :: conv_r4_yrev, conv_r8_yrev, conv_i2_yrev, conv_i4_yrev, conv_i8_yrev
  public :: conv_r4_zrev, conv_r8_zrev, conv_i2_zrev, conv_i4_zrev, conv_i8_zrev

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  interface shift_xgrids
!c  shift x-grids
!c=====
interface shift_xgrids
  module procedure &
&   shift_r4_xgrids, shift_r8_xgrids, shift_i2_xgrids, shift_i4_xgrids, shift_i8_xgrids
end interface

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  interface conv_yrev
!c  reverse y-grids
!c=====
interface conv_yrev
  module procedure &
&   conv_r4_yrev, conv_r8_yrev, conv_i2_yrev, conv_i4_yrev, conv_i8_yrev
end interface

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  interface conv_zrev
!c  reverse z-grids
!c=====
interface conv_zrev
  module procedure &
&   conv_r4_zrev, conv_r8_zrev, conv_i2_zrev, conv_i4_zrev, conv_i8_zrev
end interface

!c----------------------------------------------------------------------c

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine rdopen
!c  INPUT  integer(4) iunit; unit number
!c         integer(4) nlen ; record length [byte]
!c=====
subroutine rdopen(iunit, nlen, fname)
  !c+++ [input]
  integer(4), intent(in) :: iunit, nlen
  character(len=*), intent(in) :: fname

  !c++ open
  call fopen(iunit, fname, 'old', 0, nlen) 
 
  return
end subroutine rdopen

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wdopen
!c  INPUT  integer(4) iunit; unit number
!c         integer(4) nlen ; record length [byte]
!c=====
subroutine wdopen(iunit, nlen, fname)
  !c+++ [input]
  integer(4), intent(in) :: iunit, nlen
  character(len=*), intent(in) :: fname

  !c++ open
  call fopen(iunit, fname, 'unknown', 0, nlen) 
 
  return
end subroutine wdopen

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgrads
!c  INPUT  integer(4) iunit; unit number
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c         integer(4) it; record number
!c         character  fname; file name
!c  OUTPUT 
!c         real(4)    d; data
!c=====
subroutine rgrads(iunit, nx, ny, np, it, fname, d)
  !c+++ [input]
  integer(4), intent(in)       :: iunit       !! unit number
  integer(4), intent(in)       :: nx, ny, np  !! number of x-, y-, z-grids
  integer(4), intent(in)       :: it          !! record number
  character(len=*), intent(in) :: fname       !! file name
  !c+++ [output]
  real(4), intent(out)         :: d(nx,ny,np) !! data
  !c+++ work
  integer(4) :: nlen
  nlen = nx * ny * np * 4

  !c++ open
  call fopen(iunit, fname, 'old', 0, nlen) 
  !c+++ write
  call fread_r4(nx, ny, np, it, iunit, d)
  !c+++ close
  call fclose(iunit)
 
  return
end subroutine rgrads

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wgrads
!c  INPUT  integer(4) iunit; unit number
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c         integer(4) it; record number
!c         character  fname; file name
!c         real(4)    d; data
!c=====
subroutine wgrads(iunit, nx, ny, np, it, fname, d)
  !c+++ [input]
  integer(4), intent(in)       :: iunit       !! unit number
  integer(4), intent(in)       :: nx, ny, np  !! number of x-, y-, z-grids
  integer(4), intent(in)       :: it          !! record number
  character(len=*), intent(in) :: fname       !! file name
  real(4), intent(in)          :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4) :: nlen
  nlen = nx * ny * np * 4

  !c++ open
  call fopen(iunit, fname, 'unknown', 0, nlen) 
  !c+++ write
  call fwrite_r4(nx, ny, np, it, iunit, d)
  !c+++ close
  call fclose(iunit)

  return
end subroutine wgrads

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r4_zrev
!c  INPUT  integer(4) nx, ny, np; number of x-, y-, z-grids
!c         integer(4) it; record number
!c         integer(4) iunit; unit number
!c         real(4)    d; data
!c=====
subroutine fwrite_r4_zrev(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np     !! number of x-, y-, z-grids
  integer(4), intent(in) :: it             !! record number
  integer(4), intent(in) :: iunit          !! unit number
  real(4), intent(in)    :: d(nx,ny,np)    !! data
  !c+++ [internal work]
  integer(4)             :: nlen, k
  real(4), allocatable   :: dwrite(:,:,:)
  nlen = nx * ny * np * 4

  allocate(dwrite(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    dwrite(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c+++ write
  call fwrite_r4(nx, ny, np, it, iunit, dwrite)
  deallocate(dwrite)

  return
end subroutine fwrite_r4_zrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r4_yrev
!c  INPUT  integer(4) nx, ny, np; number of x-, y-, z-grids
!c         integer(4) it; record number
!c         integer(4) iunit; unit number
!c         real(4)    dout; data
!c=====
subroutine fwrite_r4_yrev(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np     !! number of x-, y-, z-grids
  integer(4), intent(in) :: it             !! record number
  integer(4), intent(in) :: iunit          !! unit number
  real(4), intent(in)    :: d(nx,ny,np)    !! data
  !c+++ [internal work]
  integer(4)             :: nlen, j
  real(4), allocatable   :: dwrite(:,:,:)
  nlen = nx * ny * np * 4

  allocate(dwrite(nx,ny,np))
  !c++ reverse y-axis
  do j = 1, ny
    dwrite(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c+++ write
  call fwrite_r4(nx, ny, np, it, iunit, dwrite)
  deallocate(dwrite)

  return
end subroutine fwrite_r4_yrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r4_yzrev
!c
!c  INPUT  integer(4) nx, ny, np; number of x-, y-, z-grids
!c         integer(4) it; record number
!c         integer(4) iunit; unit number
!c         real(4)    d; data
!c=====
subroutine fwrite_r4_yzrev(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np     !! number of x-, y-, z-grids
  integer(4), intent(in) :: it             !! record number
  integer(4), intent(in) :: iunit          !! unit number
  real(4), intent(in)    :: d(nx,ny,np)    !! data
  !c+++ [internal work]
  integer(4)             :: nlen, j, k
  real(4), allocatable   :: dwrite(:,:,:)
  real(4), allocatable   :: work(:,:,:)
  nlen = nx * ny * np * 4

  allocate(work(nx,ny,np), dwrite(nx,ny,np))
  !c++ reverse y-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ reverse z-axis
  do k = 1, np
    dwrite(1:nx,1:ny,np-k+1) = work(1:nx,1:ny,k)
  enddo !! k
  !c+++ write
  call fwrite_r4(nx, ny, np, it, iunit, dwrite)
  deallocate(work, dwrite)

  return
end subroutine fwrite_r4_yzrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fread_r4_to_r8
!c  read real(4) data and convert real(8) output data
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c         integer(4) it; record number
!c         integer(4) iunit; unit number
!c  OUTPUT 
!c         real(8)    d; data
!c  WORK
!c         real(4) dread
!c=====
subroutine fread_r4_to_r8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np     !! number of x-, y-, z-grids
  integer(4), intent(in) :: it             !! record number
  integer(4), intent(in) :: iunit          !! unit number
  !c+++ [output]
  real(8), intent(out)   :: d(nx,ny,np)    !! data
  !c+++ [internal work]
  integer(4)             :: nlen
  real(4), allocatable   :: dread(:,:,:)
  nlen = nx * ny * np * 4

  allocate(dread(nx,ny,np))
  !c+++ read
  call fread_r4(nx, ny, np, it, iunit, dread)
  !c+++ real(4) ==> real(8) 
  d(1:nx,1:ny,1:np) = dread(1:nx,1:ny,1:np)
  deallocate(dread)

  return
end subroutine fread_r4_to_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r8_to_r4
!c  convert real(8) input data to real(4) value and write real(4) value
!c  INPUT 
!c         integer(4) iunit; unit number for read
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c         ingeger(4) it; record number
!c         character  fname; file name
!c         real(8)    d; data
!c  WORK
!c         real(4) dwrite
!c=====
subroutine fwrite_r4_from_r8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np     !! number of x-, y-, z-grids
  integer(4), intent(in) :: it             !! record number
  integer(4), intent(in) :: iunit          !! unit number
  real(8), intent(in)    :: d(nx,ny,np)    !! data
  !c+++ [internal work]
  integer(4)             :: nlen
  real(4), allocatable   :: dwrite(:,:,:)
  nlen = nx * ny * np * 4

  allocate(dwrite(nx,ny,np))
  !c+++ real(4) ==> real(8) 
  dwrite(1:nx,1:ny,1:np) = d(1:nx,1:ny,1:np)
  !c+++ write
  call fwrite_r4(nx, ny, np, it, iunit, dwrite)
  deallocate(dwrite)

  return
end subroutine fwrite_r4_from_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine raxis
!c  INPUT  integer(4) nx, ny, np; number of x-, y-, z-grids
!c  OUTPUT real(8)    XX, YY, ZZ; x-, y-, z-grid points
!c=====
subroutine raxis(nx, ny, np, XX, YY, P)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np
  !c+++ [output]
  real(8), intent(out)   :: XX(nx), YY(ny), P(np)
  !c+++ [internal work]
  integer(4) :: i, j, k
  integer(4) :: ios     !! I/O status

!c
!c x-axis
!c===
  do i = 1, nx
    XX(i) = 360.d0 * (i - 1) / DBLE(nx)
  enddo !! i
!c
!c z-axis
!c===
  !c+++ open
  call fopen(10, 'P', 'old', 2, 0) !! P
  !c+++ read
  do k = 1, np
    read(10, *, iostat=ios) P(k)
  enddo
  !c+++ close
  call fclose(10) !! P
  if (ios /= 0) then
    write(6,'(a)') 'File Read Error: filename = P'
    stop
  endif
!c
!c y-axis
!c===
  !c+++ open
  call fopen(11, 'Y', 'old', 2, 0) !! Y
  !c+++ read
  do j = 1, ny
    read(11, *, iostat=ios) YY(j)
  enddo
  !c+++ close
  call fclose(11) !! Y
  if (ios /= 0) then
    write(6,'(a)') 'File Read Error: filename = Y'
    stop
  endif

  return
end subroutine raxis

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r4_yrev
!c  reverse y-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         real(4)    d; data
!c=====
subroutine conv_r4_yrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  real(4), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)             :: j
  real(4), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r4_yrev

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r8_yrev
!c  reverse y-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         real(8)    d; data
!c=====
subroutine conv_r8_yrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  real(8), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)             :: j
  real(8), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r8_yrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_i2_yrev
!c  reverse y-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         integer(2)    d; data
!c=====
subroutine conv_i2_yrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  integer(2), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)                :: j
  integer(2), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_i2_yrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_i4_yrev
!c  reverse y-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         integer(4)    d; data
!c=====
subroutine conv_i4_yrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  integer(4), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)                :: j
  integer(4), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_i4_yrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_i8_yrev
!c  reverse y-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         integer(8)    d; data
!c=====
subroutine conv_i8_yrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  integer(8), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)                :: j
  integer(8), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do j = 1, ny
    work(1:nx,ny-j+1,1:np) = d(1:nx,j,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_i8_yrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r4_zrev
!c  reverse z-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         real(4)    d; data
!c=====
subroutine conv_r4_zrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  real(4), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)             :: k
  real(4), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    work(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r4_zrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_r8_zrev
!c  reverse z-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         real(8)    d; data
!c=====
subroutine conv_r8_zrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  real(8), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)             :: k
  real(8), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    work(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_r8_zrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_i2_zrev
!c  reverse z-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         integer(2)    d; data
!c=====
subroutine conv_i2_zrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  integer(2), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)                :: k
  integer(2), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    work(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_i2_zrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_i4_zrev
!c  reverse z-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         integer(4)    d; data
!c=====
subroutine conv_i4_zrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  integer(4), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)                :: k
  integer(4), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    work(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_i4_zrev

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine conv_i8_zrev
!c  reverse z-grids
!c  INPUT
!c         integer(4) nx, ny, np; number of x-, y-, z-grids
!c  MODIFY      
!c         integer(8)    d; data
!c=====
subroutine conv_i8_zrev(nx, ny, np, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np  !! number of x-, y-, z-grids
  !c+++ [modify]
  integer(8), intent(inout) :: d(nx,ny,np) !! data
  !c+++ [internal work]
  integer(4)                :: k
  integer(8), allocatable   :: work(:,:,:)

  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do k = 1, np
    work(1:nx,1:ny,np-k+1) = d(1:nx,1:ny,k)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine conv_i8_zrev

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine shift_r4_xgrids
!c  shift x-grids (real*4)
!c=====
subroutine shift_r4_xgrids(nx, ny, np, ix_shift, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np
  integer(4), intent(in) :: ix_shift
  !c+++ [modify]
  real(4), intent(inout) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4)             :: i
  real(4), allocatable   :: work(:,:,:)

  if (ix_shift < 0.or.ix_shift > nx) then
    write(6, *) 'Warn: invalid range of ix_shift, do nothing'
    write(6, *) 'ix_shift = ', ix_shift
    return
  endif
  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do i = 1, nx-ix_shift
    work(i+ix_shift,1:ny,1:np) = d(i,1:,1:np)
  enddo !! k
  do i = 1, ix_shift
    work(i,1:ny,1:np) = d(nx-ix_shift+i,1:,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine shift_r4_xgrids

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine shift_r8_xgrids
!c  shift x-grids (real*8)
!c=====
subroutine shift_r8_xgrids(nx, ny, np, ix_shift, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np
  integer(4), intent(in) :: ix_shift
  !c+++ [modify]
  real(8), intent(inout) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4)             :: i
  real(8), allocatable   :: work(:,:,:)

  if (ix_shift < 0.or.ix_shift > nx) then
    write(6, *) 'Warn: invalid range of ix_shift, do nothing'
    write(6, *) 'ix_shift = ', ix_shift
    return
  endif
  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do i = 1, nx-ix_shift
    work(i+ix_shift,1:ny,1:np) = d(i,1:,1:np)
  enddo !! k
  do i = 1, ix_shift
    work(i,1:ny,1:np) = d(nx-ix_shift+i,1:,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine shift_r8_xgrids

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine shift_i2_xgrids
!c  shift x-grids (integer*2)
!c=====
subroutine shift_i2_xgrids(nx, ny, np, ix_shift, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np
  integer(4), intent(in)    :: ix_shift
  !c+++ [modify]
  integer(2), intent(inout) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4)                :: i
  integer(2), allocatable   :: work(:,:,:)

  if (ix_shift < 0.or.ix_shift > nx) then
    write(6, *) 'Warn: invalid range of ix_shift, do nothing'
    write(6, *) 'ix_shift = ', ix_shift
    return
  endif
  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do i = 1, nx-ix_shift
    work(i+ix_shift,1:ny,1:np) = d(i,1:,1:np)
  enddo !! k
  do i = 1, ix_shift
    work(i,1:ny,1:np) = d(nx-ix_shift+i,1:,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine shift_i2_xgrids

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine shift_i4_xgrids
!c  shift x-grids (integer*4)
!c=====
subroutine shift_i4_xgrids(nx, ny, np, ix_shift, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np
  integer(4), intent(in)    :: ix_shift
  !c+++ [modify]
  integer(4), intent(inout) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4)                :: i
  integer(4), allocatable   :: work(:,:,:)

  if (ix_shift < 0.or.ix_shift > nx) then
    write(6, *) 'Warn: invalid range of ix_shift, do nothing'
    write(6, *) 'ix_shift = ', ix_shift
    return
  endif
  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do i = 1, nx-ix_shift
    work(i+ix_shift,1:ny,1:np) = d(i,1:,1:np)
  enddo !! k
  do i = 1, ix_shift
    work(i,1:ny,1:np) = d(nx-ix_shift+i,1:,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine shift_i4_xgrids

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine shift_i8_xgrids
!c  shift x-grids (integer*8)
!c=====
subroutine shift_i8_xgrids(nx, ny, np, ix_shift, d)
  !c+++ [input]
  integer(4), intent(in)    :: nx, ny, np
  integer(4), intent(in)    :: ix_shift
  !c+++ [modify]
  integer(8), intent(inout) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4)                :: i
  integer(8), allocatable   :: work(:,:,:)

  if (ix_shift < 0.or.ix_shift > nx) then
    write(6, *) 'Warn: invalid range of ix_shift, do nothing'
    write(6, *) 'ix_shift = ', ix_shift
    return
  endif
  allocate(work(nx,ny,np))
  !c++ reverse z-axis
  do i = 1, nx-ix_shift
    work(i+ix_shift,1:ny,1:np) = d(i,1:,1:np)
  enddo !! k
  do i = 1, ix_shift
    work(i,1:ny,1:np) = d(nx-ix_shift+i,1:,1:np)
  enddo !! k
  !c++ modify
  d(1:nx,1:ny,1:np) = work(1:nx,1:ny,1:np)
  deallocate(work)

  return
end subroutine shift_i8_xgrids

!c---------------------------------------------------------------------c

!c=====================================================================c

end module io
