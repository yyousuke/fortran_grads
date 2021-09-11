!c
!c  rw.f90
!c  [history]
!c  2005/09/27 Yamashita: fopen
!c  2005/10/03 Yamashita: add frw4
!c  2005/10/10 Yamashita: 標準コーディングルールに沿って変更 (Yamashita)
!c  2005/10/13 Yamashita: frw ==> fread_r4, fwrite_r4
!c  2005/10/13 Yamashita: add fread_r8, fwrite_r8. fclose
!c  2005/10/14 Yamashita: add fread_i2, fwrite_i4
!c  2005/10/27 Yamashita: edit verbose output
!c  2008/02/23 Yamashita: add fopen2
!c  2014/10/10 Yamashita: modify filename treatment
!c  2014/10/10 Yamashita: add fread_i8, fwrite_i8
!c  2017/04/18 Yamashita: add interfaces (fread, fwrite)
!c  2017/06/05 Yamashita: use common_typedef
!c
!c=====================================================================c
module rw
  use common_typedef, only: i2b, i4b, i8b, r4b, r8b
  implicit none
  private
  public :: fread_r4, fread_r8, fread_i2, fread_i4, fread_i8
  public :: fwrite_r4, fwrite_r8, fwrite_i2, fwrite_i4, fwrite_i8
  public :: fread, fwrite, fopen, fopen2, fclose

!c======================================================================c

!c---------------------------------------------------------------------c
!c  interface fread
!c  read data
!c=====
interface fread
  module procedure &
&   fread_r4, fread_r8, fread_i2, fread_i4, fread_i8
end interface fread

!c---------------------------------------------------------------------c
!c  interface fwrite
!c  write data
!c=====
interface fwrite
  module procedure &
&   fwrite_r4, fwrite_r8, fwrite_i2, fwrite_i4, fwrite_i8
end interface fwrite

!c======================================================================c

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine fread_r4
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for read
!c  OUTPUT real(kind=r4b) d(nx, ny, np)
!c=====
subroutine fread_r4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in)  :: it          !! record number
  integer(kind=i4b), intent(in)  :: iunit       !! unit number for open
  real(kind=r4b), intent(out)    :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)              :: ios         !! end code

  !c+++ read
  write(6,'(a,i6, 4(a,i4,1x))') 'READ: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  read(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Read Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fread_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fread_r8
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for read
!c  OUTPUT real(kind=r8b) d(nx, ny, np)
!c=====
subroutine fread_r8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in)  :: it          !! record number
  integer(kind=i4b), intent(in)  :: iunit       !! unit number for open
  real(kind=r8b), intent(out)    :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)              :: ios         !! end code

  !c+++ read
  write(6,'(a,i6, 4(a,i4,1x))') 'READ: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  read(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Read Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fread_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fread_i2
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c  OUTPUT integer(kind=i2b) d(nx, ny, np)
!c=====
subroutine fread_i2(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in)  :: it          !! record number
  integer(kind=i4b), intent(in)  :: iunit       !! unit number for open
  integer(kind=i2b), intent(out) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)              :: ios         !! end code

  !c+++ read
  write(6,'(a,i6, 4(a,i4,1x))') 'READ: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  read(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Read Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fread_i2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fread_i4
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c  OUTPUT integer(kind=i4b) d(nx, ny, np)
!c=====
subroutine fread_i4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in)  :: it          !! record number
  integer(kind=i4b), intent(in)  :: iunit       !! unit number for open
  integer(kind=i4b), intent(out) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)              :: ios         !! end code

  !c+++ read
  write(6,'(a,i6, 4(a,i4,1x))') 'READ: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  read(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Read Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fread_i4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fread_i8
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c  OUTPUT integer(kind=i8b) d(nx, ny, np)
!c=====
subroutine fread_i8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in)  :: it          !! record number
  integer(kind=i4b), intent(in)  :: iunit       !! unit number for open
  integer(kind=i8b), intent(out) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)              :: ios         !! end code

  !c+++ read
  write(6,'(a,i6, 4(a,i4,1x))') 'READ: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  read(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Read Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fread_i8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r4
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c         real(kind=r4b) d(nx, ny, np)
!c=====
subroutine fwrite_r4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  integer(kind=i4b), intent(in) :: iunit       !! unit number for open
  real(kind=r4b), intent(in)    :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: ios         !! end code

  !c+++ write
  write(6,'(a,i6, 4(a,i4,1x))') 'WRITE: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  write(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Write Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fwrite_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_r8
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c         real(kind=r8b) d(nx, ny, np)
!c=====
subroutine fwrite_r8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  integer(kind=i4b), intent(in) :: iunit       !! unit number for open
  real(kind=r8b), intent(in)    :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: ios         !! end code

  !c+++ write
  write(6,'(a,i6, 4(a,i4,1x))') 'WRITE: t =', it, ', size =', nx, 'x', ny, 'x', np,', unit =', iunit
  write(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Write Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fwrite_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_i2
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c         integer(kind=i2b) d(nx, ny, np)
!c=====
subroutine fwrite_i2(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  integer(kind=i4b), intent(in) :: iunit       !! unit number for open
  integer(kind=i2b), intent(in) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: ios         !! end code

  !c+++ write
  write(6,'(a,i6, 4(a,i4,1x))') 'WRITE: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  write(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Write Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fwrite_i2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_i4
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c         integer(kind=i4b) d(nx, ny, np)
!c=====
subroutine fwrite_i4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  integer(kind=i4b), intent(in) :: iunit       !! unit number for open
  integer(kind=i4b), intent(in) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: ios         !! end code

  !c+++ write
  write(6,'(a,i6, 4(a,i4,1x))') 'WRITE: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  write(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Write Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fwrite_i4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fwrite_i8
!c  INPUT  integer(kind=i4b) nx, ny, np, it
!c         integer(kind=i4b) iunit; unit number for write
!c         integer(kind=i8b) d(nx, ny, np)
!c=====
subroutine fwrite_i8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np  !! x-, y-, z-sizes
  integer(kind=i4b), intent(in) :: it          !! record number
  integer(kind=i4b), intent(in) :: iunit       !! unit number for open
  integer(kind=i8b), intent(in) :: d(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: ios         !! end code

  !c+++ write
  write(6,'(a,i6, 4(a,i4,1x))') 'WRITE: t =', it, ', size =', nx, 'x', ny, 'x', np, ', unit =', iunit
  write(iunit, rec=it, iostat=ios) d

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3)') 'File Write Error:  unit = ', iunit
    stop
  endif

  return
end subroutine fwrite_i8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fopen 
!c
!c  INPUT  integer(kind=i4b) iunit; unit number for open
!c         character  fname; file name
!c         character  fstatus; file status
!c                      old, new, unknown, append
!c
!c         character  ftype; file type
!c           ftype = 0: binary, unformatted, direct  (require 'nlen')
!c           ftype = 1: binary, unformatted, sequential (ignore 'nlen')
!c           ftype = 2: ascii, formatted, sequential  (ignore 'nlen')
!c
!c         integer(kind=i4b) nlen; record length for direct access
!c
!c=====
subroutine fopen(iunit, fname, fstatus, ftype, nlen)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: iunit   !! unit number for open
  character(len=*), intent(in)   :: fname   !! filename
  character(len=*), intent(in)   :: fstatus !! file status
  integer(kind=i4b), intent(in)  :: nlen    !! record length for direct access
  integer(kind=i4b), intent(in)  :: ftype   !! file type
  !c+++ [internal work]
  integer(kind=i4b)              :: ios     !! I/O status

  !c+++ open
  if (ftype == 0) then
    open(iunit, file=trim(fname), status=fstatus, iostat=ios, &
&     form='unformatted', access='direct', recl=nlen)
  else if (ftype == 1) then
    open(iunit, file=trim(fname), status=fstatus, iostat=ios, &
&     form='unformatted', access='sequential') 
  else if (ftype == 2) then
    open(iunit, file=trim(fname), status=fstatus, iostat=ios, &
&     form='formatted', access='sequential') 
  else
    write(6,*) 'Error: Irregal input, ftype...'
    stop
  endif

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3, 2a)') 'File Open Error:  unit = ', iunit, &
&     ', file name = ', fname
    stop
  else
    write(6,'(a,i2,2a)') 'Open File:  unit = ', iunit, ', &
&     file name = ', trim(fname)
  endif

  return 
end subroutine fopen

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fopen2  (return ierr)
!c
!c  INPUT  integer(kind=i4b) iunit; unit number for open
!c         character  fname; file name
!c         character  fstatus; file status
!c                      old, new, unknown, append
!c
!c         character  ftype;
!c           ftype = 0: binary, unformatted, direct  (require 'nlen')
!c           ftype = 1: binary, unformatted, sequential (ignore 'nlen')
!c           ftype = 2: ascii, formatted, sequential  (ignore 'nlen')
!c
!c         integer(kind=i4b) nlen; record length for direct access
!c
!c  OUTPUT integer(kind=i4b) ierr; error code
!c           ierr = 0: normal, ierr = 1: error 
!c
!c=====
subroutine fopen2(iunit, fname, fstatus, ftype, nlen, ierr)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: iunit   !! unit number for open
  character(len=*), intent(in)   :: fname   !! filename
  character(len=*), intent(in)   :: fstatus !! file status
  integer(kind=i4b), intent(in)  :: nlen    !! record length for direct access
  integer(kind=i4b), intent(in)  :: ftype   !! file type
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ierr    !! error code
  !c+++ [internal work]
  integer(kind=i4b)              :: ios     !! I/O status

  if (ftype == 0) then
    open(iunit, file=trim(fname), status=fstatus, iostat=ios, &
&     form='unformatted', access='direct', recl=nlen)
  else if (ftype == 1) then
    open(iunit, file=trim(fname), status=fstatus, iostat=ios, &
&     form='unformatted', access='sequential') 
  else if (ftype == 2) then
    open(iunit, file=trim(fname), status=fstatus, iostat=ios, &
&     form='formatted', access='sequential') 
  else
    write(6,*) 'Error: Irregal input, ftype...'
    stop
  endif

  !c+++ error message
  if (ios /= 0) then
    write(6,'(a, i3, 2a)') 'File Open Error:  unit = ', iunit, &
&     ', file name = ', trim(fname)
    ierr = 1
  else
    write(6,'(a,i2,2a)') 'Open File:  unit = ', iunit, ', &
&     file name = ', trim(fname)
    ierr = 0
  endif

  return
end subroutine fopen2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fclose
!c  INPUT  integer(kind=i4b) iunit; unit number for read
!c=====
subroutine fclose(iunit)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: iunit

  !c+++ close
  close(iunit)
  write(6,'(a,i2,2a)') 'Close File:  unit = ', iunit

  return
end subroutine fclose

!c---------------------------------------------------------------------c

!c=====================================================================c

end module rw
