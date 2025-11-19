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
!c
!c=====================================================================c
module rw
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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for read
!c  OUTPUT real(4) d(nx, ny, np)
!c=====
subroutine fread_r4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  real(4), intent(out) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for read
!c  OUTPUT real(8) d(nx, ny, np)
!c=====
subroutine fread_r8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  real(8), intent(out) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c  OUTPUT integer(2) d(nx, ny, np)
!c=====
subroutine fread_i2(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  integer(2), intent(out) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c  OUTPUT integer(4) d(nx, ny, np)
!c=====
subroutine fread_i4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  integer(4), intent(out) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c  OUTPUT integer(8) d(nx, ny, np)
!c=====
subroutine fread_i8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  integer(8), intent(out) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c         real(4) d(nx, ny, np)
!c=====
subroutine fwrite_r4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  real(4), intent(in) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c         real(8) d(nx, ny, np)
!c=====
subroutine fwrite_r8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  real(8), intent(in) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c         integer(4) d(nx, ny, np)
!c=====
subroutine fwrite_i2(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  integer(2), intent(in) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c         integer(4) d(nx, ny, np)
!c=====
subroutine fwrite_i4(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  integer(4), intent(in) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) nx, ny, np, it
!c         integer(4) iunit; unit number for write
!c         integer(4) d(nx, ny, np)
!c=====
subroutine fwrite_i8(nx, ny, np, it, iunit, d)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np, it, iunit
  integer(8), intent(in) :: d(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: ios

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
!c  INPUT  integer(4) iunit; unit number for open
!c         character  fname; file name
!c         character  fstatus; file status
!c                      old, new, unknown, append
!c
!c         character  ftype; file type
!c           ftype = 0: binary, unformatted, direct  (require 'nlen')
!c           ftype = 1: binary, unformatted, sequential (ignore 'nlen')
!c           ftype = 2: ascii, formatted, sequential  (ignore 'nlen')
!c
!c         integer(4) nlen; record length for direct access
!c
!c=====
subroutine fopen(iunit, fname, fstatus, ftype, nlen)
  !c+++ [input]
  integer(4), intent(in)       :: iunit   !! unit number for open
  character(len=*), intent(in) :: fname   !! filename
  character(len=*), intent(in) :: fstatus !! file status
  integer(4), intent(in)       :: nlen    !! record length for direct access
  integer(4), intent(in)       :: ftype   !! file type
  !c+++ [internal work]
  integer(4)                   :: ios     !! I/O status

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
!c  INPUT  integer(4) iunit; unit number for open
!c         character  fname; file name
!c         character  fstatus; file status
!c                      old, new, unknown, append
!c
!c         character  ftype;
!c           ftype = 0: binary, unformatted, direct  (require 'nlen')
!c           ftype = 1: binary, unformatted, sequential (ignore 'nlen')
!c           ftype = 2: ascii, formatted, sequential  (ignore 'nlen')
!c
!c         integer(4) nlen; record length for direct access
!c
!c  OUTPUT integer(4) ierr; error code
!c           ierr = 0: normal, ierr = 1: error 
!c
!c=====
subroutine fopen2(iunit, fname, fstatus, ftype, nlen, ierr)
  !c+++ [input]
  integer(4), intent(in)       :: iunit   !! unit number for open
  character(len=*), intent(in) :: fname   !! filename
  character(len=*), intent(in) :: fstatus !! file status
  integer(4), intent(in)       :: nlen    !! record length for direct access
  integer(4), intent(in)       :: ftype   !! file type
  !c+++ [output]
  integer(4), intent(out)      :: ierr    !! error code
  !c+++ [internal work]
  integer(4)                   :: ios     !! I/O status

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
!c  INPUT  integer(4) iunit; unit number for read
!c=====
subroutine fclose(iunit)
  !c+++ [input]
  integer(4), intent(in) :: iunit

  !c+++ close
  close(iunit)
  write(6,'(a,i2,2a)') 'Close File:  unit = ', iunit

  return
end subroutine fclose

!c---------------------------------------------------------------------c

!c=====================================================================c

end module rw
