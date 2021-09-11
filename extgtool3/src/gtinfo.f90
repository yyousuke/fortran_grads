!c
!c  program gtinfo
!c  [history]
!c  2013/10/14 Yamashita: first ver.
!c  2017/05/01 Yamashita: use common_typedef
!c  2017/05/01 Yamashita: modify getparms & I/O 
!c  
!c  read GTOOL3 file and display header information
!c
!c=====================================================================c
program gtinfo
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use util, only: get_axname
  use rw, only: fopen2, fclose
  use rwgtool, only: gtopen, gtclose, gtrewind, rgthd, seek_iounit
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd)
  character(len=ncc)   :: head(ndc) !! gtool3 header
  !c+++ input from gtool3 header
  character(len=ncc)   :: hitem     !! item
  character(len=ncc*2) :: htitl     !! title
  character(len=ncc)   :: hunit     !! unit
  character(len=ncc)   :: haxisx    !! x-axis name
  character(len=ncc)   :: haxisy    !! y-axis name
  character(len=ncc)   :: haxisz    !! z-axis name
  !c+++ from rgthd
  integer(kind=i4b)    :: imax      !! x-sizes
  integer(kind=i4b)    :: jmax      !! y-sizes
  integer(kind=i4b)    :: kmax      !! z-sizes
  real(kind=r8b)       :: rmiss     !! missing value
  !c+++ input from getparms
  character(len=nfiln) :: ifile     !! inputt file name
  character(len=nfiln) :: ofile     !! output file name
  !c+++ [work]
  integer(kind=i4b)    :: i         !!
  integer(kind=i4b)    :: ios       !! end code
  integer(kind=i4b)    :: jfile_i   !! I/O unit for input file
  integer(kind=i4b)    :: jfile_o   !! I/O unit for output file
  character(len=ncc)   :: anum      !!

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ open input gtool file
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ get I/O unit numbers for output file
  call seek_iounit(jfile_o, ios)
  if (ios /= 0) call ioerror(jfile_o, ios)
  !c+++ open output text file
  call fopen2(jfile_o, trim(ofile), 'unknown', 2, 0, ios)
  if (ios /= 0) call ioerror(jfile_o, ios)

  !c+++ read gtool header & set axis-sizes and missing value
  call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)
  call gtrewind(jfile_i, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ set axis-names
  call get_axname(haxisx, haxisy, haxisz, head)
  write(6, '(2a)') 'haxisx = ', haxisx
  write(6, '(2a)') 'haxisy = ', haxisy
  write(6, '(2a)') 'haxisz = ', haxisz
  !c+++ set unit
  hunit = head(16)
  !c+++ set item
  hitem = head(3)
  !c+++ set title
  htitl(1:16) = head(14)
  htitl(17:32) = head(15)
  write(6, '(2a)') 'unit = ', hunit
  write(6, '(2a)') 'item = ', hitem
  write(6, '(2a)') 'title = ', htitl

  !c+++ write
  write(jfile_o, '(2a)') 'file_name = ', trim(ifile)
  write(jfile_o, *) 'imax = ', imax
  write(jfile_o, *) 'jmax = ', jmax
  write(jfile_o, *) 'kmax = ', kmax
  write(jfile_o, *) 'rmiss = ', rmiss
  write(jfile_o, '(2a)') 'haxisx = ', haxisx
  write(jfile_o, '(2a)') 'haxisy = ', haxisy
  write(jfile_o, '(2a)') 'haxisz = ', haxisz
  write(jfile_o, '(2a)') 'unit = ', hunit
  write(jfile_o, '(2a)') 'item = ', hitem
  write(jfile_o, '(2a)') 'title = ', htitl

  do i = 1, 64
    write(anum, '(i2.2)') i
    write(jfile_o, '(2a)') 'head('//trim(anum)//') = ', head(i)
  enddo !! i

  !c+++ close files
  call gtclose(jfile_i, ios)
  call fclose(jfile_o)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c=====
subroutine getparms
  use uopts, only: read_parms, get_parms
  use char2var, only: c2var
  !c+++ [internal work]
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtinfo.txt', ofile, ios)

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtinfo -i input-file -o output-file'
  stop 2
end subroutine xabort

!c=====================================================================c

end program gtinfo
