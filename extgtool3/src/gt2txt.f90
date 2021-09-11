!c
!c  program gt2txt
!c  [history]
!c  2015/01/20 Yamashita: first ver. (from 2013/10/14 gtinfo)
!c  2015/08/14 Yamashita: rtime is fixed
!c  2017/05/13 Yamashita: modify getparms & I/O 
!c  2017/05/13 Yamashita: use common_typedef
!c  
!c  read GTOOL3 file and write text file
!c  only for 1-dimension data
!c
!c=====================================================================c
program gt2txt
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use util, only: get_time
  use ucaln, only: dayloop
  use rw, only: fopen2, fclose
  use rwgtool, only: gtopen, gtclose, gtrewind, rgthd, rgt, seek_iounit, wmsg
  use error_handler, only: ioerror, werr2
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  !c+++ (ncc: length of gtool3 header, nfiln: length of file name)
  character(len=nfiln)        :: ifile     !! input file name
  character(len=nfiln)        :: ofile     !! output file name
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc) !! gtool3 header
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax      !! x-size
  integer(kind=i4b)           :: jmax      !! y-size
  integer(kind=i4b)           :: kmax      !! z-size
  real(kind=r8b)              :: rmiss     !! missing value
  character(len=ncc)          :: hitem     !! item of output data
  character(len=ncc*2)        :: htitl     !! title of output data
  character(len=ncc)          :: hunit     !! unit of output data
  !c+++ [input/output]
  real(kind=r8b), allocatable :: d(:,:,:)  !! data
  !c+++ [output]
  real(kind=r4b)              :: rtime     !! time (year)
  !c+++ [work]
  integer(kind=i4b)           :: it
  integer(kind=i4b)           :: iday      !! days of year
  integer(kind=i4b)           :: nday      !! number of days per year
  integer(kind=i4b)           :: ios       !! end code
  integer(kind=i4b)           :: jfile_i   !! I/O unit for input file
  integer(kind=i4b)           :: jfile_o   !! I/O unit for output file
  integer(kind=i4b)           :: jdate(6)  !! time array

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
  !c+++ check
  if (imax /= 1.or.jmax /= 1.or.kmax /= 1) then
    write(6, '(a)') 'input-file must be 1-dimension data'
    call werr2('in boundary check')
  endif

  !c+++ set unit
  hunit = head(16)
  !c+++ set item
  hitem = head(3)
  !c+++ set title
  htitl(1:16) = head(14)
  htitl(17:32) = head(15)
  write(6, '(2a)') 'title = ', htitl
  write(6, '(2a)') 'item = ', hitem
  write(6, '(2a)') 'unit = ', hunit
  !c+++ allocate
  allocate(d(imax,jmax,kmax))

!c
!c main
!c===
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ read gtool3 header & data
    call rgt(jfile_i, imax, jmax, kmax, head, d, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ read time
    call get_time(head, jdate, ios)
    if (ios /= 0) call ioerror(-1, ios)
    call dayloop(jdate(1), 1, 1, jdate(1), jdate(2), jdate(3), iday)
    call dayloop(jdate(1), 1, 1, jdate(1),       12,       31, nday)
    !ccc rtime = jdate(1) + iday / dble(nday)
    rtime = jdate(1) + (iday - 1) / dble(nday) &
&         + jdate(4) / dble(nday * 24) &
&         + jdate(5) / dble(nday * 24 * 60)

    !c+++ write
    call wmsg(head, 'WRITE')
    write(jfile_o, '(a, 1x, f9.4, 1x, e15.7)') head(27), rtime, d(1,1,1)
  enddo !! it

!c
!c final
!c===
  !c+++ deallocate
  deallocate(d)
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
  !c+++ [internal work]
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtool.txt', ofile, ios)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine xabort
!c=====
subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gt2txt -i input-file -o output-file'
  write(6, '(a)') '(input-file: 1-dimension data)'
  stop 2
end subroutine xabort

!c=====================================================================c

end program gt2txt
