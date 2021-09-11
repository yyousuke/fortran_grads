!c
!c  module error_handler
!c  [history]
!c  2017/04/28 Yamashita: first ver.: ioerror: error handler (2013/03/09 ver.)
!c  2017/05/02 Yamashita: use common_typedef
!c  2017/05/08 Yamashita: add werr, werr2 from util.f90
!c  2017/05/15 Yamashita: add werrc from util_epf.f90
!c
!c  public: werr: write error message
!c  public: werrc: write error message with error code
!c  public: werr2: write error message
!c  public: ioerror: Error handler
!c
!c=====================================================================c
module error_handler
  use common_typedef, only: i4b
  implicit none
  private
  public :: werr, werrc, werr2, ioerror

contains

!c======================================================================c

!c---------------------------------------------------------------------c
!c  subroutine werr
!c  write error message
!c=====
subroutine werr(char1, char2, msg)
  !c+++ [input]
  character(len=*), intent(in) :: char1
  character(len=*), intent(in) :: char2
  character(len=*), intent(in) :: msg !! message

  !c+++ write message
  write(6, '(2a)') 'Error: ', msg
  write(6, '(2a)') char1, char2
  stop 2
end subroutine werr

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine werrc
!c  write error message with error code
!c=====
subroutine werrc(ios, msg)
  !c+++ [input]
  integer(4), intent(in) :: ios !! end code
  character(len=*), intent(in) :: msg !! message

  !c+++ write message
  write(6, '(a)') msg
  write(6, '(a, i4)') 'stop due to error code = ', ios
  stop 2
end subroutine werrc

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine werr2
!c  write error message
!c=====
subroutine werr2(msg)
  !c+++ [input]
  character(len=*), intent(in) :: msg !! message

  !c+++ write message
  write(6, '(2a)') 'Error: ', msg
  stop 2
end subroutine werr2

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine ioerror
!c  handle I/O error
!c=====
subroutine ioerror(jfile, ios)
  integer(kind=i4b), intent(in) :: jfile !! unit number
  integer(kind=i4b), intent(in) :: ios   !! error code
  if (ios == -1) then
    write(6, *) 'Error: reach EOF, unit = ', jfile
  else if (ios == -2) then
    write(6, *) 'Error: data ended, unit = ', jfile
  else if (ios > 100.and.ios < 110) then
    write(6, *) 'Error occurred in file open, code = ', ios
  else if (ios > 110.and.ios < 120) then
    write(6, *) 'Error occurred in file read, code = ', ios
  else if (ios > 120.and.ios < 130) then
    write(6, *) 'Error occurred in file write, code = ', ios
  else if (ios > 130.and.ios < 140) then
    write(6, *) 'Error occurred in file seek, code = ', ios
  else if (ios == 141) then
    write(6, *) 'Error: data size overflow, code = ', ios
  else
    write(6, *) 'Error, unit = ', jfile, 'code = ', ios
  endif

  stop 2
end subroutine ioerror

!c---------------------------------------------------------------------c

!c======================================================================c

end module error_handler
