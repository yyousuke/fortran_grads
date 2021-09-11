!c
!c  ysys_gfort.f90
!c  [history]
!c  2015/12/11 Yamashita: first ver.
!c  2017/05/29 Yamashita: use common_typedef
!c
!c  wrapper functions 
!c  
!c=====================================================================c
module ysys
  use common_typedef, only: i2b
  implicit none
  private
  public :: getdat, gettim

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine getdat
!c  OUTPUT integer(4) iyy, imm, idd
!c  wrapper function of getdat in ifort
!c=====
subroutine getdat(iyy, imm, idd)
  !c+++ [output]
  integer(kind=i2b), intent(out) :: iyy, imm, idd
  !c+++ [internal work]
  character(len=8)               :: hdate
  character(len=10)              :: htime

  call date_and_time(hdate, htime)
  read(hdate, '(i4, i2, i2)') iyy, imm, idd

  return
end subroutine getdat

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine gettim
!c  OUTPUT integer(4) ihh, imn, iss, ics
!c  wrapper function of getdat in ifort
!c=====
subroutine gettim(ihh, imn, iss, ics)
  !c+++ [output]
  integer(kind=i2b), intent(out) :: ihh, imn, iss, ics
  !c+++ [internal work]
  character(len=8)               :: hdate
  character(len=10)              :: htime

  call date_and_time(hdate, htime)
  read(htime, '(i2, i2, i2, 1x, i3)') ihh, imn, iss, ics

  return
end subroutine gettim

!c---------------------------------------------------------------------c

!c=====================================================================c

end module ysys
