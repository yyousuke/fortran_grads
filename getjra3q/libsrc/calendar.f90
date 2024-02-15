!c
!c  module calendar
!c  [history] 
!c  2003/08/14 Yamashita: first ver. (dayloop)
!c  2005/10/15 Yamashita: f77 ==> f90
!c  2005/10/25 Yamashita: add eday
!c  2011/10/06 Yamashita: add tinfo
!c  2012/03/28 Yamashita: modify treatment of nrange=0 in endday
!c  2017/01/11 Yamashita: bug fix of dayloop (nsyy == neyy, nemm < nsmm case )
!c  2017/08/02 Yamashita: add tinfor
!c
!c  start time;  nsyy; year, nsmm; month, nsdd; day
!c  end time;  neyy; year, nemm; month, nedd; day
!c======================================================================c
module calendar
  implicit none
  private
  public :: dayloop, dayloopw, endday, nday, eday, tinfo, tinfor

  !c+++ [common]
  integer(4), parameter :: nmm = 12

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine dayloop
!c  OUTPUT irange; number of days
!c=====
subroutine dayloop(nsyy, nsmm, nsdd, neyy, nemm, nedd, irange)
  !c+++ [input]
  integer(4), intent(in) :: nsyy, nsmm, nsdd, neyy, nemm, nedd
  !c+++ [output]
  integer(4), intent(out) :: irange
  !c+++ [internal work]
  integer(4) :: iyy, imm, idd, iflag, ndd
  irange = 0
  do iyy = nsyy, neyy
    iflag = 0
    if ( (MOD(iyy,4) == 0) .and. (MOD(iyy,100) /= 0)                    &
&     .or. (MOD(iyy,400) == 0) ) iflag = 1
    if ( iyy == nsyy .and. iyy == neyy .and. nemm < nsmm ) cycle
    do imm = 1, nmm
      if ( iyy == nsyy .and. imm < nsmm ) cycle
      if (imm == 2) then
        ndd = 28 + iflag
      elseif ( (MOD(imm,2) == 1 .and. imm <= 7)                         &
&       .or. (MOD(imm,2) == 0 .and. imm >= 8) ) then
        ndd = 31
      else
        ndd = 30
      endif
      do idd = 1, ndd
        if ( (iyy == nsyy) .and. (imm == nsmm) .and. (idd < nsdd) ) cycle
        irange = irange + 1
        if ( (iyy == neyy) .and. (imm == nemm) .and. (idd == nedd) ) return
      enddo !! idd
    enddo !! imm
  enddo !! iyy
  
  return
end subroutine dayloop

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine dayloopw
!c  write year, month, day to standard output
!c=====
subroutine dayloopw(nsyy, nsmm, nsdd, neyy, nemm, nedd)
  !c+++ [input]
  integer(4), intent(in) :: nsyy, nsmm, nsdd, neyy, nemm, nedd
  !c+++ [internal work]
  integer(4) :: iyy, imm, idd, iflag, ndd
  do iyy = nsyy, neyy
    iflag = 0
    if ( (MOD(iyy,4) == 0) .and. (MOD(iyy,100) /= 0)                    &
&     .or. (MOD(iyy,400) == 0) ) iflag = 1
    do imm = 1, nmm
      if ( iyy == nsyy .and. imm < nsmm ) cycle
      if (imm == 2) then
        ndd = 28 + iflag
      elseif ( (MOD(imm,2) == 1 .and. imm <= 7)                         &
&       .or. (MOD(imm,2) == 0 .and. imm >= 8) ) then
        ndd = 31
      else
        ndd = 30
      endif
      do idd = 1, ndd
        if ( (iyy == nsyy) .and. (imm == nsmm) .and. (idd < nsdd) ) cycle
        write(6,*) iyy, imm, idd        ! edit hear
        if ( (iyy == neyy) .and. (imm == nemm) .and. (idd == nedd) ) return
      enddo !! idd
    enddo !! imm
  enddo !! iyy

  return
end subroutine dayloopw

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine endday
!c   INPUT nsyy, nsmm, nsdd, nrange
!c  OUTPUT neyy, nemm, nedd ; nsyy/nsmm/nsdd + nrange (days)
!c=====
subroutine endday(nsyy, nsmm, nsdd, neyy, nemm, nedd, nrange)
  !c+++ [input]
  integer(4), intent(in) :: nsyy, nsmm, nsdd, nrange
  !c+++ [output]
  integer(4), intent(out) :: neyy, nemm, nedd
  !c+++ [internal work]
  integer(4) :: iyy, imm, idd, iflag, irange, ndd
  if (nrange < 0) then
    write(6,*) 'Error: invalid input nrange'
    stop
  else if (nrange == 0) then
    neyy = nsyy
    nemm = nsmm
    nedd = nsdd
    return
  endif
  irange = 0
  do iyy = nsyy, 9999
    iflag = 0
    if ( (MOD(iyy,4) == 0) .and. (MOD(iyy,100) /= 0)                    &
&     .or. (MOD(iyy,400) == 0) ) iflag = 1
    do imm = 1, nmm
      if ( iyy == nsyy .and. imm < nsmm ) cycle
      if (imm == 2) then
        ndd = 28 + iflag
      elseif ( (MOD(imm,2) == 1 .and. imm <= 7)                         &
&       .or. (MOD(imm,2) == 0 .and. imm >= 8) ) then
        ndd = 31
      else
        ndd = 30
      endif
      do idd = 1, ndd
        if ( (iyy == nsyy) .and. (imm == nsmm) .and. (idd <= nsdd) ) cycle
        irange = irange + 1
        if (irange == nrange) then
          neyy = iyy
          nemm = imm
          nedd = idd
          return
        endif
      enddo !! idd
    enddo !! imm
  enddo !! iyy
  
  return
end subroutine endday

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine nday
!c  calculate a start day ('nsta') and a end day ('nend') of month
!c  until begining of the year
!c  [history]
!c    2003/8/14  Yamashita
!c    2005/10/15 f77 ==> f90 (Yamashita)
!c
!c INPUT; iyy, imon
!c OUTPUT; nsta, nend;  start/end day of month
!c=====
subroutine nday(iyy, imon, nsta, nend)
  !c+++ [input]
  integer(4), intent(in) :: iyy, imon
  !c+++ [output]
  integer(4), intent(out) :: nsta, nend
  !c+++ [internal work]
  integer(4) :: imm, ndd, iflag
  nsta = 1
  nend = 0
  iflag = 0
  if ( (MOD(iyy,4) == 0) .and. (MOD(iyy,100) /= 0)                      &
&   .or. (MOD(iyy,400) == 0)) iflag = 1
  do imm = 1, nmm
    if (imm == 2) then
      ndd = 28 + iflag
    elseif ( (MOD(imm,2) == 1 .and. imm <= 7)                           &
&     .or. (MOD(imm,2) == 0 .and. imm >= 8) ) then
      ndd = 31
    else
      ndd = 30
    endif
    nend = nsta + ndd - 1
    if (imm == imon) return
    nsta = nsta + ndd
  enddo !! imm
  
  return
end subroutine nday

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine edaym
!c  calculate a end day ('ndd') of the month
!c  [history]
!c    2003/8/14  Yamashita
!c    2005/10/15 f77 ==> f90 (Yamashita)
!c
!c INPUT; iyy, imm
!c OUTPUT; ndd;  end day of month
!c=====
subroutine eday(iyy, imm, ndd)
  !c+++ [input]
  integer(4), intent(in) :: iyy, imm
  !c+++ [output]
  integer(4), intent(out) :: ndd
  !c+++ [internal work]
  integer(4) :: iflag
  iflag = 0
  if ( (MOD(iyy,4) == 0) .and. (MOD(iyy,100) /= 0)                      &
&   .or. (MOD(iyy,400) == 0)) iflag = 1
  if (imm == 2) then
    ndd = 28 + iflag
  elseif ( (MOD(imm,2) == 1 .and. imm <= 7)                             &
&     .or. (MOD(imm,2) == 0 .and. imm >= 8) ) then
    ndd = 31
  else
    ndd = 30
  endif

  return
end subroutine eday

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine tinfo
!c  convert to '00Z01JAN2011' format
!c  [history]
!c  2011/10/06 Yamashita
!c=====
function tinfo(iyy, imm, idd, it)
  !c+++ [function]
  character(30) :: tinfo
  !c+++ [input]
  integer(4), intent(in) :: iyy, imm, idd, it
  !c+++ [internal work]
  character(10) :: arr(4) = (/'00Z', '06Z', '12Z', '18Z'/)
  character(10) :: amon(nmm) = (/'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/)
  write(tinfo, '(a, i2.2, a, i4.4)') trim(arr(it)), idd, trim(amon(imm)), iyy
end function tinfo

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine tinfor
!c  convert to '00Z01JAN2011' format
!c  [history]
!c  2017/08/02 Yamashita
!c=====
subroutine tinfor(tinfo, iyy, imm, idd, it)
  !c+++ [input]
  character(30), intent(in) :: tinfo
  !c+++ [output]
  integer(4), intent(out) :: iyy, imm, idd, it
  !c+++ [internal work]
  integer(4) :: i
  character(10) :: arrin, amonin
  character(10) :: arr(4) = (/'00Z', '06Z', '12Z', '18Z'/)
  character(10) :: amon(nmm) = (/'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/)

  !ccc read(tinfo, '(a, i2.2, a, i4.4)') arrin, idd, amonin, iyy
  read(tinfo(1:3), '(a)') arrin
  read(tinfo(4:5), '(i2.2)') idd
  read(tinfo(6:8), '(a)') amonin
  read(tinfo(9:12), '(i4.4)') iyy
  !c+++ rec
  it = -1
  do i = 1, 4
    if (arrin == arr(i) ) it = i
  enddo
  !c+++ month
  imm = -1
  do i = 1, nmm
    if (amonin == amon(i) ) imm = i
  enddo

  return
end subroutine tinfor

!c----------------------------------------------------------------------c

!c=====================================================================c

end module calendar
