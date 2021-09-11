!c  module ucaln
!c   [history]
!c  2013/03/10 Yamashita: tcent from util.f90
!c  2013/03/10 Yamashita: eday, endday, dayloop from calendar.f90
!c  2013/11/27 Yamashita: add tnext from util_conv.f90
!c
!c  public:
!c  subroutine tnext: calculate next time with tdur, htunit
!c  subroutine tcent: calculate center time 
!c  subroutine datetime2sec: output time (s)
!c  subroutine date2day: convert iyy/imm/idd to iday (from 0000/1/1)
!c  subroutine eday: calculate a end day ('ndd') of the month
!c  subroutine dayloop: output irange (number of days)
!c  internal:
!c  subroutine day2date: convert iday (from 0000/1/1) to iyy/imm/idd
!c  subroutine time2sec: convert isec to ihh:imn:iss
!c  subroutine sec2time: convert ihh:imn:iss to isec
!c  subroutine endday: calculate end day with nrange (number of days)
!c======================================================================c
module ucaln
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: tnext, tcent, datetime2sec, eday, date2day, dayloop

  !c+++ [common]
  integer(kind=i4b), parameter :: nmm = 12

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine tnext
!c  calculate next time with tdur, htunit
!c  output: iyy/imm/idd ihh:imn:iss
!c  [history]
!c  2013/10/29 Yamashita
!c=====
subroutine tnext(nsyy, nsmm, nsdd, nshh, nsmn, nsss, &
&  iyy, imm, idd, ihh, imn, iss, &
&  tdur, htunit)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nsyy, nsmm, nsdd    !! start date
  integer(kind=i4b), intent(in)  :: nshh, nsmn, nsss    !! start time
  real(kind=r8b), intent(in)     :: tdur                !! time_interval
  character(len=*), intent(in)   :: htunit              !! unit of time
  !c+++ [output]
  integer(kind=i4b), intent(out) :: iyy, imm, idd       !! date
  integer(kind=i4b), intent(out) :: ihh, imn, iss       !! time
  !c+++ [internal work]
  integer(kind=i4b)              :: jdays, jsecs, jseca !!
  integer(kind=i4b)              :: jdaye, jsece        !!

  !c+++ set end day (tentative)
  iyy = nsyy
  imm = nsmm
  idd = nsdd
  ihh = nshh
  imn = nsmn
  iss = nsss
  !c+++ start day ==> jdays
  call date2day(nsyy, nsmm, nsdd, jdays)
  !c+++ start time (sec)
  call time2sec(nshh, nsmn, nsss, jsecs)
  jsece = jsecs

  if (trim(htunit) == 'HOUR') then
    !c+++ calc. jdaye and jsece
    jseca = jsecs + int(tdur * 3600.d0)
    jdaye = int(real(jdays) + dble(jseca) / 86400.d0)
    jsece = mod(jseca, 86400)
  else if (trim(htunit) == 'MIN') then
    !c+++ calc. jdaye and jsece
    jseca = jsecs + int(tdur * 60.d0)
    jdaye = int(real(jdays) + dble(jseca) / 86400.d0)
    jsece = mod(jseca, 86400)
  else if (trim(htunit) == 'SEC') then
    !c+++ calc. jdaye and jsece
    jseca = jsecs + int(tdur)
    jdaye = int(real(jdays) + dble(jseca) / 86400.d0)
    jsece = mod(jseca, 86400)
  else if (trim(htunit) == 'DAY') then
    !c+++ calc. jdaye
    jdaye = jdays + int(tdur)
  else if (trim(htunit) == 'MON') then
    !c+++ set end day
    iyy = nsyy + int((dble(imm) + tdur - 1.d0) / 12.d0)
    imm = mod(nsmm + int(tdur), 12)
    if (imm == 0) imm = 12
    idd = nsdd
    return
  else if (trim(htunit) == 'YEAR') then
    !c+++ set end day
    iyy = nsyy + int(tdur)
    imm = nsmm
    idd = nsdd
    return
  else
    write(6, '(2a)') 'Warn: invalid htunit = ', trim(htunit)
    return
  endif

  !c+++ end time (day) ==> end time yy/mm/dd
  call day2date(jdaye-1, iyy, imm, idd)
  !c+++ end time (sec) ==> end time hh:mn:ss
  call sec2time(jsece, ihh, imn, iss)

  return
end subroutine tnext

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine tcent
!c  calculate center time between nsyy/nsmm/nsdd nshh:nsmn:nsss
!c                             to neyy/nemm/nedd nehh:nemn:ness
!c  output: iyy/imm/idd ihh:imn:iss
!c  [history]
!c  2013/01/08 Yamashita
!c=====
subroutine tcent(nsyy, nsmm, nsdd, nshh, nsmn, nsss, neyy, nemm, nedd, nehh, nemn, ness, iyy, imm, idd, ihh, imn, iss)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nsyy, nsmm, nsdd    !! start date
  integer(kind=i4b), intent(in)  :: nshh, nsmn, nsss    !! start time
  integer(kind=i4b), intent(in)  :: neyy, nemm, nedd    !! end date
  integer(kind=i4b), intent(in)  :: nehh, nemn, ness    !! end time
  !c+++ [output]
  integer(kind=i4b), intent(out) :: iyy, imm, idd       !! date
  integer(kind=i4b), intent(out) :: ihh, imn, iss       !! time
  !c+++ [internal work]
  integer(kind=i4b)              :: jdays, jdaye, jdayc !!
  integer(kind=i4b)              :: jsecs, jsece, jsecc !!
  integer(kind=i4b)              :: jhhc                !!
  real(kind=r4b)                 :: rdayc               !!
  ihh = 0
  imn = 0
  iss = 0
  !c+++ start day
  call date2day(nsyy, nsmm, nsdd, jdays)
  !c+++ end day
  call date2day(neyy, nemm, nedd, jdaye)
  !c+++ center day
  rdayc = real(jdays + jdaye) / 2.d0
  jdayc = int(rdayc)
  jhhc = (rdayc - jdayc) * 24

  !c+++ start time (sec)
  call time2sec(nshh, nsmn, nsss, jsecs)
  !c+++ end time (sec)
  call time2sec(nehh, nemn, ness, jsece)
  !c+++ center time (sec)
  if (jhhc == 0) then
    jsecc = (jsecs + jsece) / 2
  else
    jsecc = (jsecs + jsece + 86400) / 2
  endif

  !c+++ center time (day) ==> center time yy/mm/dd
  call day2date(jdayc-1, iyy, imm, idd)
  !c+++ center time (sec) ==> center time hh:mn:ss
  call sec2time(jsecc, ihh, imn, iss)

  return
end subroutine tcent

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine datetime2sec
!c
!c  input: iyy/imm/idd ihh:imn:iss
!c  output: time
!c  [history]
!c  2016/11/17 Yamashita
!c=====
subroutine datetime2sec(iyy, imm, idd, ihh, imn, iss, time)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: iyy, imm, idd, ihh, imn, iss !!  date/time
  !c+++ [output]
  real(kind=r4b), intent(out)   :: time                         !! (sec)
  !c+++ [internal work]
  integer(kind=i4b)             :: jday, jsec                   !!

  !c+++ date ==> day
  call date2day(iyy, imm, idd, jday)
  !c+++ time ==> sec
  call time2sec(ihh, imn, iss, jsec)

  !c+++ datetime
  time = real(jday) * 86400.d0 + real(jsec)

  return
end subroutine datetime2sec

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine date2day
!c  convert iyy/imm/idd to iday (from 0000/1/1)
!c
!c=====
subroutine date2day(iyy, imm, idd, iday)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: iyy, imm, idd !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: iday          !!
  !c+++ yy/mm/dd ==> day
  call dayloop(0, 1, 1, iyy, imm, idd, iday)
  return
end subroutine date2day

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine day2date
!c  convert iday (from 0000/1/1) to iyy/imm/idd
!c
!c=====
subroutine day2date(iday, iyy, imm, idd)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: iday          !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: iyy, imm, idd !!
  !c+++ day ==> yy/mm/dd
  call endday(0, 1, 1, iyy, imm, idd, iday)
  return
end subroutine day2date

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine time2sec
!c  convert isec to ihh:imn:iss
!c
!c=====
subroutine time2sec(ihh, imn, iss, isec)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: ihh, imn, iss !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: isec          !!
  !c+++ day hh:mn:ss ==> sec
  isec = (ihh * 3600) + (imn * 60) + iss
  return
end subroutine time2sec

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine sec2time
!c  convert ihh:imn:iss to isec
!c
!c=====
subroutine sec2time(isec, ihh, imn, iss)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: isec          !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ihh, imn, iss !!
  !c+++ [internal work]
  integer(kind=i4b)              :: iwork         !!
  if (isec >= 86400.or.isec < 0) then
    write(6, *) 'Error: invalid isec (0 <= isec < 86400)'
    stop
  endif

  iwork = isec
  ihh = 0
  imn = 0
  !c+++ sec ==> hh:mn:ss
  do while (iwork >= 3600)
    ihh = ihh + 1
    iwork = iwork - 3600
  enddo !! hour
  do while (iwork >= 60)
    imn = imn + 1
    iwork = iwork - 60
  enddo !! minute
  iss = iwork

  return
end subroutine sec2time

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine eday
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
  integer(kind=i4b), intent(in)  :: iyy, imm !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ndd      !!
  !c+++ [internal work]
  integer(kind=i4b)              :: iflag    !!
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
!c  subroutine endday
!c   INPUT nsyy, nsmm, nsdd, nrange
!c  OUTPUT neyy, nemm, nedd ; nsyy/nsmm/nsdd + nrange (days)
!c=====
subroutine endday(nsyy, nsmm, nsdd, neyy, nemm, nedd, nrange)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nsyy, nsmm, nsdd   !! start date
  integer(kind=i4b), intent(in)  :: nrange             !! (days)
  !c+++ [output]
  integer(kind=i4b), intent(out) :: neyy, nemm, nedd   !! end date
  !c+++ [internal work]
  integer(kind=i4b)              :: iyy, imm, idd      !!
  integer(kind=i4b)              :: iflag, irange, ndd !!
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
!c  subroutine dayloop
!c  OUTPUT irange; number of days
!c=====
subroutine dayloop(nsyy, nsmm, nsdd, neyy, nemm, nedd, irange)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nsyy, nsmm, nsdd !! start date
  integer(kind=i4b), intent(in)  :: neyy, nemm, nedd !! end date
  !c+++ [output]
  integer(kind=i4b), intent(out) :: irange           !! (days)
  !c+++ [internal work]
  integer(kind=i4b)              :: iyy, imm, idd    !!
  integer(kind=i4b)              :: iflag, ndd       !!
  irange = 0
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
        irange = irange + 1
        if ( (iyy == neyy) .and. (imm == nemm) .and. (idd == nedd) ) return
      enddo !! idd
    enddo !! imm
  enddo !! iyy
  
  return
end subroutine dayloop

!c----------------------------------------------------------------------c

!c======================================================================c

end module ucaln

