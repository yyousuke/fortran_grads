!c
!c  program mktgrd
!c  [history]
!c  2006/05/17 Yamashita: first ver.
!c  2006/06/02 Yamashita: nsmmが1月でない場合にもicが1から始まるバグを修正
!c  2021/06/10 Yamashita: for daily mean/monthly mean
!c  
!c  output: jc(record number), rc(time), jyy(year), jmm(month)
!c
!c
!c  write grid information of time
!c  for monthly mean
!c   range; nsyy/nsmm - neyy/nemm
!c   nmm; number of months
!c   iflag = 0 ;  write whole monthes in nsyy/nsmm - neyy/nemm
!c   iflag = 1 ;  write nsmm to nemm
!c   output: jc(record number), rc(time), jyy(year), jmm(month)
!c 
!c  for daily mean
!c   range; nsyy/nsmm - neyy/nemm
!c   nmm; number of months
!c   iflag = 2 ;  write whole days in nsyy/nsmm - neyy/nemm
!c   iflag = 3 ;  write nsmm to nemm
!c   output: jc(record number), rc(time), jyy(year), jmm(day of year)
!c   ex. DJF mean: iflag = 3, jyy is previous year for Jan., Feb.
!c     jmm is days from 1 Dec.
!c   ex. JJA mean: iflag = 3, jyy is year, jmm is days from 1 Jun.
!c  
!c======================================================================c
program mktgrd
  implicit none
  !c+++ [input]
  integer(4) :: nsyy, nsmm !! start year/month
  integer(4) :: neyy, nemm !! end year/month
  integer(4) :: nmm        !! number of months or days
  integer(4) :: iflag      !! 
  !c+++ [work]
  integer(4) :: ic         !! record number (must be same as bin file)
  integer(4) :: iyy        !! index of year
  integer(4) :: imm        !! index of month
  integer(4) :: idd        !! index of day
  real(8)    :: rc, rc2    !! time (year)

  !c+++ input
  read(5, *) nsyy, nsmm, neyy, nemm, nmm, iflag
  !c+++ open output file
  open (10, file='fort.10')
  !c+++ eliminate invalid input
  rc = nsyy + (nsmm - 1) / dble(nmm)
  rc2 = neyy + (nemm - 1) / dble(nmm)
  if (rc > rc2) then
    write(6, *) 'Error: the start time is later then the end time.'
    stop
  endif

  if (iflag == 0.or. iflag == 1) then
    !c+++ monthly index
    call mkmonthly
  else if (iflag == 2.or. iflag == 3) then
    !c+++ daily index
    call mkdaily
  else
    write(6, *) 'Error: invalid input iflag, ', iflag
    stop
  endif

  stop

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c
!c  subroutine mkmonthly
!c  [history]
!c  2006/05/17 Yamashita: first ver.
!c  2006/06/02 Yamashita: nsmmが1月でない場合にもicが1から始まるバグを修正
!c  2021/06/10 Yamashita: subroutine ver.
!c
!c  input data must be started from nsyy/nsmm with monthly time step
!c  output: jc(record number), rc(time), jyy(year), jmm(month)
!c=====
subroutine mkmonthly()

  ic = 0
  year: do iyy = nsyy, neyy
    month: do imm = 1, nmm
      if (iyy == nsyy.and.imm < nsmm) then
         if (iflag == 1) ic = ic + 1
         cycle month
      endif 
      ic = ic + 1

      if (iflag == 1.and.nsmm <= nemm) then
        if (imm < nsmm.or.nemm < imm) cycle month
      elseif (iflag == 1.and.nsmm > nemm) then
        if (nemm < imm.and.imm < nsmm) cycle month
      endif
      rc = iyy + (imm - 1) / dble(nmm)
      !c+++ write record id and time
      write(6, '(i6, 2x, f15.8, 2(2x, i6))') ic, rc, iyy, imm
      write(10, '(i6, 2x, f15.8, 2(2x, i6))') ic, rc, iyy, imm
      if (iyy == neyy.and.imm == nemm) exit year
    enddo month !! imm
  enddo year !! iyy

  return
end subroutine mkmonthly

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c
!c  subroutine mkdaily
!c  [history]
!c  2021/06/10 Yamashita: first ver.
!c
!c  input data must be started from nsyy/nsmm with daily time step
!c  output: jc(record number), rc(time), jyy(year), jmm(day of year)
!c=====
subroutine mkdaily()
  !c+++ [internal work]
  integer(4) :: ista         !! record number (start)
  integer(4) :: iend         !! record number (end)
  integer(4) :: nedd         !! last day of month
  integer(4) :: ndd          !! days of year
  integer(4) :: iyy2         !! index of year for write

  year: do iyy = nsyy, neyy
    if (iflag == 2) then
      call dayloop(iyy, 1, 1, iyy, 12, 31, ndd) !! days of year
    else if (iflag == 3.and.nsmm <= nemm) then
      call eday(iyy, nemm, nedd)
      call dayloop(iyy, nsmm, 1, iyy, nemm, nedd, ndd) !! days from nsmm/1
    elseif (iflag == 3.and.nsmm > nemm) then
      call eday(iyy+1, nemm, nedd)
      call dayloop(iyy, nsmm, 1, iyy+1, nemm, nedd, ndd) !! days from nsmm/1
    endif
    idd = 0
    month: do imm = 1, nmm
      iyy2 = iyy
      if (iyy == nsyy.and.imm < nsmm) cycle month
      if (iflag == 3.and.nsmm <= nemm) then
        if (imm < nsmm.or.nemm < imm) cycle month
      else if (iflag == 3.and.nsmm > nemm) then
        if (nemm < imm.and.imm < nsmm) cycle month
        if (imm <= nemm) iyy2 = iyy - 1 !! e.g. DJF case
        call dayloop(iyy2, nsmm, 1, iyy, imm, 1, idd) !! days from prev. year
        idd = idd - 1
      endif
      if (iflag == 2) then
        call dayloop(iyy, 1, 1, iyy, imm, 1, idd) !! days of year
        idd = idd - 1
      endif
      !c+++ calc. record number of start day
      call dayloop(nsyy, 1, 1, iyy, imm, 1, ista)
      !c+++ calc. record number of end day
      call eday(iyy, imm, nedd)
      call dayloop(nsyy, 1, 1, iyy, imm, nedd, iend)
      !c+++ daily loop
      day: do ic = ista, iend
        idd = idd + 1
        rc = iyy2 + (idd - 1) / dble(ndd)
        !c+++ write record id and time
        write(6, '(i6, 2x, f15.8, 2(2x, i6))') ic, rc, iyy2, idd
        write(10, '(i6, 2x, f15.8, 2(2x, i6))') ic, rc, iyy2, idd
      enddo day !! ic
      if (iyy == neyy.and.imm == nemm) exit year
    enddo month !! imm
  enddo year !! iyy

  return
end subroutine mkdaily

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine dayloop
!c  OUTPUT irange; number of days
!c=====
subroutine dayloop(nsyy, nsmm, nsdd, neyy, nemm, nedd, irange)
  !c+++ [input]
  integer(4), intent(in)  :: nsyy, nsmm, nsdd, neyy, nemm, nedd
  !c+++ [output]
  integer(4), intent(out) :: irange
  !c+++ [internal work]
  integer(4)              :: iyy, imm, idd, iflag, ndd
  irange = 0
  do iyy = nsyy, neyy
    iflag = 0
    if ( mod(iyy,4) == 0 .and. mod(iyy,100) /= 0 &
      .or. (mod(iyy,400) == 0) ) iflag = 1
    if ( iyy == nsyy .and. iyy == neyy .and. nemm < nsmm ) cycle
    do imm = 1, nmm
      if ( iyy == nsyy .and. imm < nsmm ) cycle
      if (imm == 2) then
        ndd = 28 + iflag
      elseif ( mod(imm,2) == 1 .and. imm <= 7 &
        .or. mod(imm,2) == 0 .and. imm >= 8 ) then
        ndd = 31
      else
        ndd = 30
      endif
      do idd = 1, ndd
        if ( iyy == nsyy .and. imm == nsmm .and. idd < nsdd) cycle
        irange = irange + 1
        if ( iyy == neyy .and. imm == nemm .and. idd == nedd) return
      enddo !! idd
    enddo !! imm
  enddo !! iyy
  
  return
end subroutine dayloop

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
  if ( mod(iyy,4) == 0 .and. mod(iyy,100) /= 0 &
    .or. mod(iyy,400) == 0) iflag = 1
  if (imm == 2) then
    ndd = 28 + iflag
  elseif ( mod(imm,2) == 1 .and. imm <= 7 &
    .or. mod(imm,2) == 0 .and. imm >= 8 ) then
    ndd = 31
  else
    ndd = 30
  endif

  return
end subroutine eday

!c----------------------------------------------------------------------c

end program mktgrd
