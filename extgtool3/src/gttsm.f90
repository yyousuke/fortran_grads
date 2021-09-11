!c
!c  program gttsm
!c  [history]
!c  2015/08/14 Yamashita: first ver. (from gtcut)
!c  2017/05/18 Yamashita: modify getparms & I/O 
!c  2017/05/18 Yamashita: use common_typedef
!c  2019/11/15 Yamashita: add area average
!c
!c  Time-Spatial Minimum
!c  cut GTOOL3 data from nsyy/msmm/nsdd to neyy/nemm/nedd
!c                  from nsx/nex, nsy/ney, nsz/nez
!c  calc=MIN: calculate minimum value
!c  calc=MAX: calculate maximum value
!c  calc=AVE: calculate area average
!c
!c
!c  external: module common_args
!c  external: module common_typedef
!c  internal: subroutine mdata
!c     external: module util
!c               subroutine darea_ave
!c    internal: subroutine rmin
!c    internal: subroutine rmax
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module util
!c            subroutine get_time
!c            subroutine getyaxis
!c  external: module ucaln
!c            subroutine dayloop
!c  external: module rwgtool
!c            subroutines seek_iounit, gtopen, rgthd, rgt, wgthdd, wmsg
!c  eaternal: error_handler
!c            subroutine ioerror
!c
!c=====================================================================c
program gttsm
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use util, only: get_time, getyaxis
  use ucaln, only: dayloop
  use rwgtool, only: seek_iounit, gtopen, rgthd, rgt, wgthdd, wmsg
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  character(len=nfiln)        :: ifile            !! input gtool file name
  character(len=nfiln)        :: ofile            !! output gtool file name
  character(len=nfiln)        :: afile            !! output text file name
  integer(kind=i4b)           :: nsyy, nsmm, nsdd !! start time
  integer(kind=i4b)           :: neyy, nemm, nedd !! end time
  integer(kind=i4b)           :: nsx, nex         !! x-sta, x-end
  integer(kind=i4b)           :: nsy, ney         !! y-sta, y-end
  integer(kind=i4b)           :: nsz, nez         !! z-sta, z-end
  character(len=ncc)          :: hitem            !! new item of output data
  character(len=ncc*2)        :: htitl            !! new title of output data
  character(len=ncc)          :: hunit            !! new unit of output data
  character(len=ncc)          :: hcalc            !! MIN/MAX
  character(len=ncc)          :: hdfmt            !! data format
  logical                     :: oapnd            !! t: append, f: replace
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)        !! gtool3 header
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax             !! x-axis sizes
  integer(kind=i4b)           :: jmax             !! y-axis sizes
  integer(kind=i4b)           :: kmax             !! z-axis sizes
  real(kind=r8b)              :: rmiss            !! missing value
  character(len=ncc)          :: haxisy         !! y-axis name
  !c+++ [input]
  !c+++ input from rgtd
  real(kind=r8b), allocatable :: d(:,:,:)         !! data
  !c+++ input from getyaxis
  real(kind=r8b), allocatable :: lat(:)           !! lat[deg]
  !c+++ [output]
  integer(kind=i4b)           :: ii, jj, kk       !! x-, y-, z-pos.
  real(kind=r4b)              :: rtime            !! time (year)
  real(kind=r4b)              :: dout(1,1,1)      !! output data
  !c+++ [work]
  integer(kind=i4b)           :: it
  integer(kind=i4b)           :: iyy, imm, idd    !! time
  integer(kind=i4b)           :: msx, mex         !! x-sta, x-end for calc.
  integer(kind=i4b)           :: msy, mey         !! y-sta, y-end for calc.
  integer(kind=i4b)           :: msz, mez         !! z-sta, z-end for calc.
  integer(kind=i4b)           :: jdate(6)         !! time array
  integer(kind=i4b)           :: iday             !! days of year
  integer(kind=i4b)           :: nday             !! number of days per year
  integer(kind=i4b)           :: ios              !! end code
  integer(kind=i4b)           :: jfile_i          !! I/O unit for input gtool file
  integer(kind=i4b)           :: jfile_o          !! I/O unit for output gtool file
  integer(kind=i4b)           :: jfile_a          !! I/O unit for output text file
  logical                     :: owrite = .false. !!

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ open input gtool file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open output gtool file
  write(6, *) 'open output file: ', trim(ofile)
  if (oapnd) then
    call gtopen(trim(ofile), 'a', jfile_o, ios)
  else
    call gtopen(trim(ofile), 'w', jfile_o, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

  !c+++ open output text file
  write(6, *) 'open output text file: ', trim(afile)
  if (oapnd) then
    call gtopen(trim(afile), 'aa', jfile_a, ios)
  else
    call gtopen(trim(afile), 'wa', jfile_a, ios)
  endif
  if (ios /= 0) call ioerror(jfile_a, ios)
!c
!c main
!c===
  it = 0
  do while(1 == 1)
    it = it + 1

    !c+++ read header & set axis-sizes, missing value
    call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    backspace(jfile_i)
    !c+++ allocate
    allocate(d(imax,jmax,kmax))

    !c+++ read gtool3 header & data
    call rgt(jfile_i, imax, jmax, kmax, head, d, ios)
    if (ios /= 0) then
      deallocate(d)
      call ioerror(jfile_i, ios)
    endif

    !c+++ work area
    msx = nsx
    if (nsx <= 0) msx = 1
    mex = nex
    if (nex <= 0) mex = imax
    msy = nsy
    if (nsy <= 0) msy = 1
    mey = ney
    if (ney <= 0) mey = jmax
    msz = nsz
    if (nsz <= 0) msz = 1
    mez = nez
    if (nez <= 0) mez = kmax

    !c+++ read time
    call get_time(head, jdate, ios)
    if (ios /= 0) call ioerror(-1, ios)
    call dayloop(jdate(1), 1, 1, jdate(1), jdate(2), jdate(3), iday)
    call dayloop(jdate(1), 1, 1, jdate(1),       12,       31, nday)
    rtime = jdate(1) + (iday - 1) / dble(nday) &
&         + jdate(4) / dble(nday * 24) &
&         + jdate(5) / dble(nday * 24 * 60)

    !c+++ set y-axis name
    haxisy = head(32)
    !c+++ set data format of output data
    if (hdfmt == 'NULL') hdfmt = head(38)
    !c+++ set unit of gtool header
    if (hunit /= 'NULL') head(16) = hunit
    !c+++ set item of gtool header
    if (hitem /= 'NULL') head(3) = hitem
    !c+++ set title of gtool header
    if (htitl /= 'NULL') then
      head(14) = htitl(1:16)
      head(15) = htitl(17:32)
    endif
    !c+++ set size of output data
    head(29) = ' '
    write(head(31), '(i16)') 1
    head(32) = ' '
    write(head(34), '(i16)') 1
    head(35) = ' '
    write(head(37), '(i16)') 1
    write(head(64), '(i16)') 1

    !c+++ set date
    iyy = jdate(1)
    imm  = jdate(2)
    idd  = jdate(3)
    !c+++ if iyy/imm/idd is later than nsyy/nsmm/nsdd, output is true
    if (.not. owrite) then
      if (iyy == nsyy.and.imm > nsmm.or.iyy > nsyy) owrite = .true.
      if (iyy == nsyy.and.imm == nsmm) then
        if (idd >= nsdd) owrite = .true.
      endif
    endif

    !c+++ if iyy/imm/idd is later than neyy/nemm/nedd, output is false
    if (iyy == neyy.and.imm > nemm.or.iyy > neyy) owrite = .false.
    if (iyy == neyy.and.imm == nemm.and.idd > nedd) owrite = .false.

    !c+++ write
    if (owrite) then
      allocate(lat(jmax))
      !c+++ read y-axis file
      call getyaxis(jmax, haxisy, lat)
      !c+++ calculate min./max.
      call mdata(imax, jmax, kmax, msx, mex, msy, mey, msz, mez, rmiss, &
&       lat, hcalc, d, ii, jj, kk, dout(1,1,1))
      deallocate(lat)

      !c+++ write gtool3 header & data
      call wgthdd(jfile_o, 1, 1, 1, head, hdfmt, dout, ios)
      if (ios /= 0) call ioerror(jfile_o, ios)

      !c+++ write text data
      write(jfile_a, '(a, 1x, f9.4, 1x, 3(i4, 1x), e15.7)') &
&       head(27), rtime, ii, jj, kk, dout
      call wmsg(head, 'WTEXT')
    endif
    !c+++ deallocate
    deallocate(d)
  enddo !! it
  !c+++ close files
1 close(jfile_i)
  close(jfile_o)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine mdata
!c
!c=====
subroutine mdata(imax, jmax, kmax, msx, mex, msy, mey, msz, mez, rmiss, &
& lat, hcalc, d, ii, jj, kk, dout)
  use util, only: darea_ave
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax     !! x-, y-, z-sizes
  integer(kind=i4b), intent(in)  :: msx, mex             !! x-sta, x-end
  integer(kind=i4b), intent(in)  :: msy, mey             !! y-sta, y-end
  integer(kind=i4b), intent(in)  :: msz, mez             !! z-sta, z-end
  real(kind=r8b), intent(in)     :: rmiss                !! missing value
  real(kind=r8b), intent(in)     :: lat(jmax)            !! lat[deg]
  character(len=*), intent(in)   :: hcalc                !! MIN/MAX
  real(kind=r8b), intent(in)     :: d(imax,jmax,kmax)    !! data
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ii, jj, kk           !! position
  real(kind=r4b), intent(out)    :: dout                 !! data
  !c+++ [internal work]
  integer(kind=i4b)              :: i, j, k              !! loop variables
  integer(kind=i4b)              :: nrec                 !! record length
  integer(kind=i4b)              :: ipos                 !! position
  real(kind=r8b), allocatable    :: w(:)                 !! work data
  integer(kind=i4b), allocatable :: iw(:,:)              !! work pos.
  real(kind=r8b)                 :: hol(imax,jmax)       !! horizontal data
  real(kind=r8b)                 :: mean                 !! work data

  if (trim(hcalc) == 'AVE') then
    dout = 0.0
    do k = msz, mez
      hol(1:imax,1:jmax) = d(1:imax,1:jmax,k)
      call darea_ave(imax, jmax, msx, mex, msy, mey, rmiss, &
&       hol, lat, mean, .true.)
      dout = dout + mean
    enddo !! k
    !c+++ set output data
    ii = -1
    jj = -1
    kk = -1
    dout = dout / dble(mez - msz + 1)
  else
    allocate(w(imax*jmax*kmax))    !! work data
    allocate(iw(imax*jmax*kmax,3)) !! work pos.
    !c+++ copy 
    nrec = 0
    do k = msz, mez
      do j = msy, mey
        do i = msx, mex
          nrec = nrec + 1
          w(nrec) = d(i,j,k)
          iw(nrec,1) = i
          iw(nrec,2) = j
          iw(nrec,3) = k
        enddo !! i
      enddo !! j
    enddo !! k
    if (trim(hcalc) == 'MIN') then
      call rmin(nrec, w(1:nrec), ipos)
    else if (trim(hcalc) == 'MAX') then
      call rmax(nrec, w(1:nrec), ipos)
    endif
    !c+++ set output data
    ii = iw(ipos,1)
    jj = iw(ipos,2)
    kk = iw(ipos,3)
    dout = w(ipos)
    deallocate(w, iw)
  endif

  return
end subroutine mdata

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine rmin
!c
!c=====
subroutine rmin(n, din, imin)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: n      !! data size
  real(kind=r8b), intent(in)     :: din(n) !! input data
  !c+++ [output]
  integer(kind=i4b), intent(out) :: imin   !! number at mimimum value
  !c+++ [internal work]
  integer(kind=i4b)              :: j      !!

  imin = 1
  do j = 1, n
    if (din(j) < din(imin)) imin = j
  enddo !! j

  return
end subroutine rmin

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine rmax
!c
!c=====
subroutine rmax(n, din, imax)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: n      !! data size
  real(kind=r8b), intent(in)     :: din(n) !! input data
  !c+++ [output]
  integer(kind=i4b), intent(out) :: imax   !! number at maximum value
  !c+++ [internal work]
  integer(kind=i4b)              :: j      !!

  imax = 1
  do j = 1, n
    if (din(j) > din(imax)) imax = j
  enddo !! j

  return
end subroutine rmax

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c=====
subroutine getparms
  use uopts, only: read_parms, get_parms, get_strend
  use char2var, only: c2var
  !c+++ [internal work]
  character(len=ncc)            :: hval             !!
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  call get_parms('a', 'gtool.txt', afile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  !c+++ startd time
  call get_parms('nsyy', '0', hval, ios)
  call c2var(nsyy, '(i4.4)', hval)
  call get_parms('nsmm', '1', hval, ios)
  call c2var(nsmm, '(i2.2)', hval)
  call get_parms('nsdd', '1', hval, ios)
  call c2var(nsdd, '(i2.2)', hval)
  !c+++ end time
  call get_parms('neyy', '9999', hval, ios)
  call c2var(neyy, '(i4.4)', hval)
  call get_parms('nemm', '12', hval, ios)
  call c2var(nemm, '(i2.2)', hval)
  call get_parms('nedd', '31', hval, ios)
  call c2var(nedd, '(i2.2)', hval)
  !c+++ x-ranges
  call get_parms('nsx', '-1', hval, ios)
  call c2var(nsx, '(i16)', hval)
  call get_parms('nex', '-1', hval, ios)
  call c2var(nex, '(i16)', hval)
  !c+++ y-ranges
  call get_parms('nsy', '-1', hval, ios)
  call c2var(nsy, '(i16)', hval)
  call get_parms('ney', '-1', hval, ios)
  call c2var(ney, '(i16)', hval)
  !c+++ z-ranges
  call get_parms('nsz', '-1', hval, ios)
  call c2var(nsz, '(i16)', hval)
  call get_parms('nez', '-1', hval, ios)
  call c2var(nez, '(i16)', hval)
  !c+++ calc.
  call get_parms('calc', 'MIN', hcalc, ios)
  if (trim(hcalc) == 'MIN' .or. trim(hcalc) == 'MAX' .or. trim(hcalc) == 'AVE') then
  else
    call xabort
  endif
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine xabort
!c=====
subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gttsm -i input-file -o output-file'
  write(6, '(a)') '-a output-text-file'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-nsyy year -nsmm month -nsdd day (start time)'
  write(6, '(a)') '-neyy year -nemm month -nedd day (end time)'
  write(6, '(a)') '-nsx x-sta -nex x-end'
  write(6, '(a)') '-nsy y-sta -ney y-end'
  write(6, '(a)') '-nsz z-sta -nez z-end'
  write(6, '(a)') '-calc MIN/MAX/AVE'
  write(6, '(a)') '-apnd f/t'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c=====================================================================c

end program gttsm
