!c
!c  program gtjoinz
!c  [history]
!c  2018/01/10 Yamashita: first ver. (from 2017/05/15 gtcut.f90)
!c
!c  cut GTOOL3 data from nsyy/msmm/nsdd to neyy/nemm/nedd
!c  join z-axis of same date
!c
!c=====================================================================c
program gtjoinz
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use util, only: get_time
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgt, rgt_r4, rgthd, wgthdd
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  character(len=nfiln)        :: ifile, ofile     !! input/output file name
  character(len=ncc)          :: haxisz           !! z-axis name
  character(len=ncc)          :: hdfmt            !! data format
  integer(kind=r4b)           :: kmlim            !! maximum of z-sizes
  integer(kind=i4b)           :: nsyy, nsmm, nsdd !! start time
  integer(kind=i4b)           :: neyy, nemm, nedd !! end time
  logical                     :: oapnd            !! t: append, f: replace
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)        !! gtool3 header
  character(len=ncc)          :: head2(ndc)       !! gtool3 header (save)
  !c+++ input from gtool3 header
  integer(kind=r4b)           :: imax, jmax, kmax !! x-, y-, z-sizes
  real(kind=r8b)              :: rmiss            !! missing value
  !c+++ input data
  real(kind=r4b), allocatable :: d4(:,:,:)
  real(kind=r8b), allocatable :: d8(:,:,:)
  !c+++ [output]
  real(kind=r4b), allocatable :: d4o(:,:,:)
  real(kind=r8b), allocatable :: d8o(:,:,:)
  !c+++ [work]
  integer(kind=i4b)           :: it
  integer(kind=i4b)           :: ik               !! z-size
  integer(kind=i4b)           :: iyy, imm, idd    !! time
  integer(kind=i4b)           :: jyy, jmm, jdd    !! time (save)
  integer(kind=i4b)           :: jdate(6)         !! time array
  integer(kind=i4b)           :: ios, ios2        !! end code
  integer(kind=i4b)           :: jfile_i, jfile_o !! I/O unit for input/output file
  logical                     :: owrite = .false.

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

  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)
  call gtrewind(jfile_i, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)
  !c+++ allocate
  if (trim(hdfmt) == 'UR4') then
    allocate(d4o(imax,jmax,kmlim))
    d4o(1:imax,1:jmax,1:kmlim) = 0.0
  else
    allocate(d8o(imax,jmax,kmlim))
    d8o(1:imax,1:jmax,1:kmlim) = 0.d0
  endif

!c
!c main
!c===
  ios2 = 0
  jyy = -1
  jmm = -1
  jdd = -1
  ik = 0
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ read gtool header
    call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
    !ccc if (ios == -1) exit !! reach EOF
    !ccc if (ios /= 0) call ioerror(jfile_i, ios)
    if (ios > 0) call ioerror(jfile_i, ios)
    backspace(jfile_i)
    ios2 = ios
    !c+++ read time
    call get_time(head, jdate, ios)
    if (ios /= 0) call ioerror(-1, ios)
    !c+++ set date
    iyy = jdate(1)
    imm = jdate(2)
    idd = jdate(3)

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
      if (trim(hdfmt) == 'UR4') then
        !c+++ allocate
        allocate(d4(imax,jmax,kmax))
        !c+++ read gtool3 header & data
        call rgt_r4(jfile_i, imax, jmax, kmax, head, d4, ios)
      else
        !c+++ allocate
        allocate(d8(imax,jmax,kmax))
        !c+++ read gtool3 header & data
        call rgt(jfile_i, imax, jmax, kmax, head, d8, ios)
      endif
      !ccc if (ios == -1) exit !! reach EOF
      !ccc if (ios /= 0) call ioerror(jfile_i, ios)
      if (ios > 0) call ioerror(jfile_i, ios)

      if (iyy == jyy.and.imm == jmm.and.idd == jdd.and.ios == 0) then
        !c+++ continue
        !c+++ add z-data
        if (trim(hdfmt) == 'UR4') then
          d4o(1:imax,1:jmax,ik+1:ik+kmax) = d4(1:imax,1:jmax,1:kmax)
        else
          d8o(1:imax,1:jmax,ik+1:ik+kmax) = d8(1:imax,1:jmax,1:kmax)
        endif
        !c++ z-size
        ik = ik + kmax
        if (ik > kmlim) call ioerror(-1, 141)
      else
        !c+++ next date

        !c+++ write previous day data
        if (ik > 0) then
          !c+++ set gtool header
          head2(35) = haxisz
          write(head2(37), '(i16)') ik
          write(head2(64), '(i16)') imax*jmax*ik
          if (trim(hdfmt) == 'UR4') then
            !c+++ write gtool3 header & data
            call wgthdd(jfile_o, imax, jmax, ik, head2, hdfmt, d4o(1:imax,1:jmax,1:ik), ios)
          else
            !c+++ write gtool3 header & data
            call wgthdd(jfile_o, imax, jmax, ik, head2, hdfmt, d8o(1:imax,1:jmax,1:ik), ios)
          endif
          if (ios /= 0) call ioerror(jfile_o, ios)
        endif

        !c+++ init z-data
        if (trim(hdfmt) == 'UR4') then
          d4o(1:imax,1:jmax,1:kmlim) = 0.0
          d4o(1:imax,1:jmax,1:kmax) = d4(1:imax,1:jmax,1:kmax)
        else
          d8o(1:imax,1:jmax,1:kmlim) = 0.0
          d8o(1:imax,1:jmax,1:kmax) = d8(1:imax,1:jmax,1:kmax)
        endif
        !c+++ z-size
        ik = kmax
        if (ios2 == -1) exit !! reach EOF in file read
      endif
      !c+++ deallocate
      if (allocated(d4)) deallocate(d4)
      if (allocated(d8)) deallocate(d8)
    else
      !c+++ skip
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
    endif

    !c+++ save
    jyy = iyy
    jmm = imm
    jdd = idd
    head2(1:ndc) = head(1:ndc)
  enddo !! it
  !c+++ deallocate
  if (allocated(d4o)) deallocate(d4o)
  if (allocated(d8o)) deallocate(d8o)
  !c+++ close files
  call gtclose(jfile_i, ios)
  call gtclose(jfile_o, ios)

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
  character(len=ncc)            :: hval             !!
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ axis-name
  call get_parms('az', 'NUMBER1000', haxisz, ios)
  !c+++ maximum of z-axis-size
  call get_parms('zmax', '1000', hval, ios)
  call c2var(kmlim, '(i4.4)', hval)
  !c+++ start time
  call get_parms('nsyy', '0000', hval, ios)
  call c2var(nsyy, '(i4.4)', hval)
  call get_parms('nsmm', '01', hval, ios)
  call c2var(nsmm, '(i2.2)', hval)
  call get_parms('nsdd', '01', hval, ios)
  call c2var(nsdd, '(i2.2)', hval)
  !c+++ end time
  call get_parms('neyy', '9999', hval, ios)
  call c2var(neyy, '(i4.4)', hval)
  call get_parms('nemm', '12', hval, ios)
  call c2var(nemm, '(i2.2)', hval)
  call get_parms('nedd', '31', hval, ios)
  call c2var(nedd, '(i2.2)', hval)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtjoinz -i input-file -o output-file'
  write(6, '(a)') '-az z-axis_name -zmax z-maximum_size'
  write(6, '(a)') '-nsyy year -nsmm month -nsdd day (start time)'
  write(6, '(a)') '-neyy year -nemm month -nedd day (end time)'
  write(6, '(a)') '-apnd f/t'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtjoinz
