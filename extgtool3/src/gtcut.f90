!c
!c  program gtcut
!c  [history]
!c  2013/02/01 Yamashita: first ver.
!c  2013/03/11 Yamashita: use modules
!c  2013/10/18 Yamashita: modify gtopen and remove seek_iounit
!c  2014/08/04 Yamashita: add dfmt option
!c  2017/04/27 Yamashita: modify getparms & I/O 
!c  2017/04/28 Yamashita: modify I/O error
!c  2017/05/01 Yamashita: use common_typedef
!c  2017/05/15 Yamashita: remove werr, werrc
!c  cut GTOOL3 data from nsyy/msmm/nsdd to neyy/nemm/nedd
!c
!c=====================================================================c
program gtcut
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use util, only: get_time
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgt, rgt_r4, rgthd, wgthdd
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  !c+++ (ncc: length of gtool3 header, nfiln: length of file name)
  character(len=nfiln)        :: ifile, ofile     !! input/output file name
  character(len=ncc)          :: hdfmt            !! data format
  integer(kind=i4b)           :: nsyy, nsmm, nsdd !! start time
  integer(kind=i4b)           :: neyy, nemm, nedd !! end time
  logical                     :: oapnd            !! t: append, f: replace
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)        !! gtool3 header
  !c+++ input from gtool3 header
  integer(kind=r4b)           :: imax, jmax, kmax !! x-, y-, z-sizes
  real(kind=r8b)              :: rmiss            !! missing value
  !c+++ [input/output]
  real(kind=r4b), allocatable :: d4(:,:,:)
  real(kind=r8b), allocatable :: d8(:,:,:)
  !c+++ [work]
  integer(kind=i4b)           :: it
  integer(kind=i4b)           :: iyy, imm, idd    !! time
  integer(kind=i4b)           :: jdate(6)         !! time array
  integer(kind=i4b)           :: ios              !! end code
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

!c
!c main
!c===
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ read gtool header
    call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    backspace(jfile_i)

    !c+++ read time
    call get_time(head, jdate, ios)
    if (ios /= 0) call ioerror(-1, ios)

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
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)

      if (trim(hdfmt) == 'UR4') then
        !c+++ write gtool3 header & data
        call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, d4, ios)
        !c+++ deallocate
        deallocate(d4)
      else
        !c+++ write gtool3 header & data
        call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, d8, ios)
        !c+++ deallocate
        deallocate(d8)
      endif
      if (ios /= 0) call ioerror(jfile_o, ios)
    else
      !c+++ skip
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
    endif
  enddo !! it
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
  write(6, '(a)') 'gtcut -i input-file -o output-file'
  write(6, '(a)') '-nsyy year -nsmm month -nsdd day (start time)'
  write(6, '(a)') '-neyy year -nemm month -nedd day (end time)'
  write(6, '(a)') '-apnd f/t'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtcut
