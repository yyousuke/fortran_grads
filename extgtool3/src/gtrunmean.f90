!c
!c  program gtrunmean
!c  [history]
!c  2016/11/22 Yamashita: first ver.
!c  2017/02/27 Yamashita: options (mvf and -mval)
!c
!c  running mean  (Hamming filter)
!c
!c  external: module common_typedef
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module util
!c            subroutines set_user, set_jdaten, get_time
!c  external module util_runmean
!c            subroutine runmean_main
!c  external: module ucaln
!c            subroutine datetime2sec
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtrewind, gtskip, rgthd,
!c            rgt_r4, wgthdd
!c  eaternal: error_handler
!c            subroutine ioerror, werr2
!c
!c=====================================================================c
program gtrunmean
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b

  use util, only: set_user, set_jdaten, get_time
  use util_runmean, only: runmean_main
  use ucaln, only: datetime2sec
  use rwgtool, only: gtopen, gtclose, gtrewind, gtskip, rgthd, rgt_r4, wgthdd
  use error_handler, only: ioerror, werr2
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd)
  character(len=ncc)              :: head(ndc)    !! gtool3 header
  !c+++ input from rgthd
  integer(kind=i4b)               :: imax         !! x-axis sizes
  integer(kind=i4b)               :: jmax         !! y-axis sizes
  integer(kind=i4b)               :: kmax         !! z-axis sizes
  real(kind=r8b)                  :: rmiss        !! missing value
  !c+++ input from getparms
  integer(kind=i4b)               :: ista         !! start record
  integer(kind=i4b)               :: iend         !! end record
  character(len=nfiln)            :: ifile        !! input file name
  character(len=nfiln)            :: ofile        !! output file name
  character(len=ncc)              :: hitem        !! new item of output data
  character(len=ncc*2)            :: htitl        !! new title of output data
  character(len=ncc)              :: hunit        !! new unit of output data
  character(len=ncc)              :: mtype        !! type of running mean
  real(kind=r4b)                  :: tavr         !! time ave. of fourier (s)
  real(kind=r4b)                  :: mval         !! minimum-value (potype=mvf)
  character(len=ncc)              :: hdfmt          !! data format
  logical                         :: oapnd        !! t: append, f: replace
  !c+++ input from rgtd
  real(kind=r4b), allocatable     :: din(:,:,:)   !! input data 
  !c+++ [output]
  !c+++ output with wgthdd
  real(kind=r4b), allocatable     :: dout(:,:,:)  !! output data
  !c+++ [work]
  integer(kind=i4b)               :: it
  integer(kind=i4b)               :: nd           !! data number
  integer(kind=i4b)               :: ios          !! end code
  real(kind=r4b), allocatable     :: d(:,:,:,:)   !! data for save
  character(len=ncc), allocatable :: hd(:,:) !! gtool3 header for save
  !c+++ time
  integer(kind=i4b)               :: jdate(6)     !! yy, mm, dd, hh, mn, ss
  real(kind=r4b)                  :: tlen         !! tlen = timee-times (s)
  real(kind=r4b)                  :: times        !! start time (s)
  real(kind=r4b)                  :: timee        !! end time (s)
  !c+++ I/O unit number
  integer(kind=i4b)               :: jfile_i      !! for input file
  integer(kind=i4b)               :: jfile_o      !! for output file

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
  !c+++ get time from header
  call get_time(head, jdate, ios)
  if (ios /= 0) call ioerror(-1, ios)
  !c+++ jdate ==> times (s)
  call datetime2sec(jdate(1), jdate(2), jdate(3), jdate(4), jdate(5), jdate(6), times)

  !c+++ allocate
  allocate(din(imax,jmax,kmax), dout(imax,jmax,kmax))
  allocate(d(imax,jmax,kmax,max(9999, iend)))
  allocate(hd(ndc,max(9999, iend)))

!c
!c read
!c===
  it = 0
  nd = 0
  do while(1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      cycle
    endif
    !c+++ read data (current for ocent=f, next for ocent=t)
    call rgt_r4(jfile_i, imax, jmax, kmax, head, din, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)
    nd = nd + 1
    d(1:imax,1:jmax,1:kmax,nd) = din(1:imax,1:jmax,1:kmax)
    hd(1:ndc,nd) = head(1:ndc)

    if (it == iend) exit
  enddo !! it
  !c+++ close input file
  call gtclose(jfile_i, ios)
  !c+++ get time from header
  call get_time(head, jdate, ios)
  if (ios /= 0) call ioerror(-1, ios)
  !c+++ jdate ==> timee (s)
  call datetime2sec(jdate(1), jdate(2), jdate(3), jdate(4), jdate(5), jdate(6), timee)
  tlen = (timee - times)
  if (int(tlen / tavr) > nd) then
    write(6, *) 'tlen: ', tlen
    write(6, *) 'tavr: ', tavr
    write(6, *) 'ien: ', int(tlen / tavr)
    call werr2("tavr must be larger than time step of data")
  endif
    
!c
!c running mean
!c===
  call runmean_main(imax, jmax, kmax, nd, real(rmiss), d(1:imax,1:jmax,1:kmax,1:nd), tlen, tavr, mval, mtype)

!c
!c write
!c===
  do it = 1, nd
    !c+++ set output data
    head(1:ndc) = hd(1:ndc,it)
    dout(1:imax,1:jmax,1:kmax) = d(1:imax,1:jmax,1:kmax,it)
    !c+++ set dfmt of output data
    head(38) = 'UR4'
    !c+++ set unit of output data
    if (hunit /= 'NULL') head(16) = hunit
    !c+++ set item of output data
    if (hitem /= 'NULL') head(3) = hitem
    !c+++ set title of output data
    if (htitl /= 'NULL') then
      head(14) = htitl(1:16)
      head(15) = htitl(17:32)
    endif

    !c+++ write gtool3 header & data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, dout, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)
  enddo !! it
  write(6, '(a, i6)') 'total record number = ', nd

  !c+++ deallocate
  deallocate(din, dout, d, hd)
  !c+++ close files
  call gtclose(jfile_o, ios)

  stop

contains

!c=====================================================================c

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
  !c+++ input/output files
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ type of running mean
  call get_parms('mtype', 'frt_ham', mtype, ios)
  !c+++ time ave. of fourier (s)
  call get_parms('tavr', '86400.0', hval, ios)
  call c2var(tavr, '(1pe15.5)', hval)
  !c+++ minimum-value
  call get_parms('mval', '0.0', hval, ios)
  call c2var(mval, '(1pe15.5)', hval)
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
  write(6, '(a)') 'gtrunmean -i input-file -o output-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-tavr time (s) (average time, default: 86400.0)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-mtype TYP_WIN_PRE_POS'
  write(6, '(a)') 'first 3 digit TYP: type of running mean'
  write(6, '(a)') '  frt: fourier transform'
  write(6, '(a)') '  box: boxcar filter and ignore WIN/PRE'
  write(6, '(a)') 'second 3 digit WIN: type of window function'
  write(6, '(a)') '  ham: Hamming window'
  write(6, '(a)') '  han: hann window'
  write(6, '(a)') '  rec: rectangular window'
  write(6, '(a)') 'third 3 digit PRE: type of pre processing'
  write(6, '(a)') '  rta or rat: remove trend and average before fourier'
  write(6, '(a)') '  rav: remove average before fourier'
  write(6, '(a)') '  rtr: remove trend before fourier'
  write(6, '(a)') 'fourth 3 digit POS: type of post processing'
  write(6, '(a)') '  nnf: non negative filter after fourier'
  write(6, '(a)') '  mvf: minimum-value filter after fourier'
  write(6, '(a)') '  (specify -mval minimum-value, default: 0.0)'
  write(6, '(a)') ' '
  write(6, '(a)') '-mtype frt_ham or frt_ham__'
  write(6, '(a)') '  fourier transform with Hamming filter (default)'
  write(6, '(a)') '-mtype frt_ham_rta or frt_ham_rta_'
  write(6, '(a)') '  fourier transform with Hamming filter and remove ave/trend'
  write(6, '(a)') '-mtype frt_ham__nnf'
  write(6, '(a)') '  fourier transform with Hamming filter and non-negative filter'
  write(6, '(a)') '-mtype box'
  write(6, '(a)') '  boxcar filter'
  write(6, '(a)') '-mtype box___mvf -mval 300.0'
  write(6, '(a)') '  boxcar filter with minimum-value filter (values larger than 300)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '-apnd f/t'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtrunmean
