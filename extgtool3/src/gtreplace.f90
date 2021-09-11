!c
!c  program gtreplace
!c  [history]
!c  2013/10/10 Yamashita: first ver.
!c  2013/10/18 Yamashita: modify gtopen and remove seek_iounit
!c  2017/07/28 Yamashita: modify getparms & I/O 
!c  2017/07/28 Yamashita: use common_typedef & common_const
!c
!c  replace time-varying data by constant data
!c
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtskip, gtrewind, rgt, rgth, rgt_r4, rgthd, wgthdd
!c  eaternal: error_handler
!c            subroutine ioerror, werr
!c
!c=====================================================================c
program gtreplace
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use rwgtool, only: gtopen, gtclose, gtskip, gtrewind, rgt_r4, rgth, rgthd, wgthdd
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header
  real(kind=r4b), allocatable :: d(:,:,:)       !! input data
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax           !! x-, y-, z-axis sizes
  integer(kind=i4b)           :: jmax           !! x-, y-, z-axis sizes
  integer(kind=i4b)           :: kmax           !! x-, y-, z-axis sizes
  real(kind=r8b)              :: rmiss          !! missing value
  !c+++ input from getparms
  !c+++ (ncc: length of gtool3 header, nfiln: length of file name)
  integer(kind=i4b)           :: ista, iend     !! start/end record
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: rfile          !! input const-data file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [input/output]
  real(kind=r4b), allocatable :: r(:,:,:)       !! replace data
  !c+++ [work]
  integer(kind=i4b)           :: it             !!
  integer(kind=i4b)           :: ios            !! end code
  character(len=ncc*2)        :: htitlz
  integer(kind=i4b)           :: jfile_i        !! for input file
  integer(kind=i4b)           :: jfile_o        !! for output file
  integer(kind=i4b)           :: jfile_r        !! I/O unit for input const-data file

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ open input gtool file
  write(6, *) 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open output file
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
  allocate(d(imax,jmax,kmax), r(imax,jmax,kmax))

!c
!c read replace data
!c===
  !c+++ open input const-data file
  write(6, *) 'open input-const-data file: ', trim(rfile)
  call gtopen(trim(rfile), 'r', jfile_r, ios)
  if (ios /= 0) call ioerror(jfile_r, ios)

  !c+++ read gtool header & data
  call rgt_r4(jfile_r, imax, jmax, kmax, head2, r, ios)
  if (ios /= 0) call ioerror(jfile_r, ios)
  !c+++ check data sizes
  if (head(31) /= head2(31)) call werr(head(31), head2(31), 'Data sizes are not match!')
  if (head(34) /= head2(34)) call werr(head(34), head2(34), 'Data sizes are not match!')
  if (head(37) /= head2(37)) call werr(head(37), head2(37), 'Data sizes are not match!')
  !c+++ check axis names
  if (head(29) /= head2(29)) call werr(head(29), head2(29), 'Axis names are not match!')
  if (head(32) /= head2(32)) call werr(head(32), head2(32), 'Axis names are not match!')
  if (head(35) /= head2(35)) call werr(head(35), head2(35), 'Axis names are not match!')

  !c+++ set unit of output data
  if (hunit == 'NULL') hunit = head2(16)
  !c+++ set item of output data
  if (hitem == 'NULL') hitem = head2(3)
  !c+++ set title of output data
  if (htitl == 'NULL') then
    htitl(1:16) = head2(14)
    htitl(17:32) = head2(15)
  endif
  !c+++ close input const-data file
  call gtclose(jfile_r, ios)

!c
!c main
!c===
  it = 0
  do while(1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      cycle
    endif

    !c+++ read input file (read header & skip data)
    call rgth(jfile_i, head, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit

    !c+++ write replace data
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, r, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo !! it
  !c+++ close files
  call gtclose(jfile_i, ios)
  call gtclose(jfile_o, ios)
  deallocate(d, r)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c
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
  call get_parms('r', 'const.gt', rfile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
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
  write(6, '(a)') 'gtreplace -o output-file'
  write(6, '(a)') '-i input-file for definition of output data time'
  write(6, '(a)') '-r constant-data-file for replacement'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtreplace
