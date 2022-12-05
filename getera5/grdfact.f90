!c  
!c  grdfact.f90
!c  2017/04/20 first ver. (Yamashita)
!c
!c  dout = din * fact + offset
!c======================================================================c 
program main
  use calendar, only: eday, dayloop
  use calculate, only: shift
  use rw, only: fclose, fread_r4, fwrite_r4
  use io, only: rdopen, wdopen
  implicit none
  !c+++ [parameter]
  integer(4), parameter:: nmm = 12   !! months
  !c+++ [input]
  !c+++ from getparms
  integer(4)           :: nx, ny, nz !! x-, y-, z-sizes
  integer(4)           :: nrecparday !! record per day
  integer(4)           :: nsyy, nsmm !! start year, month
  integer(4)           :: neyy, nemm !! end year, month
  real(4)              :: undef      !! missing value
  character(200)       :: idir, odir !! input/output directory name
  character(100)       :: ivar, ovar !! input/output variable name
  character(30)        :: isuf, osuf !! input/output suffix name
  real(8)              :: fact       !! factor
  real(8)              :: offset     !! offset
  logical              :: onefile    !! t: read/write 1 file on, f: off
  logical              :: omonth     !! t: monthly mean data, f: daily/6hr
  !c+++ from fil
  real(4), allocatable :: din(:,:,:)
  !c+++ [output]
  real(4), allocatable :: dout(:,:,:)
  !c+++ [work]
  integer(4)           :: iyy, imm, it
  integer(4)           :: nlen, ista, iend, ic, irec, irr
  integer(4)           :: nsm, nem, ndd, nt
  character(10)        :: ayy
  character(400)       :: ifile     !! input file
  character(400)       :: ofile     !! output file
  real(8)              :: undef8    !! missing value
  !c+++ input 
  call getparms(nx, ny, nz, nrecparday, nsyy, nsmm, neyy, nemm, undef, &
&   idir, odir, ivar, ovar, isuf, osuf, fact, offset, onefile, omonth)
  nlen = nx * ny * nz * 4
  undef8 = dble(undef)
  allocate(din(nx,ny,nz))
  allocate(dout(nx,ny,nz))

  if (onefile) then
    !c+++ open for read
    ifile = trim(idir)//'/'//trim(ivar)//trim(isuf) !! input file
    call rdopen(21, nlen, ofile)
    !c+++ open for write
    ofile = trim(odir)//'/'//trim(ovar)//trim(osuf) !! output file
    call wdopen(51, nlen, ofile)
    irec = 0
    ic = 0
  endif
  do iyy = nsyy, neyy
    write(ayy, '(i4)') iyy
    if (.not. onefile) then
      !c+++ open for read
      ifile = trim(idir)//'/'//trim(ivar)//'.'//trim(ayy)//trim(isuf) !! input file
      call rdopen(21, nlen, ifile)
      !c+++ open for write
      ofile = trim(odir)//'/'//trim(ovar)//'.'//trim(ayy)//trim(osuf) !! output file
      call wdopen(51, nlen, ofile)
    endif
    !c+++ start/end month
    if (iyy == nsyy) then
      nsm = nsmm
    else
      nsm = 1
    endif
    if (iyy == neyy) then
      nem = nemm
    else
      nem = nmm
    endif
    if (.not. onefile) irec = 0
    if (.not. onefile) ic = 0
    do imm = nsm, nem
      call eday(iyy, imm, ndd) !! calculate a end day of the month
      call dayloop(iyy, nsm, 1, iyy, imm, 1, ista) !! start day
      call dayloop(iyy, nsm, 1, iyy, imm, ndd, iend) !! end day
      ista = (ista - 1) * nrecparday + 1
      iend = iend * nrecparday
      nt = iend - ista + 1
      do it = ista, iend
        ic = ic + 1
        if (.not. omonth) then
          irr = it
          if (onefile) irr = ic
          !c+++ read
          call fread_r4(nx, ny, nz, irr, 21, din)
          !c+++ dout = din * fact + offset
          call shift(nx, ny, nz, undef8, din, dout, fact, offset)
          !c+++ write 
          call fwrite_r4(nx, ny, nz, irr, 51, dout)
        endif
      enddo !! it
      !c+++ monthly 
      irec = irec + 1
      if (omonth) then
        !c+++ read
        call fread_r4(nx, ny, nz, irec, 21, din)
        !c+++ dout = din * fact + offset
        call shift(nx, ny, nz, undef8, din, dout, fact, offset)
        !c+++ write 
        call fwrite_r4(nx, ny, nz, irec, 51, dout)
      endif
    enddo !! imm
    call fclose(21)
    call fclose(51)
  enddo !! iyy
  deallocate(din, dout)

  stop
end program main

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c
!c=====
subroutine getparms(nx, ny, nz, nrecparday, nsyy, nsmm, neyy, nemm, undef, &
&  idir, odir, ivar, ovar, isuf, osuf, fact, offset, onefile, omonth)
  implicit none
  !c+++ [output]
  integer(4), intent(out)       :: nx, ny, nz !! x-, y-, z-sizes
  integer(4), intent(out)       :: nrecparday !! record per day
  integer(4), intent(out)       :: nsyy, nsmm !! start year, month
  integer(4), intent(out)       :: neyy, nemm !! end year, month
  real(4), intent(out)          :: undef      !! missing value
  character(len=*), intent(out) :: idir, odir !! input/output directory name
  character(len=*), intent(out) :: ivar, ovar !! input/output variable name
  character(len=*), intent(out) :: isuf, osuf !! input/output suffix name
  real(8), intent(out)          :: fact       !! factor
  real(8), intent(out)          :: offset     !! offset
  logical, intent(out)          :: onefile    !! t: output 1 file on, f: off
  logical, intent(out)          :: omonth     !! t: monthly mean data, f: daily/6hr
  !c+++ [internal work]
  character(len=200)            :: opt, hval
  !c+++ loop variables
  integer(4)                    :: it
  !c+++ function
  integer(4)                    :: iargc
  !c+++ arguments (default values)
  nx         = 1            !! x-size
  ny         = 1            !! y-size
  nz         = 1            !! z-size
  nrecparday = 4            !! record par day
  nsyy       = 2000         !! start year
  nsmm       = 1            !! start month
  neyy       = 2000         !! end year
  nemm       = 12           !! end month
  undef      = -999.0       !! missing value
  fact       = 1.d0         !! factor
  offset     = 0.d0         !! offset
  onefile    = .false.      !! read/write 1 file
  omonth     = .false.      !! monthly mean
  idir       = '.'          !! input directory name
  odir       = '.'          !! output directory name
  ivar       = 'inp'        !! input variable name
  ovar       = 'out'        !! output variable name
  isuf       = '.grads'     !! input suffix name
  osuf       = '.grads'     !! output suffix name

  !c+++ check arguments {like C, main(int argc, char **argv)}
  if (mod(iargc(),2) /= 0) call xabort

  do it = 0, iargc()/2-1
    call getarg(it*2+1, opt)
    if (opt == '-od') then
      call getarg(it*2+2, odir)
    else if (opt == '-ov') then
      call getarg(it*2+2, ovar)
    else if (opt == '-os') then
      call getarg(it*2+2, osuf)
    else if (opt == '-id') then
      call getarg(it*2+2, idir)
    else if (opt == '-iv') then
      call getarg(it*2+2, ivar)
    else if (opt == '-is') then
      call getarg(it*2+2, isuf)
    else if (opt == '-nx') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') nx
    else if (opt == '-ny') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') ny
    else if (opt == '-nz') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') nz
    else if (opt == '-nsyy') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') nsyy
    else if (opt == '-nsmm') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') nsmm
    else if (opt == '-neyy') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') neyy
    else if (opt == '-nemm') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') nemm
    else if (opt == '-nr') then
      call getarg(it*2+2, hval)
      read(hval, '(i16)') nrecparday
    else if (opt == '-rmiss') then
      call getarg(it*2+2, hval)
      read(hval, *) undef
    else if (opt == '-fact') then
      call getarg(it*2+2, hval)
      read(hval, *) fact
    else if (opt == '-offset') then
      call getarg(it*2+2, hval)
      read(hval, *) offset
    else if (opt == '-onefile') then
      call getarg(it*2+2, hval)
      read(hval, *) onefile
    else if (opt == '-omonth') then
      call getarg(it*2+2, hval)
      read(hval, *) omonth
    else if (opt == '-h') then
      call xabort
    else
      call xabort
    endif
  enddo !! it
  write(6, '(a, i6)') 'getparms: nx = ', nx
  write(6, '(a, i6)') 'getparms: ny = ', ny
  write(6, '(a, i6)') 'getparms: nz = ', nz
  write(6, '(a, i6)') 'getparms: nrecparday = ', nrecparday
  write(6, '(a, i4.4)') 'getparms: nsyy = ', nsyy
  write(6, '(a, i2.2)') 'getparms: nsmm = ', nsmm
  write(6, '(a, i4.4)') 'getparms: neyy = ', neyy
  write(6, '(a, i2.2)') 'getparms: nemm = ', nemm
  write(6, '(a, 1pe15.7)') 'getparms: undef = ', undef
  write(6, '(2a)') 'getparms: idir = ', trim(idir)
  write(6, '(2a)') 'getparms: ivar = ', trim(ivar)
  write(6, '(2a)') 'getparms: isuf = ', trim(isuf)
  write(6, '(2a)') 'getparms: odir = ', trim(odir)
  write(6, '(2a)') 'getparms: ovar = ', trim(ovar)
  write(6, '(2a)') 'getparms: osuf = ', trim(osuf)
  write(6, '(a, 1pe15.7)') 'getparms: fact = ', fact
  write(6, '(a, 1pe15.7)') 'getparms: offset = ', offset
  write(6, '(a, l1)') 'getparms: onefile = ', onefile
  write(6, '(a, l1)') 'getparms: omonth = ', omonth

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: grdfact'
  write(6, '(a)') '-od output-directory-name'
  write(6, '(a)') '-ov output-variable-name'
  write(6, '(a)') '-os output-suffix-name'
  write(6, '(a)') '-id input-directory-name'
  write(6, '(a)') '-iv input-variable-name'
  write(6, '(a)') '-is input-suffix-name'
  write(6, '(a)') '-onefile t/f (read/write 1 file, dafault: f)'
  write(6, '(a)') '-omonth t/f (t: monthly mean data, dafault: f)'
  write(6, '(a)') ' '
  write(6, '(a)') '-fact 1.d0 -offset 0.d0'
  write(6, '(a)') ' '
  write(6, '(a)') '-nx x-size -ny y-size -nz z-size (default: 1)'
  write(6, '(a)') '-nr records-per-day (default: 4)'
  write(6, '(a)') '-rmiss missing-value (default: -999.0)'
  write(6, '(a)') ' '
  write(6, '(a)') '-nsyy start-year -nsmm start-month'
  write(6, '(a)') '-neyy end-year -nemm end-month'
  write(6, '(a)') ' '

  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c
