!c  
!c  grd6hr2day.f90
!c  2017/02/08 first ver. (Yamashita)
!c
!c  calculate daily mean data from 6hr data
!c  input: 4times daily data, output: daily mean data
!c 
!c======================================================================c 
program main
  use calendar, only: eday, dayloop
  use rw, only: fclose, fread_r4, fwrite_r4
  use io, only: rdopen, wdopen, shift_xgrids, conv_yrev, conv_zrev
  use tave, only: TIMEave
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
  integer(4)           :: xshift     !! shift x-grids (default 0)
  logical              :: oyrev      !! t: y-rev. on, f: off
  logical              :: ozrev      !! t: z-rev. on, f: off
  !c+++ from file
  real(4), allocatable :: din(:,:,:,:)
  !c+++ [output]
  real(4), allocatable :: dout(:,:,:)
  !c+++ [work]
  integer(4)           :: iyy, imm, it
  integer(4)           :: nlen, ista, iend, iflag, irec
  integer(4)           :: nsm, nem, ndd, nt
  character(10)        :: ayy
  character(400)       :: ifile      !! input file
  character(400)       :: ofile      !! output file
  real(8)              :: undef8     !! missing value

  !c+++ read parameters
  call getparms
 
  nlen = nx * ny * nz * 4
  undef8 = dble(undef)
  allocate(din(nx,ny,nz,nrecparday))
  allocate(dout(nx,ny,nz))

  do iyy = nsyy, neyy
    write(ayy, '(i4)') iyy
    !c+++ open for read
    ifile = trim(idir)//'/'//trim(ivar)//'.'//trim(ayy)//trim(isuf) !! input file
    call rdopen(21, nlen, ifile)
    !c+++ open for write
    ofile = trim(odir)//'/'//trim(ovar)//'.'//trim(ayy)//trim(osuf) !! output file
    call wdopen(51, nlen, ofile)
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
    irec = 0
    do imm = nsm, nem
      call eday(iyy, imm, ndd) !! calculate a end day of the month
      call dayloop(iyy, nsm, 1, iyy, imm, 1, ista) !! start day
      call dayloop(iyy, nsm, 1, iyy, imm, ndd, iend) !! end day
      ista = (ista - 1) * nrecparday + 1
      iend = iend * nrecparday
      nt = iend - ista + 1
      iflag = 0
      do it = ista, iend
        iflag = iflag + 1
        !c+++ read
        call fread_r4(nx, ny, nz, it, 21, din(1:nx,1:ny,1:nz,iflag))
        if (iflag == nrecparday) then
          irec = irec + 1
          !c+++ calc daily mean data
          call TIMEave(nx, ny, nz, nrecparday, undef8, din, dout)
          !c+++ shift x-grids
          if (xshift /= 0) then
            call shift_xgrids(nx, ny, nz, xshift, dout)
          endif
          !c+++ reverse y-grids
          if (oyrev) then
            call conv_yrev(nx, ny, nz, dout)
          endif
          !c+++ reverse z-grids
          if (ozrev) then
            call conv_zrev(nx, ny, nz, dout)
          endif

          !c+++ write 
          call fwrite_r4(nx, ny, nz, irec, 51, dout)
          iflag = 0
        endif
      enddo !! it
    enddo !! imm
    call fclose(21)
    call fclose(51)
  enddo !! iyy
  deallocate(din, dout)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c=====
subroutine getparms
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
  xshift     = 0            !! shift x-grids (default 0)
  oyrev      = .false.      !! reverse y-grids
  ozrev      = .false.      !! reverse z-grids
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
    else if (opt == '-xshift') then
      call getarg(it*2+2, hval)
      read(hval, *) xshift
    else if (opt == '-yrev') then
      call getarg(it*2+2, hval)
      read(hval, *) oyrev
    else if (opt == '-zrev') then
      call getarg(it*2+2, hval)
      read(hval, *) ozrev
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
  write(6, '(a, i6)') 'getparms: xshift = ', xshift
  write(6, '(a, l1)') 'getparms: oyrev = ', oyrev
  write(6, '(a, l1)') 'getparms: ozrev = ', ozrev
  write(6, '(2a)') 'getparms: idir = ', trim(idir)
  write(6, '(2a)') 'getparms: ivar = ', trim(ivar)
  write(6, '(2a)') 'getparms: isuf = ', trim(isuf)
  write(6, '(2a)') 'getparms: odir = ', trim(odir)
  write(6, '(2a)') 'getparms: ovar = ', trim(ovar)
  write(6, '(2a)') 'getparms: osuf = ', trim(osuf)

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: grd6hr2day'
  write(6, '(a)') '-od output-directory-name'
  write(6, '(a)') '-ov output-variable-name'
  write(6, '(a)') '-os output-suffix-name'
  write(6, '(a)') '-id input-directory-name'
  write(6, '(a)') '-iv input-variable-name'
  write(6, '(a)') '-is input-suffix-name'
  write(6, '(a)') ' '
  write(6, '(a)') '-nx x-size -ny y-size -nz z-size (default: 1)'
  write(6, '(a)') '-nr records-per-day (default: 4)'
  write(6, '(a)') '-rmiss missing-value (default: -999.0)'
  write(6, '(a)') ' '
  write(6, '(a)') '-nsyy start-year -nsmm start-month'
  write(6, '(a)') '-neyy end-year -nemm end-month'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-yrev f/t -zrev f/t (reverse y-, z-grids, dafault: f)'
  write(6, '(a)') '-xshift x_shift (shift x-grids, default: 0)'
  write(6, '(a)') '------------------------------------------'

  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program main
