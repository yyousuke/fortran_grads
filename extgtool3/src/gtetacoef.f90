!c  program gtetacoef
!c  [history]
!c  2025/01/21 Yamashita: first ver. from gtinfo.f90
!c  
!c  read GTOOL3 etacoef file and display coefs.
!c
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module common_typedef
!c  external: module rw
!c            subroutines fopen2, fclose
!c  external: module rwgtool
!c            subroutines seek_iounit, get_zaxsize, get_etacoef
!c  external: error_handler
!c            subroutine ioerror
!c
!c=====================================================================c
program gtetacoef
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use rw, only: fopen2, fclose
  use rwgtool, only: seek_iounit, get_zaxsize, get_etacoef
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from file (rgthd)
  real(kind=r8b), allocatable :: eta_fa(:) !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:) !! for eta half lev.
  !c+++ from get_zaxsize
  integer(kind=i4b)           :: kmax      !! z-sizes
  !c+++ input from getparms
  character(len=ncc)          :: haxisz    !! z-axis name (for eta-levels)
  character(len=nfiln)        :: ofile     !! output file name
  !c+++ [work]
  integer(kind=i4b)           :: k         !! loop variable
  integer(kind=i4b)           :: nch       !! loc. of character
  integer(kind=i4b)           :: ios       !! end code
  integer(kind=i4b)           :: jfile_o   !! I/O unit for output file

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ get I/O unit numbers for output file
  call seek_iounit(jfile_o, ios)
  if (ios /= 0) call ioerror(jfile_o, ios)
  !c+++ open output text file
  call fopen2(jfile_o, trim(ofile), 'unknown', 2, 0, ios)
  if (ios /= 0) call ioerror(jfile_o, ios)

!c
!c main
!c===
  !c+++ set z-axis-size
  write(6, *) 'haxisz = ', haxisz
  call get_zaxsize(haxisz, kmax)
  !c+++ allocate
  allocate(eta_fa(kmax), eta_fb(kmax))
  !c+++ read z-axis file (for eta-lev)
  call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  !c+++ modify specific axis-files
  if (haxisz(1:4) == 'CETA') then
    nch = scan(haxisz, '.')
    if (nch /= 0.and.haxisz(nch+1:nch+1) == 'M') then
      eta_fa(:) = eta_fa(:) * 1d-2
    endif
  endif

  !c+++ write
  write(jfile_o, '(2a)') 'axis_name = ', trim(haxisz)
  write(jfile_o, *) 'kmax = ', kmax
  write(jfile_o, *) 'eta_fa = '
  do k = 1, kmax
    write(jfile_o, *) eta_fa(k)
  enddo
  write(jfile_o, *) 'eta_fb = '
  do k = 1, kmax
    write(jfile_o, *) eta_fb(k)
  enddo

  !c+++ deallocate
  deallocate(eta_fa, eta_fb)
  !c+++ close files
  call fclose(jfile_o)

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
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('az', 'NUMBER1000', haxisz, ios) !! z-axis name
  call get_parms('o', 'gtetacoef.txt', ofile, ios)

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtetacoef -az eta-axis-name -o output-file'
  stop 2
end subroutine xabort

!c=====================================================================c

end program gtetacoef
