!c
!c  program mkgrid2
!c  [history]
!c    2006/05/28 Yamashita
!c    2006/08/28 gintx, ginty (Yamashita)
!c    2006/09/14 input nx, ny, nz and allocate p(nz) (Yamashita)
!c  make grid information of space
!c  (when x and y grids are levels.)
!c  require; xdef, ydef, zdef
!c
!c======================================================================c
program mkgrid2
  implicit none
  !c+++ input
  integer(4) :: nx, ny, nz, isz, iez
  real(4) :: nsx, nex, nsy, ney
  real(4), allocatable :: x(:), y(:), p(:)
  !c+++ work
  integer(4) :: ix, iy, iz, isx, iex, isy, iey
  real(4) :: lon, lat
  read(5, *) nx, ny, nz, nsx, nex, nsy, ney, isz, iez
  allocate(x(nx), y(ny), p(nz))
  open (1, file='zdef')
  open (2, file='ydef')
  open (3, file='xdef')
  read(1, *) p
  read(2, *) y
  read(3, *) x

  open (10, file='fort.10')
!c+++ eliminate invalid input
  if (x(1) > x(nx)) then
    write(6, *) 'the stating value of longitude must be smaller then   &
&     the ending value of that.'
    stop
  elseif (nsx > nex) then
    write(6, *) 'nsx must be smaller then nex.'
    stop
  elseif (y(1) < y(ny)) then
    write(6, *) 'the stating value of latitude must be greater then     &
&     the ending value of that.'
    stop
  elseif (nsy < ney) then
    write(6, *) 'nsy must be greater then ney.'
    stop
  elseif (p(1) < p(nz)) then
    write(6, *) 'the stating value of pressure must be greater then     &
&     the ending value of that.'
    stop
  elseif (isz > iez) then
    write(6, *) 'isz must be smaller then iez.'
    stop
  endif
  if (x(nx) > 360. .or. x(1) < 0) then
    write(6, *) 'invalid input longitude'
    stop
  elseif (abs(y(ny)) > 90. .or. abs(y(1)) > 90.) then
    write(6, *) 'invalid input latitude'
    stop
  elseif (isz < 1 .or. iez > nz) then
    write(6, *) 'invalid input level'
    stop
  endif

!c+++ write grid id
  do iz = isz, iez
    do iy = 1, ny
      if (y(iy) < ney .or. nsy < y(iy)) cycle
      do ix = 1, nx 
        if (x(ix) < nsx .or. nex < x(ix)) cycle
!ccc    write(6, *) ix, iy, iz, x(ix), y(iy), p(iz)
        write(10, '(3(i4, 2x), 2(f8.4, 2x), f15.8)') ix, iy, iz, x(ix), y(iy), p(iz)
      enddo !! ix
    enddo !! iy
  enddo !! iz

  stop
end program mkgrid2
