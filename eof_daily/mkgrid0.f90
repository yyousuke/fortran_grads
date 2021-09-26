!c
!c  program mkgrid
!c  [history]
!c    2006/05/28 Yamashita
!c    2006/08/28 gintx, ginty (Yamashita)
!c    2006/09/14 input nx, ny, nz and allocate p(nz) (Yamashita)
!c  make grid information of space
!c  (when x and y grids are linear.)
!c  require; zdef
!c
!c======================================================================c
program mkgrid
  implicit none
  !c+++ input
  integer(4) :: nx, ny, nz, isz, iez
  real(4) :: nsx, nex, nsy, ney
  real(4), allocatable :: p(:)
  !c+++ work
  integer(4) :: ix, iy, iz, isx, iex, isy, iey
  real(4) :: lon, lat
  real(4) :: gintx, ginty
  read(5, *) nx, ny, nz, nsx, nex, nsy, ney, isz, iez
  gintx = 360 / DBLE(nx)
  ginty = 180 / DBLE(ny - 1)
  allocate(p(nz))
  open (1, file='zdef')
  read(1, *) p

  open (10, file='fort.10')
!c+++ longitude
  isx = (nsx + gintx) / gintx
  iex = (nex + gintx) / gintx
!c+++ latitude
  isy = (90 - nsy + ginty) / ginty
  iey = (90 - ney + ginty) / ginty
!ccc  write(11, *) isx, iex, isy, iey
!c+++ eliminate invalid input
  if (isx > iex) then
    write(6, *) 'the stating value of longitude is greater then         &
&     the ending value of that.'
    stop
  elseif (isy > iey) then
    write(6, *) 'the stating value of latitude is greater then          &
&     the ending value of that.'
    stop
  elseif (isz > iez) then
    write(6, *) 'the stating value of pressure is greater then          &
&     the ending value of that.'
    stop
  endif
  if (iex > nx.or.isx < 1) then
    write(6, *) 'invalid input longitude'
    stop
  elseif (iey > ny.or.isy < 1) then
    write(6, *) 'invalid input latitude'
    stop
  endif

!c+++ write grid id
  do iz = isz, iez
    do iy = isy, iey
      do ix = isx, iex 
        lon = gintx * (ix - 1)
        lat = 90 - ginty * (iy - 1)
!ccc    write(6, *) ix, iy, iz, lon, lat, p(iz)
        write(10, '(3(i4, 2x), 2(f8.4, 2x), f15.8)') ix, iy, iz, lon, lat, p(iz)
      enddo !! ix
    enddo !! iy
  enddo !! iz

  stop
end program mkgrid
