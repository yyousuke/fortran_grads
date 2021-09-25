program main
  implicit none
  integer(4), parameter :: nx = 288, ny = 145, nz = 23, nt = 372
  integer(4) :: i, j, k, it
  real(4) :: uin(nx,ny,nz), uout(nx,ny,nz)
  open(10, file='uwnd.mon.mean.grads', status='old', access='direct', form='unformatted', recl=nx*ny*nz*4)
  open(51, file='uwnd_lit.mon.mean.grads', status='unknown', access='direct', form='unformatted', recl=nx*ny*nz*4)

  do it = 1, nt
    read(10, rec=it) uin
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          call convend(uout(i,j,k), uin(i,j,k), 4)
        enddo !! i
      enddo !! j
    enddo !! k
    write(51, rec=it) uout
  enddo !! it

  stop
end program main
