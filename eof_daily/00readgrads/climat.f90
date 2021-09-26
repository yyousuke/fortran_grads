!c
!c  program climat
!c  [history]
!c    2005/11/02 Yamashita
!c    2005/05/30 for EOF analysis (Yamashita)
!c  climatology for each calendar month
!c
program climat
  implicit none
  !c+++ input
  integer(4) :: nx, ny, np, nrec !! grid number and records par year
  integer(4) :: ista, iend, irecsta !! start, end record and first month
  real(4), allocatable :: DIN(:,:,:)
  !c+++ output
  real(4), allocatable :: DOUT(:,:,:)
  !c+++ work
  integer(4)  :: i, j, k, it, irec, nlen
  integer(4), allocatable :: isum(:,:,:,:)
  real(8), allocatable :: clim(:,:,:,:)
  real(8) :: undef = -999.d0

  read(5, *) nx, ny, np, nrec, ista, iend, irecsta
  nlen = nx * ny * np * 4
!ccc  write(6, *) ista, iend, irecsta
  allocate( DIN(nx,ny,np), DOUT(nx,ny,np), clim(nx,ny,np,nrec), isum(nx,ny,np,nrec) )

  open(11, file='fort.11', status='old', form='unformatted', access='direct', recl=nlen)
  open(51, file='fort.51', status='unknown', form='unformatted', access='direct', recl=nlen)

  !c+++ init
  clim(1:nx,1:ny,1:np,1:nrec) = 0.d0
!c
!c read & sum
!c===
  irec = irecsta
  do it = ista, iend
    read(11, rec=it, err=999) DIN
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          if ( DIN(i,j,k) /= undef ) then
            clim(i,j,k,irec) = clim(i,j,k,irec) + DBLE( DIN(i,j,k) )
            isum(i,j,k,irec) = isum(i,j,k,irec) + 1
          endif
        enddo !! i
      enddo !! j
    enddo !! k
    irec = irec + 1
    if (irec == (nrec + 1) ) irec = 1
  enddo !! it
!c
!c average & write
!c===
  do irec = 1, nrec
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          if ( isum(i,j,k,irec) /= 0 ) then
            DOUT(i,j,k) = clim(i,j,k,irec) / DBLE( isum(i,j,k,irec) )
          else
            DOUT(i,j,k) = undef
          endif
        enddo !! i
      enddo !! j
    enddo !! k
    write(51, rec=irec) DOUT
  enddo !! irec

  stop
  999 write(6, *) 'Error: DO NOT READ, rec =', it
end program climat
