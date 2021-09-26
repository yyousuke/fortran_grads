!c
!c  program zmean
!c  [history]
!c    2005/11/02 Yamashita
!c    2005/05/30 for EOF analysis (Yamashita)
!c
program zmean
  implicit none
  !c+++ input
  integer(4) :: nx, ny, np, nrec !! grid number and records par year
  integer(4) :: ista, iend, irecsta !! start, end record and first month
  real(4), allocatable :: DIN(:,:,:)
  !c+++ output
  real(4), allocatable :: DOUT(:,:,:)
  !c+++ work
  integer(4)  :: j, k, it, irec
  integer(4), allocatable :: isum(:,:,:,:)
  real(8), allocatable :: clim(:,:,:,:)
  real(8) :: undef = -999.d0

  read(5, *) nx, ny, np, nrec, ista, iend, irecsta
  allocate( DIN(nx,ny,np), DOUT(1,ny,np), clim(1,ny,np,nrec), isum(1,ny,np,nrec) )
  !c+++ init
  clim(1:nx,1:ny,1:np,1:nrec) = 0.d0
!c
!c read & sum
!c===
  open(11, file='fort.11', status='old', form='unformatted', access='direct', recl=nx*ny*np*4)
  open(51, file='fort.51', status='unknown', form='unformatted', access='direct', recl=ny*np*4)
  open(52, file='fort.52', status='unknown', form='unformatted', access='direct', recl=ny*np*4)

  irec = irecsta
  do it = ista, iend
    !c+++ read
    read(11, rec=it, err=999) DIN
    !c+++ zonalmean
    call ZONALmean(nx, ny, np, undef, DIN, DOUT)
    !c+++ write
    write(51, rec=it) DOUT
    !c+++ summation
    do k = 1, np
      do j = 1, ny
        if ( DOUT(1,j,k) /= undef ) then
          clim(1,j,k,irec) = clim(1,j,k,irec) + DBLE( DOUT(1,j,k) )
          isum(1,j,k,irec) = isum(1,j,k,irec) + 1
        endif
      enddo !! j
    enddo !! k
    irec = irec + 1
    if (irec == (nrec + 1) ) irec = 1
  enddo !! it
!c
!c average
!c===
  do irec = 1, nrec
    do k = 1, np
      do j = 1, ny
        if ( isum(1,j,k,irec) /= 0 ) then
          DOUT(1,j,k) = clim(1,j,k,irec) / DBLE( isum(1,j,k,irec) )
        else
          DOUT(1,j,k) = undef
        endif
      enddo !! j
    enddo !! k
    write(52, rec=irec) DOUT
  enddo !! irec

  stop
  999 write(6, *) 'Error: DO NOT READ, rec =', it
end program zmean
!c----------------------------------------------------------------------c
subroutine ZONALmean(nx, ny, np, undef, data, zonal)
  !c+++ input
  integer(4), intent(in) :: nx, ny, np
  real(4), intent(in) :: data(nx,ny,np)
  real(8), intent(in) :: undef
  !c+++ output
  real(4), intent(out) :: zonal(ny,np)
  !c+++ work
  integer(4) :: i, j, k, iflag

  do k = 1, np
    do j = 1, ny
      zonal(j,k) = 0.0
      iflag = 0
      do i = 1, nx, 2
        if(data(  i,j,k) /= undef) then
          zonal(j,k) = zonal(j,k) + DBLE( data(  i,j,k) )
          iflag = iflag + 1 
        endif
        if(data(i+1,j,k) /= undef) then
          zonal(j,k) = zonal(j,k) + DBLE( data(i+1,j,k) )
          iflag = iflag + 1 
        endif
      enddo !! i
      if ( MOD(nx,2) == 1 ) then
        if (data(nx,j,k) /= undef) then
          zonal(j,k) = zonal(j,k) + DBLE( data(nx,j,k) )
          iflag = iflag + 1 
        endif
      endif
      if (iflag /= 0) then
        zonal(j,k) = zonal(j,k) / DBLE(iflag)
      else
        zonal(j,k) = undef
      endif
    enddo !! j
  enddo !! k

  return
end subroutine ZONALmean
