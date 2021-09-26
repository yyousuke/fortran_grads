!c
!c  program mkdata
!c  [history]
!c  2003/08/05 Yamashita: 01mkdata-x.csh
!c  2021/06/07 Yamashita: f90 ver. from 01mkdata-x.csh
!c
!c======================================================================c
program mkdata
  use common_args,  only: nx, ny, nz, nmm, nv, nc, nsyy, neyy, nsmm, nemm, weight, opt_clim
  implicit none
  !c+++ [input]
  !c+++ grid id
  integer(4), allocatable :: jc(:)               !! grid id of time grid point
  integer(4), allocatable :: jyy(:), jmm(:)      !!   those year, month values
  integer(4), allocatable :: jx(:), jy(:), jz(:) !! grid id of spatial grid point
  real(4), allocatable    :: rc(:)               !! time for each time grid point
  real(4), allocatable    :: rx(:), ry(:), rz(:) !! lon., lat. and pres. level
                                                 !!   for each spatial grid point
  !c+++ data
  real(4), allocatable    :: indat(:,:,:)        !! original data 
  real(4), allocatable    :: clim(:,:,:,:)       !! climatology
  !c+++ [output]
  real(8), allocatable    :: X(:)                !! anomary data 
                                                 !!   for each spatial grid point
  !c+++ [work]
  integer(4)              :: ix, iy, iz, iv      !! loop variables (spatial)
  integer(4)              :: ic, imm             !! loop variables (time)
  integer(4)              :: nlen                !! data length for read
  integer(4)              :: mx, my, mz, mc      !! selected grid id
  real(8)                 :: pi                  !! pi
  pi = 4.d0 * datan(1.d0)
  nlen = nx * ny * nz * 4

  !c+++ allocate
  allocate(jc(nc), jyy(nc), jmm(nc), jx(nv), jy(nv), jz(nv))
  allocate(rc(nc), rx(nv), ry(nv), rz(nv))
  allocate(indat(nx,ny,nz), clim(nx,ny,nz,nmm), X(nv))
  !c+++ open input data
  open (21, file='fort.21', form='unformatted', access='direct', recl=nlen)
  !c+++ open input climatology
  if (opt_clim) then
    open (12, file='fort.12', form='unformatted', access='direct', recl=nlen)
  endif
  !c+++ open output X
  open (51, file='fort.51', form='unformatted', access='direct', recl=nv*8)

  !c+++ read time grid points
   do ic = 1, nc
     read(10, *) jc(ic), rc(ic), jyy(ic), jmm(ic)
   enddo !! ic

   !c +++ read spatial grid points
   do iv = 1, nv
     read(11, *) jx(iv), jy(iv), jz(iv), rx(iv), ry(iv), rz(iv)
   enddo !! iv

  !c+++ read climatology
  if (opt_clim) then
    do imm = 1, nmm
      read(12, rec=imm) clim(1:nx,1:ny,1:nz,imm)
    enddo !! imm
  else
    clim(1:nx,1:ny,1:nz,1:nmm) = 0.d0
  endif

  !c+++ close files
  close(10)
  close(11)
  if (opt_clim) close(12)

  do ic = 1, nc
    mc = jc(ic)
    write(6, *) 'rec = ', mc,', time = ', jyy(ic), jmm(ic)
    !c+++ read original data
    read(21, rec=mc) indat(:,:,:)
    if (weight == 1) then
      do iv = 1, nv
        mx = jx(iv)
        my = jy(iv)
        mz = jz(iv)
        X(iv) = sqrt(cos(ry(iv) / 180.d0 * pi))                         &
&             * sqrt(rz(iv))                                            &
&             * dble( indat(mx,my,mz) - clim(mx,my,mz,jmm(ic)) )
      enddo !! iv
    else
      do iv = 1, nv
        mx = jx(iv)
        my = jy(iv)
        mz = jz(iv)
        X(iv) = sqrt(cos(ry(iv) / 180.d0 * pi))                         &
&             * dble( indat(mx,my,mz) - clim(mx,my,mz,jmm(ic)) )
      enddo !! iv
    endif
    !c+++ write anomaly
    write(51, rec=ic) X
  enddo !! ic
  !c+++ close files
  close(21)
  close(51)

  stop
end program mkdata
