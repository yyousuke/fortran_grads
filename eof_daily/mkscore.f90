!c
!c  program mkscore
!c  [history]
!c  2003/08/05 Yamashita: 04e-ev2score.csh
!c  2021/06/07 Yamashita: mkscore.f90 from 04e-ev2score.csh
!c  2021/06/07 merge ready, bstat of average.f90
!c  2021/06/07 merge avesd of 05EOF-stat.csh
!c 
!c  input: eigenvalue.dat
!c  output: info-PCA.dat
!c
!c======================================================================c
program mkscore
  use common_args,  only: nx, ny, nz, nmm, nv, nc, npc, weight, opt_clim
  implicit none
  !c+++ [input]
  !c+++ from <stdin>
  logical :: opt_neg(npc)                        !! negative sign
  !c+++ input from file
  !c+++ grid id
  integer(4), allocatable :: jc(:)               !! grid id of time grid point
  integer(4), allocatable :: jyy(:), jmm(:)      !!   those year, month values
  integer(4), allocatable :: jx(:), jy(:), jz(:) !! grid id of spatial grid point
  real(4), allocatable    :: rc(:)               !! time for each time grid point
  real(4), allocatable    :: rx(:), ry(:), rz(:) !! lon., lat. and pres. level
  !c+++ data
  integer(4)              :: jpc                 !! PC number 
  real(8), allocatable    :: VE(:,:)             !! eigen vector
  real(8), allocatable    :: X(:,:), AVE(:)      !! data-x, average
  !c+++ [output]
  real(8), allocatable    :: score(:,:)          !! score time-series
  real(8)                 :: nscore              !! normalized score
  !c+++ [work]
  integer(4)              :: ic, iv, ipc         !! loop variable
  real(8)                 :: SCOR                !! score
  real(8)                 :: aves, sds           !! ave, var, sd of score
  real(8)                 :: neg                 !! 1 or -1

  !c+++ input
  read(5, *) opt_neg
  !c+++ allocate
  allocate(jc(nc), jyy(nc), jmm(nc), jx(nv), jy(nv), jz(nv))
  allocate(rc(nc), rx(nv), ry(nv), rz(nv))
  allocate(VE(nv,nv), X(0:nc,0:nv), AVE(nv), score(nc,npc))
  !c+++ read input data
  call ready(nx, ny, nz, nc, nv, nmm, weight, jc, jyy, jmm, jx, jy, jz, rc, rx, ry, rz, X)
  !c+++ basic statistics
  call bstat(nc, nv, X, ave)
  !c+++ open output file
  open(51, file='fort.51', status='unknown') !! EOF-score
  open(61, file='fort.61', status='unknown') !! EOF-vector
  open(71, file='fort.71', status='unknown') !! EOF-norm_score

  !c +++ read VE(iv,ipc)
  open(22, file='fort.22')
  do ipc = 1, npc
    neg = 1.d0
    if (opt_neg(ipc)) neg = - neg
    do iv = 1, nv
      !c+++ read
      read(22, *) jpc, VE(iv,ipc)
      VE(iv,ipc) = neg * VE(iv,ipc)
      !c+++ write vector
      write(61, '(i6, 2x, 2(f8.4, 2x), f15.8, 2x, 1pe16.6)') ipc, &
        rx(iv), ry(iv), rz(iv), VE(iv,ipc)
    enddo !! iv
  enddo !! ipc

  !c+++ LP = max. number of the output principal component
  do ipc = 1, npc
    SCOR = 0.0
    do iv = 1, nv
      SCOR = SCOR + AVE(iv) * VE(iv,ipc)
    enddo !! iv
    X(0,ipc) = SCOR !! in total
  enddo !! ipc

  !c+++ calc. component score in each time(ic=1,nc)
  do ic = 1, nc
    do ipc = 1, npc
      SCOR = -X(0,ipc)
      !c+++ summention of all points(calc. "variance anomaly")
      do iv = 1, nv
        SCOR = SCOR + X(ic,iv) * VE(iv,ipc)
      enddo !! iv
      score(ic,ipc) = SCOR
    enddo !! ipc
  enddo !! ic

  write(6, *) 'CHECK score timeseries'
  do ipc = 1, npc
    !c+++ calc. average/standarized deviation of score
    call avesd(nc, score(:,ipc), aves, sds)
    do ic = 1, nc
      !c+++ calc. normalized score
      nscore = (score(ic,ipc) - aves) / sds
      !c+++ write score
      write(51, '(i3, 2x, i6, 2x, f15.8, 2x, 2(i6, 2x), 1pe16.6)') &
        ipc, jc(ic), rc(ic), jyy(ic), jmm(ic), score(ic,ipc)
      !c+++ write normalized score
      write(71, '(i3, 2x, i6, 2x, f15.8, 2(2x, i6), 2(2x, 1pe16.6))') &
        ipc, jc(ic), rc(ic), jyy(ic), jmm(ic), score(ic,ipc), nscore
      if (ipc == 1.and.jmm(ic) == 1) then
        write(6, '(i3, 2x, i6, 2x, f15.8, 2(2x, i6), 2(2x, 1pe16.6))') &
          ipc, jc(ic), rc(ic), jyy(ic), jmm(ic), score(ic,ipc), nscore
      endif
    enddo !! ic
  enddo !! ipc

  stop

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c
!c  subroutine ready
!c  [history]
!c  2003/08/14 Yamashita
!c  2006/05/16 read rx, ry
!c  2006/05/30 f77 ==> f90
!c  2021/06/07 merge into mkscore.f90
!c
!c=====
  subroutine ready(nx, ny, nz, nc, nv, nmm, weight, jc, jyy, jmm, jx, jy, jz, rc, rx, ry, rz, X)
  !c+++ [input]
  integer(4), intent(in)  :: nx, ny, nz             !! num. of x-, y-, z-grids
  integer(4), intent(in)  :: nc, nv                 !! num. of time & spatial grids
  integer(4), intent(in)  :: nmm                    !! number of months
  integer(4), intent(in)  :: weight                 !! vertical weight
  !c+++ [output]
  !c+++ grid id for time and spatial grid point
  integer(4), intent(out) :: jc(nc)                 !! grid id of time grid point
  integer(4), intent(out) :: jyy(nc), jmm(nc)       !!   those year, month values
  integer(4), intent(out) :: jx(nv), jy(nv), jz(nv) !! grid id of spatial grid point
  real(4), intent(out)    :: rc(nc)                 !! time for each time grid point
  real(4), intent(out)    :: rx(nv), ry(nv), rz(nv) !! lon., lat. and pres. level
  real(8), intent(out)    :: X(0:nc,0:nv)           !! anom. data
  !c+++ [internal work]
  integer(4)              :: iv                     !! loop variables (spatial)
  integer(4)              :: ic, imm                !! loop variables (time)
  integer(4)              :: nlen                   !! data length for read
  integer(4)              :: mx, my, mz, mc         !! selected grid id
  real(4)                 :: indat(nx,ny,nz)        !! original data 
  real(4)                 :: clim(nx,ny,nz,nmm)     !! climatology
  real(8)                 :: pi                     !! pi
  pi = 4.d0 * datan(1.d0)
  nlen = nx * ny * nz * 4

  !c+++ grid points
  open (10, file='fort.10', form='formatted', access='sequential') !! time
  open (11, file='fort.11', form='formatted', access='sequential') !! spatial
  !c+++ climatology
  if (opt_clim) then
    open (12, file='fort.12', form='unformatted', access='direct', recl=nlen)
  endif
  !c+++ open input data
  open (21, file='fort.21', form='unformatted', access='direct', recl=nlen)

  !c+++ read time grid points
  do ic = 1, nc
    read(10, *) jc(ic), rc(ic), jyy(ic), jmm(ic)
  enddo !! ic

  !c+++ read spatial grid points
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
        X(ic,iv) = sqrt(cos(ry(iv) / 180.d0 * pi)) &
              * sqrt(rz(iv)) &
              * dble( indat(mx,my,mz) - clim(mx,my,mz,jmm(ic)) )
      enddo !! iv
    else
      do iv = 1, nv
        mx = jx(iv)
        my = jy(iv)
        mz = jz(iv)
        X(ic,iv) = sqrt(cos(ry(iv) / 180.d0 * pi)) &
                 * dble( indat(mx,my,mz) - clim(mx,my,mz,jmm(ic)) )
      enddo !! iv
    endif
  enddo !! ic
  !c+++ close file
  close(21)

  return
end subroutine ready

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c
!c  subroutine bstat
!c  [history]
!c  2003/08/14 Yamashita
!c  2006/05/30 f77 ==> f90
!c  2021/06/07 merge into mkscore.f90
!c  Compute basic statistics for multivariate data.
!c
!c=====
subroutine bstat(nc, nv, X, ave)
  !c+++ [input]
  integer(4), intent(in) :: nc, nv        !! number of time, spatial grids
  real(8), intent(in)    :: X(0:nc,0:nv)  !! data
  !c+++ [output]
  real(8), intent(out)   :: ave(nv)       !! average
  !c+++ [internal work]
  integer(4)             :: ic, iv        !! loop variables
  real(8)                :: WI, WX(nv)    !!
  !c+++ initialize
  WX(1:nv) = X(1,1:nv) !! X(1,iv)=reference value of anomaly data
  ave(1:nv) = 0.d0

  !c+++ pre-computation for making matrix
  write(6, *) 'pre-computation for making matrix (in bstat)'
  do ic = 2, nc !! timeseries loop
    do iv = 1, nv !! grid number loop
      WI = X(ic,iv) - WX(iv)
      ave(iv) = ave(iv) + WI
    enddo !! iv
  enddo !! ic
  ave(1:nv) = ave(1:nv) / dble(nc) + WX(1:nv)

  return
end subroutine bstat

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c
!c  subroutine avesd
!c  average and standard deviation
!c
!c=====
subroutine avesd(nc, d, ave, sdev)
  !c+++ [input]
  integer(4), intent(in) :: nc            !! number of time
  real(8), intent(in)    :: d(nc)         !! data
  !c+++ [output]
  real(8), intent(out)   :: ave, sdev     !! average, standard deviation
  !c+++ [internal work]
  integer(4)             :: ic            !! loop variables
  real(8)                :: var           !! variance
  !c+++ initialize
  ave = 0.d0
  var = 0.d0
  do ic = 1, nc
    ave = ave + d(ic) 
    var = var + d(ic)**2
  enddo !! ic
  ave = ave / dble(nc)
  var = var / dble(nc) - ave**2
  sdev = sqrt(var)
  write(6, *) 'CHK(ave,var,sd)', ave, var, sdev

  return
end subroutine avesd

!c---------------------------------------------------------------------c

end program mkscore
