!c
!c  program vmean
!c  [history]
!c  2005/11/02 Yamashita
!c  2005/05/30 Yamashita: for EOF analysis
!c  2021/06/07 Yamashita: vertical mean & U, V ==> Zeta
!c  climatology for each calendar month
!c
!c=====================================================================c
program vmean
  implicit none
  !c+++ [parameter]
  real(8), parameter:: ar = 6.370d6 !! equatorial radius (m)
  real(8), parameter :: undef = -999.d0
  !c+++ [input]
  integer(4) :: nx, ny, np, nrec !! grid number and records
  integer(4) :: zmin, zmax !! min/max of vertical grid number
  real(4), allocatable :: u(:,:,:) !! zonal wind
  real(4), allocatable :: v(:,:,:) !! meridional wind
  real(4), allocatable :: din(:,:,:)
  !c+++ [output]
  real(4), allocatable :: dout(:,:)
  !c+++ [work]
  integer(4)  :: i, j, k, it, nlen, nleno
  integer(4), allocatable :: isum(:,:)
  real(8), allocatable :: yy(:), cs(:)
  real(8), allocatable :: work(:,:)
  real(8) :: pi
  pi = acos(-1.d0)

  !c+++ from <stdin>
  read(5, *) nx, ny, np, nrec, zmin, zmax
  nlen = nx * ny * np * 4
  nleno = nx * ny * 4
  !c+++ allocate
  allocate(u(nx,ny,np), v(nx,ny,np), din(nx,ny,np))
  allocate(dout(nx,ny), work(nx,ny), isum(nx,ny), yy(ny), cs(ny))
  !c+++ open
  open(11, file='fort.11', status='old', form='unformatted', access='direct', recl=nlen)
  open(12, file='fort.12', status='old', form='unformatted', access='direct', recl=nlen)
  open(51, file='fort.51', status='unknown', form='unformatted', access='direct', recl=nleno)

  !c+++ y-grids
  open(10, file='../ydef', status='old')
  read(10, *) yy(:)
  close(10)
  do j = 1, ny
    cs(j) = cos(yy(j) * pi / 180.d0)
  enddo !! j
  !c+++ init
  work(1:nx,1:ny) = 0.d0
!c
!c main
!c===
  do it = 1, nrec
    !c+++ read
    read(11, rec=it, err=9) u
    read(12, rec=it, err=9) v
    !c+++ U, V ==> Zeta
    call vortxy(nx, ny, np, undef, u, v, din, yy, ar, cs)
    !c+++ sum
    do k = zmin, zmax
      do j = 1, ny
        do i = 1, nx
          if (din(i,j,k) /= undef) then
            work(i,j) = work(i,j) + dble(din(i,j,k))
            isum(i,j) = isum(i,j) + 1
          endif
        enddo !! i
      enddo !! j
    enddo !! k
    !c+++ mean
    do j = 1, ny
      do i = 1, nx
        if (isum(i,j) /= 0) then
          dout(i,j) = work(i,j) / dble(isum(i,j))
        else
          dout(i,j) = undef
        endif
      enddo !! i
    enddo !! j
    !c+++ write
    write(51, rec=it) dout
  enddo !! it
  !c+++ close
  close(11)
  close(51)
  !c+++ deallocate
  deallocate(u, v, din, dout, work, isum, yy, cs)

  stop
9 write(6, *) 'Error: DO NOT READ, rec =', it

contains 

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine:  vortxy
!c  INPUT
!c      real(4) data1(nx,ny,np) ; data on x-axis
!c              data2(nx,ny,np) ; data on y-axis
!c  OUTPUT
!c      real(4) vort(nx,ny,np)  ;  d (data2)/dx - d (data1)/dy
!c  WORK
!c      real(4) WORK(nx,ny,nz)
!c=====
subroutine vortxy(nx, ny, np, undef, data1, data2, vort, YY, AR, CS)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np
  real(4), intent(in) :: data1(nx,ny,np), data2(nx,ny,np)
  real(8), intent(in) :: YY(ny), AR, CS(ny), undef
  !c+++ [output]
  real(4), intent(out) :: vort(nx,ny,np)
  !c+++ [internal work]
  real(4) :: work(nx,ny,np)

  !c+++ d (data2) / dx
  call diffxf(nx, ny, np, undef, data2, vort, AR, CS)
  !c+++ d (data1) / dy
  call diffyf(nx, ny, np, undef, data1, work, YY, AR, CS)
  !c+++  vorticity = d (data2) / dx - d (data1) / dy
  call sub(nx, ny, np, undef, vort, work, vort, 1.d0, 1.d0, 0.d0, 0.d0)

  return
end subroutine vortxy

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxf
!c  INPUT
!c      real(4) data(nx,ny,np) ; data
!c  OUTPUT
!c      real(4) diff(nx,ny,np) ; d (data) / dx   (dx=AR*cos(lat)*DX)
!c=====
subroutine diffxf(nx, ny, np, undef, data, diff, AR, CS)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np
  real(4), intent(in) :: data(nx,ny,np)
  real(8), intent(in) :: CS(ny), AR, undef
  !c+++ [output]
  real(4), intent(out) :: diff(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: i, j, k, i1, i2
  real(8) :: D, DX, DDX, PI
  PI = 4.d0 * DATAN(1.d0) 
!c
!c d / dx
!c===
  DX = 2.d0 * PI / nx
  do k = 1, np
    do j = 1, ny
      if (j == 1.or.j == ny) then
        DDX = 0.d0
      else
        DDX = 1.d0 / (2.d0 * AR * CS(j) * DX)
      endif
      do i = 1, nx
        i1 = i - 1
        i2 = i + 1
        if (i == 1) i1 = nx
        if (i == nx) i2 = 1
        if (data(i1,j,k) /= undef.and.data(i2,j,k) /= undef) then
          D = DBLE(data(i2,j,k)) - data(i1,j,k)
          diff(i,j,k) = D * DDX
        else
          diff(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine diffxf

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffyf
!c  INPUT
!c      real(4) data(nx,ny,np) ; data
!c  OUTPUT
!c      real(4) diff(nx,ny,np) ; d (cos(lat) * data) / dy   
!c                                              (dy=AR*cos(lat)*DY)
!c                                              (DY=lat2-lat1 [rad])
!c=====
subroutine diffyf(nx, ny, np, undef, data, diff, YY, AR, CS)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np
  real(4), intent(in) :: data(nx,ny,np)
  real(8), intent(in) :: YY(ny), AR, CS(ny), undef
  !c+++ [output]
  real(4), intent(out) :: diff(nx,ny,np)
  !c+++ [internal work]
  integer(4) :: i, j, k, j1, j2, jc
  real(8) :: DDY, PI
  real(8) :: spole, npole
  PI = 4.d0 * DATAN(1.d0) 
  if (ny == 1) then
    diff(1:nx,1:ny,1:np) = data(1:nx,1:ny,1:np)
    return
  endif
!c
!c d / dy
!c===
  do k = 1, np
    !c+++ NP
    j1 = 1
    j2 = 2
    npole = 0.d0
    do i = 1, nx
      if (data(i,j2,k) /= undef.and.data(i,j1,k) /= undef) then
        npole = npole + data(i,j2,k) + (data(i,j1,k) - data(i,j2,k)) &
&         * (90.d0 - YY(j2)) / (YY(j1) - YY(j2))
      else
        npole = undef
        exit
      endif
    enddo !! i 
    npole = npole / dble(nx) 
    !c+++ SP
    j1 = ny - 1
    j2 = ny
    spole = 0.d0
    do i = 1, nx
      if (data(i,j2,k) /= undef.and.data(i,j1,k) /= undef) then
        spole = spole + data(i,j1,k) + (data(i,j2,k) - data(i,j1,k)) &
&         * (-90.d0 - YY(j1)) / (YY(j2) - YY(j1))
      else
        spole = undef
        exit
      endif
    enddo !! i 
    spole = spole / dble(nx) 

    !c+++ global
    do j = 1, ny
      j1 = j - 1
      j2 = j + 1
      jc = j
      if (j == 1)  j1 = 1
      !ccc if (j == 1)  jc = 2
      if (j == ny) j2 = ny
      !ccc if (j == ny) jc = ny - 1
      DDY = AR * CS(jc) * PI * (YY(j2) - YY(j1)) / 180.d0
      do i = 1, nx
        if (data(i,j2,k) /= undef.and.data(i,j1,k) /= undef) then
          diff(i,j,k) = (CS(j2) * data(i,j2,k) - CS(j1) * data(i,j1,k)) / DDY
          !c+++ NP
          if (j == 1) then
            if (npole /= undef) then
              DDY = AR * PI * (90.d0 - YY(j2)) / 180.d0
              diff(i,j,k) = (npole - dble(data(i,j2,k))) / DDY
              !ccc DDY = AR * CS(jc) * PI * (90.d0 - YY(j2)) / 180.d0
              !ccc diff(i,j,k) = CS(jc) * (npole - dble(data(i,j2,k))) / DDY
            else
              diff(i,j,k) = undef
            endif
          endif
          !c+++ SP
          if (j == ny) then
            if (spole /= undef) then
              DDY = AR * PI * (-90.d0 - YY(j1)) / 180.d0
              diff(i,j,k) = (spole - dble(data(i,j1,k))) / DDY
              !ccc DDY = AR * CS(jc) * PI * (-90.d0 - YY(j1)) / 180.d0
              !ccc diff(i,j,k) = CS(jc) * (spole(i) - dble(data(i,j1,k))) / DDY
            else
              diff(i,j,k) = undef
            endif
          endif
          !ccc if (j == 1.or.j == ny) then
          !ccc  diff(i,j,k) = CS(jc) * (data(i,j2,k) - DBLE(data(i,j1,k))) / DDY
          !ccc  !ccc diff(i,j,k) = 0.d0
          !ccc endif
        else
          diff(i,j,k) = undef
        endif
      enddo !! i 
    enddo !! j
  enddo !! k

  return
end subroutine diffyf

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  sub
!c  INPUT 
!c      real*4 data1(nx,ny,np)
!c      real*4 data2(nx,ny,np)
!c  OUTPUT 
!c      real*4 dout(nx,ny,np); (data1*fact1 + ofs1) - (data2*fact2 + ofs2)
!c=====
subroutine sub(nx, ny, np, undef, data1, data2, dout, fact1, fact2, ofs1, ofs2)
  !c+++ [input]
  integer(4), intent(in) :: nx, ny, np      !!
  real(8), intent(in)    :: undef           !!
  real(4), intent(in)    :: data1(nx,ny,np) !!
  real(4), intent(in)    :: data2(nx,ny,np) !!
  real(8), intent(in)    :: fact1, fact2    !!
  real(8), intent(in)    :: ofs1, ofs2      !!
  !c+++ [output]
  real(4), intent(out)   :: dout(nx,ny,np)  !!
  !c+++ [internal work]
  integer(4)             ::  i, j, k        !!
!c
!c data1 - data2
!c===
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        if (data1(i,j,k) /= undef.and.data2(i,j,k) /= undef) then
          dout(i,j,k) = (data1(i,j,k) * fact1 + ofs1)                    &
&                     - (data2(i,j,k) * fact2 + ofs2)
        else
          dout(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine sub

!c---------------------------------------------------------------------c

end program vmean
