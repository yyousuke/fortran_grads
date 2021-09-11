!c
!c  module diff
!c  [history]
!c  2005/05/21 Yamashita
!c  2005/09/03 Yamashita: add 2-order diff
!c  2005/09/27 Yamashita: bugfix for diffrz
!c  2010/10/17 Yamashita: modify diffy2 (remove unused CS)
!c  2010/10/18 Yamashita: add dble for some calculations
!c  2010/10/18 Yamashita: modify scheme of diffx, diffxf, diffx2, diffxf2
!c  2010/10/20 Yamashita: modify scheme of diffxf, diffxf2, diffyf
!c  2010/10/22 Yamashita: use diffyf instead of diffy in diffxyf
!c  2017/06/06 Yamashita: use common_typedef
!c
!c  integer nx; number of x-grid  ny; number of y-grid
!c          np; number of p-grid
!c
!c  const
!c      real(kind=r8b) PP(np); sigma levels  YY(ny); latitude
!c      real(kind=r8b) CS(j); cos( lat[rad] ) AR; radius of planet
!c      real(kind=r8b) undef; undefined value
!c=====================================================================c
module diff
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: diffx, diffxf, diffx2, diffxf2, diffy, diffyf, diffy2, diffz, diffrz
  public :: diffxy, diffxyf, diffxz, diffxzf, diffyz, diffyzf

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine:  diffx
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d (din) / dx   (dx=AR*DX)
!c=====
subroutine diffx(nx, ny, np, undef, din, diff, AR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: AR             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: i1, i2         !!
  real(kind=r8b)                :: D, DX, DDX, PI !!
  PI = 4.d0 * datan(1.d0) 
!c
!c d / dx
!c===
  DX = 2.d0 * PI / nx
  DDX = 1.d0 / (2.d0 * AR * DX)
  do k = 1, np
    do j = 1, ny
      do i = 1, nx
        i1 = i - 1
        i2 = i + 1
        if (i == 1) i1 = nx
        if (i == nx) i2 = 1
        if (din(i1,j,k) /= undef.and.din(i2,j,k) /= undef) then
          D = dble(din(i2,j,k)) - din(i1,j,k)
          diff(i,j,k) = D * DDX
        else
          diff(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine diffx

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxf
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d (din) / dx   (dx=AR*cos(lat)*DX)
!c=====
subroutine diffxf(nx, ny, np, undef, din, diff, AR, CS)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: CS(ny), AR     !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: i1, i2         !!
  real(kind=r8b)                :: D, DX, DDX, PI !!
  PI = 4.d0 * datan(1.d0) 
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
        if (din(i1,j,k) /= undef.and.din(i2,j,k) /= undef) then
          D = dble(din(i2,j,k)) - din(i1,j,k)
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
!c  subroutine:  diffx2
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dx^2  (dx=AR*DX)
!c=====
subroutine diffx2(nx, ny, np, undef, din, diff, AR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: AR             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: i1, i2         !!
  real(kind=r8b)                :: D, DX, DDX, PI !!
  PI = 4.d0 * datan(1.d0) 
!c
!c d / dx
!c===
  DX = 2.d0 * PI / nx
  do k = 1, np
    do j = 1, ny
      DDX = 1.d0 / (AR * DX)
      do i = 1, nx
        i1 = i - 1
        i2 = i + 1
        if (i == 1) i1 = nx
        if (i == nx) i2 = 1
        if (din(i2,j,k) /= undef.and.din(i,j,k) /= undef       &
&         .and.din(i1,j,k) /= undef) then
          D = dble(din(i2,j,k)) + dble(din(i1,j,k)) - 2.d0 * din(i,j,k)
          diff(i,j,k) = D * DDX**2
        else
          diff(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine diffx2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxf2
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dx^2  (dx=AR*cos(lat)*DX)
!c=====
subroutine diffxf2(nx, ny, np, undef, din, diff, AR, CS)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: CS(ny), AR     !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: i1, i2         !!
  real(kind=r8b)                :: D, DX, DDX, PI !!
  PI = 4.d0 * datan(1.d0) 
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
        if (din(i2,j,k) /= undef.and.din(i,j,k) /= undef       &
&         .and.din(i1,j,k) /= undef) then
          D = dble(din(i2,j,k)) + dble(din(i1,j,k)) - 2.d0 * din(i,j,k)
          diff(i,j,k) = D * DDX**2
        else
          diff(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine diffxf2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffy
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d (din) / dy  (dy=AR*DY)
!c                                              (DY=lat2-lat1 [rad])
!c=====
subroutine diffy(nx, ny, np, undef, din, diff, YY, AR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: j1, j2         !!
  real(kind=r8b)                :: DDY, PI        !!
  PI = 4.d0 * datan(1.d0) 
!c
!c d / dy
!c===
  do k = 1, np
    do j = 1, ny
      j1 = j - 1
      j2 = j + 1
      if (j == 1)  j1 = 1
      if (j == ny) j2 = ny
      DDY = AR * PI * (YY(j2) - YY(j1)) / 180.d0
      DDY = 1.d0 / DDY
      do i = 1, nx   ! i
        if (din(i,j2,k) /= undef.and.din(i,j1,k) /= undef) then
          diff(i,j,k) = (din(i,j2,k) - dble(din(i,j1,k))) * DDY
        else
          diff(i,j,k) = undef
        endif
      enddo !! i 
    enddo !! j
  enddo !! k

  return
end subroutine diffy

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffyf
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d (cos(lat) * din) / dy   
!c                                              (dy=AR*cos(lat)*DY)
!c                                              (DY=lat2-lat1 [rad])
!c=====
subroutine diffyf(nx, ny, np, undef, din, diff, YY, AR, CS)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR, CS(ny)     !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: j1, j2, jc     !!
  real(kind=r8b)                :: DDY, PI        !!
  real(kind=r8b)                :: spole, npole   !!
  PI = 4.d0 * datan(1.d0) 
  if (ny == 1) then
    diff(1:nx,1:ny,1:np) = din(1:nx,1:ny,1:np)
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
      if (din(i,j2,k) /= undef.and.din(i,j1,k) /= undef) then
        npole = npole + din(i,j2,k) + (din(i,j1,k) - din(i,j2,k)) &
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
      if (din(i,j2,k) /= undef.and.din(i,j1,k) /= undef) then
        spole = spole + din(i,j1,k) + (din(i,j2,k) - din(i,j1,k)) &
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
        if (din(i,j2,k) /= undef.and.din(i,j1,k) /= undef) then
          diff(i,j,k) = (CS(j2) * din(i,j2,k) - CS(j1) * din(i,j1,k)) / DDY
          !c+++ NP
          if (j == 1) then
            if (npole /= undef) then
              DDY = AR * PI * (90.d0 - YY(j2)) / 180.d0
              diff(i,j,k) = (npole - dble(din(i,j2,k))) / DDY
              !ccc DDY = AR * CS(jc) * PI * (90.d0 - YY(j2)) / 180.d0
              !ccc diff(i,j,k) = CS(jc) * (npole - dble(din(i,j2,k))) / DDY
            else
              diff(i,j,k) = undef
            endif
          endif
          !c+++ SP
          if (j == ny) then
            if (spole /= undef) then
              DDY = AR * PI * (-90.d0 - YY(j1)) / 180.d0
              diff(i,j,k) = (spole - dble(din(i,j1,k))) / DDY
              !ccc DDY = AR * CS(jc) * PI * (-90.d0 - YY(j1)) / 180.d0
              !ccc diff(i,j,k) = CS(jc) * (spole(i) - dble(din(i,j1,k))) / DDY
            else
              diff(i,j,k) = undef
            endif
          endif
          !ccc if (j == 1.or.j == ny) then
          !ccc  diff(i,j,k) = CS(jc) * (din(i,j2,k) - dble(din(i,j1,k))) / DDY
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
!c  subroutine:  diffy2
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dy^2   
!c                                              (dy=AR*DY)
!c                                              (DY=lat2-lat1 [rad])
!c=====
subroutine diffy2(nx, ny, np, undef, din, diff, YY, AR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: j1, j2, jc     !!
  real(kind=r8b)                :: DDY, PI        !!
  PI = 4.d0 * datan(1.d0) 
!c
!c d / dy
!c===
  do k = 1, np
    do j = 1, ny
      j1 = j - 1
      j2 = j + 1
      jc = j
      if (j == 1)  j1 = 1
      if (j == 1)  jc = 2
      if (j == ny) j2 = ny
      if (j == ny) jc = ny - 1
      DDY = 0.5d0 * AR * PI * (YY(j2) - YY(j1)) / 180.d0

      do i = 1, nx
        if (din(i,j2,k) /= undef.and.din(i,j1,k) /= undef    &
&         .and.din(i,jc,k)/=undef) then
          diff(i,j,k) = (dble(din(i,j2,k)) + dble(din(i,j1,k)) - 2.d0 * din(i,jc,k)) / DDY**2
        else
          diff(i,j,k) = undef
        endif
      enddo !! i 
    enddo !! j
  enddo !! k

  return
end subroutine diffy2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine  diffz
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d (din) / dz
!c=====
subroutine diffz(nx, ny, np, undef, din, diff, PP)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: PP(np)
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: k1, k2         !!
  real(kind=r8b)                :: DP, PI         !!
  PI = 4.d0 * datan(1.d0) 
!c
!c d / dz
!c===
  do k = 1, np
    k1 = k - 1
    k2 = k + 1
    if (k == 1) k1 = 1
    if (k == np) k2 = np
    DP = PP(k2) - PP(k1)
    DP = 1 / DP
    do j = 1, ny
      do i = 1, nx
        if (din(i,j,k2) /= undef.and.din(i,j,k1) /= undef) then
          diff(i,j,k) = (dble(din(i,j,k2)) - din(i,j,k1)) * DP
        else
          diff(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine diffz

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffrz
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c      real(kind=r4b)  RHO(nx,ny,nz) ; density
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; 1/rho * d(rho * din) / dz
!c
!c      ieq = 0 ; use RHO
!c      ieq = 1 ; use constant value r0 (1.2 for earth )
!c=====
subroutine diffrz(nx, ny, np, undef, din, RHO, diff, PP, r0, ieq)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r4b), intent(inout) :: RHO(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: PP(np)         !!
  real(kind=r8b), intent(in)    :: r0             !!
  integer(kind=i4b), intent(in) :: ieq            !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !!
  integer(kind=i4b)             :: k1, k2         !!
  real(kind=r8b)                :: DP, PI         !!
  PI = 4.d0 * datan(1.d0) 
!c
!c init
!c===
  if (ieq == 1) then 
    do k = 1, np
      do j = 1, ny
        do i = 1, nx
          RHO(i,j,k) = r0 * exp(PP(k))
         enddo !! i
       enddo !! j
    enddo !! k
  endif
!c
!c 1/rho * d(rho*  ) / dz
!c===
  do k = 1, np
    k1 = k - 1
    k2 = k + 1
    if (k == 1) k1 = 1
    if (k == np) k2 = np
    DP = PP(k2) - PP(k1)
    DP = 1 / DP
    do j = 1, ny
      do i = 1, nx
        if (din(i,j,k2) /= undef.and.din(i,j,k1) /= undef        &
&         .and.RHO(i,j,k2) /= undef.and.RHO(i,j,k1) /= undef) then
          if (RHO(i,j,k) /= undef.and.RHO(i,j,k) /= 0) then
            diff(i,j,k) = (RHO(i,j,k2) * dble(din(i,j,k2)) &
&                        - RHO(i,j,k1) * dble(din(i,j,k1))) * DP / RHO(i,j,k)
          else
            diff(i,j,k) = undef
          endif
        else
          diff(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine diffrz

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxy
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dxdy  (dx=AR*DX, dy=AR*DY)
!c                                                  (DY=lat2-lat1 [rad])
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffxy(nx, ny, np, undef, din, diff, YY, AR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b), allocatable   :: work(:,:,:)    !!
  allocate(work(nx,ny,np))
!c
!c d^2 / dx dy
!c===
  call diffx(nx, ny, np, undef, din, work, AR)
  call diffy(nx, ny, np, undef, work, diff, YY, AR)
  deallocate(work)

  return
end subroutine diffxy

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxyf
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dxdy  (dx=AR*cos(lat)*DX)
!c                                                  (dy=AR*DY)
!c                                                  (DY=lat2-lat1 [rad])
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffxyf(nx, ny, np, undef, din, diff, YY, AR, CS)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR, CS(ny)     !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b), allocatable   :: work(:,:,:)    !!
  allocate(work(nx,ny,np))
!c
!c d^2 / dx dy
!c===
  call diffxf(nx, ny, np, undef, din, work, AR, CS)
  call diffy(nx, ny, np, undef, work, diff, YY, AR)
  deallocate(work)

  return
end subroutine diffxyf

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxz
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dxdz  (dx=AR*DX)
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffxz(nx, ny, np, undef, din, diff, AR, PP)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: AR             !!
  real(kind=r8b), intent(in)    :: PP(np)         !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b), allocatable   :: work(:,:,:)    !!
  allocate(work(nx,ny,np))
!c
!c d^2 / dx dz
!c===
  call diffx(nx, ny, np, undef, din, work, AR)
  call diffz(nx, ny, np, undef, work, diff, PP)
  deallocate(work)

  return
end subroutine diffxz

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxzf
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dxdz  (dx=AR*cos(lat)*DX)
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffxzf(nx, ny, np, undef, din, diff, AR, CS, PP)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: AR, CS(ny)     !!
  real(kind=r8b), intent(in)    :: PP(np)         !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b), allocatable   :: work(:,:,:)    !!
  allocate(work(nx,ny,np))
!c
!c d^2 / dx dz
!c===
  call diffxf(nx, ny, np, undef, din, work, AR, CS)
  call diffz(nx, ny, np, undef, work, diff, PP)
  deallocate(work)

  return
end subroutine diffxzf

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffyz
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dydz  (dy=AR*DY)
!c                                                  (DY=lat2-lat1 [rad])
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffyz(nx, ny, np, undef, din, diff, YY, AR, PP)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR             !!
  real(kind=r8b), intent(in)    :: PP(np)         !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b), allocatable   :: work(:,:,:)    !!
  allocate(work(nx,ny,np))
!c
!c d^2 / dy dz
!c===
  call diffy(nx, ny, np, undef, din, work, YY, AR)
  call diffz(nx, ny, np, undef, work, diff, PP)
  deallocate(work)

  return
end subroutine diffyz

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffyzf
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dydz  (dy=AR*DY)
!c                                                  (DY=lat2-lat1 [rad])
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffyzf(nx, ny, np, undef, din, diff, YY, AR, CS, PP)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR, CS(ny)     !!
  real(kind=r8b), intent(in)    :: PP(np)         !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b), allocatable   :: work(:,:,:)    !!
  allocate(work(nx,ny,np))
!c
!c d^2 / dy dz
!c===
  call diffyf(nx, ny, np, undef, din, work, YY, AR, CS)
  call diffz(nx, ny, np, undef, work, diff, PP)
  deallocate(work)

  return
end subroutine diffyzf

!c---------------------------------------------------------------------c

!c=====================================================================c

end module diff
