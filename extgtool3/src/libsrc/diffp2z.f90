!c
!c  module diffp2z
!c  [history]
!c  2009/02/16 Yamashita
!c  2009/03/02 Yamashita: bug fix
!c  2009/05/30 Yamashita: add diffrz_p2z
!c  2017/06/05 Yamashita: use common_typedef
!c
!c  integer nx; number of x-grid  ny;number of y-grid
!c          np; number of p-grid
!c
!c  const
!c      real(kind=r8b) P(np); p-levels [hPa]  YY(ny); latitude
!c      real(kind=r8b) CS(j); cos( lat[rad] ) AR; radius of planet [m]
!c      real(kind=r8b) undef; undefined value
!c=====================================================================c
module diffp2z
  use common_typedef, only: i4b, r4b, r8b
  use diff
  use calculate
  implicit none
  private
  public :: diffz_p2z, diffrz_p2z
  public :: diffxz_p2z, diffxzf_p2z, diffyz_p2z, diffyzf_p2z

contains

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutne  diffz_p2z
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c      real(kind=r4b) RHO(nx,ny,np)  ; RHO [kg/m3]
!c      real(kind=r8b) P(np) ; p-levels [hPa]
!c      real(kind=r8b) GR ; gravitational acceleration [m/s2]
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d (din) / dz = -RHO * GR * d(din)/dp
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffz_p2z(nx, ny, np, undef, din, RHO, diff, P, GR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r4b), intent(in)    :: RHO(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: P(np)          !! [hPa]
  real(kind=r8b), intent(in)    :: GR             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b)                :: work(nx,ny,np) !!
  real(kind=r8b)                :: PP(np)         !! [Pa]
  PP(1:np) = P(1:np) * 100.d0 !! P[hPa] ==> PP[Pa]
!c
!c - RHO * GR *d (din) / dp
!c===
  !c+++ work = d (din) / dp
  call diffz(nx, ny, np, undef, din, work, PP)
  !c+++ diff = -RHO * GR * work
  call mlt(nx, ny, np, undef, RHO, work, diff, -GR, 1.d0, 0.d0, 0.d0)

  return
end subroutine diffz_p2z

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutne  diffrz_p2z
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c      real(kind=r4b) RHO(nx,ny,np)  ; RHO [kg/m3]
!c      real(kind=r8b) P(np) ; p-levels [hPa]
!c      real(kind=r8b) GR ; gravitational acceleration [m/s2]
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; 
!c                  1/RHO *d (RHO * din) / dz = - GR * d(RHO * din)/dp
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffrz_p2z(nx, ny, np, undef, din, RHO, diff, P, GR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r4b), intent(in)    :: RHO(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: P(np)          !! [hPa]
  real(kind=r8b), intent(in)    :: GR             !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b)                :: work(nx,ny,np) !!
  real(kind=r8b)                :: PP(np)         !! [Pa]
  PP(1:np) = P(1:np) * 100.d0 !! P[hPa] ==> PP[Pa]
!c
!c - GR * d (RHO * din) / dp
!c===
  call mlt(nx, ny, np, undef, RHO, din, work, -GR, 1.d0, 0.d0, 0.d0)
  call diffz(nx, ny, np, undef, work, diff, PP)

  return
end subroutine diffrz_p2z

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxz_p2z
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c      real(kind=r4b) RHO(nx,ny,np)  ; RHO [kg/m3]
!c      real(kind=r8b) P(np) ; p-levels [hPa]
!c      real(kind=r8b) AR ; radius of planet [m]
!c      real(kind=r8b) GR ; gravitational acceleration [m/s2]
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dxdz  (dx=AR*DX)
!c                               (d / dz = -RHO * GR * d/dp)
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffxz_p2z(nx, ny, np, undef, din, RHO, diff, P, AR, GR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r4b), intent(in)    :: RHO(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: P(np)          !! [hPa]
  real(kind=r8b), intent(in)    :: AR, GR         !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b)                :: work(nx,ny,np) !!
!c
!c d^2 / dx dz
!c===
  call diffx(nx, ny, np, undef, din, work, AR)
  call diffz_p2z(nx, ny, np, undef, work, RHO, diff, P, GR)

  return
end subroutine diffxz_p2z

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffxzf_p2z
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c      real(kind=r4b) RHO(nx,ny,np)  ; RHO [kg/m3]
!c      real(kind=r8b) P(np) ; p-levels [hPa]
!c      real(kind=r8b) AR ; radius of planet [m]
!c      real(kind=r8b) CS(ny); cos( lat[rad] ) 
!c      real(kind=r8b) GR ; gravitational acceleration [m/s2]
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dxdz  (dx=AR*cos(lat)*DX)
!c                              (d / dz = -RHO * GR * d/dp)
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffxzf_p2z(nx, ny, np, undef, din, RHO, diff, P, AR, CS, GR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r4b), intent(in)    :: RHO(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: P(np)          !! [hPa]
  real(kind=r8b), intent(in)    :: AR, CS(ny), GR !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b)                :: work(nx,ny,np) !!
!c
!c d^2 / dx dz
!c===
  call diffxf(nx, ny, np, undef, din, work, AR, CS)
  call diffz_p2z(nx, ny, np, undef, work, RHO, diff, P, GR)

  return
end subroutine diffxzf_p2z

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffyz_p2z
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c      real(kind=r4b) RHO(nx,ny,np)  ; RHO [kg/m3]
!c      real(kind=r8b) P(np) ; p-levels [hPa]
!c      real(kind=r8b) YY(ny); latitude [deg]
!c      real(kind=r8b) AR ; radius of planet [m]
!c      real(kind=r8b) GR ; gravitational acceleration [m/s2]
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dydz  (dy=AR*DY)
!c                                                  (DY=lat2-lat1 [rad])
!c                              (d / dz = -RHO * GR * d/dp)
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffyz_p2z(nx, ny, np, undef, din, RHO, diff, P, YY, AR, GR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r4b), intent(in)    :: RHO(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: P(np)          !! [hPa]
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR, GR         !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b)                :: work(nx,ny,np) !!
!c
!c d^2 / dy dz
!c===
  call diffy(nx, ny, np, undef, din, work, YY, AR)
  call diffz_p2z(nx, ny, np, undef, work, RHO, diff, P, GR)

  return
end subroutine diffyz_p2z

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine:  diffyzf_p2z
!c  INPUT
!c      real(kind=r4b) din(nx,ny,np) ; data
!c      real(kind=r4b) RHO(nx,ny,np)  ; RHO [kg/m3]
!c      real(kind=r8b) P(np) ; p-levels [hPa]
!c      real(kind=r8b) YY(ny); latitude [deg]
!c      real(kind=r8b) AR ; radius of planet [m]
!c      real(kind=r8b) CS(ny); cos( lat[rad] ) 
!c      real(kind=r8b) GR ; gravitational acceleration [m/s2]
!c  OUTPUT
!c      real(kind=r4b) diff(nx,ny,np) ; d^2 (din) / dydz  (dy=AR*DY)
!c                                                  (DY=lat2-lat1 [rad])
!c                              (d / dz = -RHO * GR * d/dp)
!c  WORK
!c      real(kind=r4b) work(nx,ny,np)
!c=====
subroutine diffyzf_p2z(nx, ny, np, undef, din, RHO, diff, P, YY, AR, CS, GR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np     !!
  real(kind=r8b), intent(in)    :: undef          !!
  real(kind=r4b), intent(in)    :: din(nx,ny,np)  !!
  real(kind=r4b), intent(in)    :: RHO(nx,ny,np)  !!
  real(kind=r8b), intent(in)    :: P(np)          !! [hPa]
  real(kind=r8b), intent(in)    :: YY(ny)         !!
  real(kind=r8b), intent(in)    :: AR, CS(ny), GR !!
  !c+++ [output]
  real(kind=r4b), intent(out)   :: diff(nx,ny,np) !!
  !c+++ [internal work]
  real(kind=r4b)                :: work(nx,ny,np) !!
!c
!c d^2 / dy dz
!c===
  call diffyf(nx, ny, np, undef, din, work, YY, AR, CS)
  call diffz_p2z(nx, ny, np, undef, work, RHO, diff, P, GR)

  return
end subroutine diffyzf_p2z

!c---------------------------------------------------------------------c

!c=====================================================================c

end module diffp2z
