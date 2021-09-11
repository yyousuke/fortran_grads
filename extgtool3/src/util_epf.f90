!c
!c  module util_epf
!c  [history]
!c  2013/03/30 Yamashita: first ver. (from 2013/03/29 gtepflux.f90 by Yamashita)
!c  2013/04/01 Yamashita: add wsta_pr
!c  2013/10/18 Yamashita: add get_axis and modify getyaxis, getzaxis
!c  2017/05/15 Yamashita: remove get_zaxsize, get_axis, get_etacoef
!c  2017/07/26 Yamashita: remove conv_zaxis, get_axname
!c
!c  public: omg2w: convert from omg (hPa/s) into w (m/s)
!c  public: n2out: output Brunt Vaisalla frequency N^2 (1/s2)
!c  public: flux_pr: main program of E-P flux
!c  public: wsta_pr: main program of Residual mean meridional circulation
!c  public: conv_zaxis: convert from sig-/eta-levels into p-levels
!c    external get_etacoef: get eta_fa and eta_fb from eta-axis information file
!c  public: conv_fourier: convert from original data into wavenumber component
!c  public: getyaxis: get y-axis points from axis information file
!c  public: getzaxis: get z-axis points from axis information file
!c    external: get_axis: get axis points from axis information file
!c
!c======================================================================c
module util_epf
  implicit none
  private
  public :: omg2w, n2out, flux_pr, wsta_pr, conv_fourier
  public :: getyaxis, getzaxis

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine omg2w
!c  convert from omg (hPa/s) into w (m/s)
!c  w = -H/p * omg
!c=====
subroutine omg2w(imax, jmax, kmax, rmiss, omg, w, p, h)
  !c+++ [input]
  integer(4), intent(in) :: imax, jmax, kmax
  real(8), intent(in)    :: rmiss               !! undefined value
  real(4), intent(in)    :: omg(imax,jmax,kmax) !! input omg(hPa/s)
  real(8), intent(in)    :: p(kmax)             !! p (hPa)
  real(8), intent(in)    :: h                   !! scale height H(m)
  !c+++ [output]
  real(4), intent(out)   :: w(imax,jmax,kmax)   !! output w(m/s)
  !c+++ [internal work]
  integer(4) :: i, j, k

  !c+++ w  = -H/p * omg
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (omg(i,j,k) == rmiss) then
          w(i,j,k) = rmiss
        else
          w(i,j,k) = - h / p(k) / 100. * omg(i,j,k) * 100. ! omg[hPa/s] for CCMV, p[hPa]->w[m/s]
          !ccc w(i,j,k) = -h / p(k) / 100. * omg(i,j,k)       ! omg[Pa/s] for NCEP, ERA40, JRA25 p[hPa]->w[m/s]
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine omg2w

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine n2out
!c  output Brunt Vaisalla frequency N^2 (1/s2)
!c  N^2 = -P*Rd/H^2*(dT/dp) + Rd*KAPPA/H^2*(T)
!c=====
subroutine n2out(imax, jmax, kmax, rmiss, t, n2, p, rd, h, rkappa)
  !c+++ [input]
  integer(4), intent(in) :: imax, jmax, kmax
  real(8), intent(in)    :: rmiss              !! undefined value
  !c+++ data
  real(4), intent(in)    :: t(imax,jmax,kmax)  !! temperature (K)
  real(8), intent(in)    :: p(kmax)            !! pressure (hPa)
  !c+++ constants
  real(8), intent(in)    :: rd                 !! gas constant of dry air (J/K/kg)
  real(8), intent(in)    :: h                  !! scale height H(m)
  real(8), intent(in)    :: rkappa             !! rkappa = rd / cp
  !c+++ [output]
  real(4), intent(out)   :: n2(imax,jmax,kmax) !! N^2 (1/s2)
  !c+++ [internal work]
  integer(4) :: i, j, k, k1, k2

  !c+++ N^2 = -P*Rd/H^2*(dT/dp) + Rd*KAPPA/H^2*(T)
  do k = 1, kmax
    k1 = k - 1
    k2 = k + 1
    if (k == 1) k1 = 1
    if (k == kmax) k2 = kmax
    do j = 1, jmax
      do i = 1, imax
        if (t(i,j,k) /= rmiss.and.t(i,j,k1) /= rmiss.and.t(i,j,k2) /= rmiss) then
          n2(i,j,k) = -p(k) * 100.d0 * rd / h / h * &
&           (t(i,j,k2) - t(i,j,k1)) / (p(k2) - p(k1)) / 100.d0 &
&           + rd * rkappa / h / h * t(i,j,k)
        else
          n2(i,j,k) = rmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine n2out

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine flux_pr
!c  main program of E-P flux
!c  epfy = rho0*cos(phi)*(u_z*R/H*[v't']/N^2 - [u'v'])
!c  epfz = rho0*cos(phi)*{(f-u_y)*R/H*[v't']/N^2 - [u'w']}
!c  epfdiv = epfy0y_i/rho0/cos(phi) + epfz0z_i/rho0/cos(phi)
!c    epfy0y_i = 1/(a*cos)*d(cos*epfy)/dphi
!c    epfz0z_i = -p/h*d(epfz)/dp
!c=====
subroutine flux_pr(imax, jmax, kmax, nv, rmiss, &
& uv, vt, uw, ulon, n2, rho0, d, &
& lat, phi, f, cs, p, a, g, h, rd)
  use calculate, only: add
  use diff, only: diffyf
  use diffp2z, only: diffz_p2z
  use zmean, only: ZONALmean
  !c+++ [input]
  integer(4), intent(in) :: imax, jmax, kmax   !! x-, y-, z- size
  integer(4), intent(in) :: nv                 !! number of output variables
  real(8), intent(in)    :: rmiss              !! undefined value
  !c+++ data
  real(4), intent(in)    :: uv(imax,jmax,kmax) !! eddy u'v' (m2/s2)
  real(4), intent(in)    :: vt(imax,jmax,kmax) !! eddy v't' (Km/s)
  real(4), intent(in)    :: uw(imax,jmax,kmax) !! eddy u'w' (m2/s2)
  real(4), intent(in)    :: ulon(jmax,kmax)    !! [u] (m/s)
  real(4), intent(in)    :: n2(jmax,kmax)      !! N^2 (1/s2)
  real(4), intent(in)    :: rho0(jmax,kmax)    !! rho (kg/m3)
  !c+++ axis
  real(8), intent(in)    :: lat(jmax)          !! latitude (deg)
  real(8), intent(in)    :: phi(jmax)          !! latitude (rad)
  real(8), intent(in)    :: f(jmax)            !! Coriolis parameter (1/s)
  real(8), intent(in)    :: cs(jmax)           !! cosine of latitude ()
  real(8), intent(in)    :: p(kmax)            !! pressure (hPa)
  !c+++ constants
  real(8), intent(in)    :: a                  !! equatorial radius (m)
  real(8), intent(in)    :: g                  !! gravitational acceleration (m/s2)
  real(8), intent(in)    :: h                  !! scale height H(m)
  real(8), intent(in)    :: rd                 !! gas constant of dry air (J/K/kg)
  !c+++ [output]
  real(4), intent(out)   :: d(jmax,kmax,nv)    !! output variables
  !c d(1): epfy (kg/m/s2), d(2): epfz (kg/m/s2), d(3): epfdiv (m/s2)
  !c d(4): epfy0y (m/s2), d(5): epfz0z (m/s2)
  !c d(6): epfy2 (kg/s2), d(7): epfz2 (kg/s2), d(8): epfdiv2 (kg/m/s2)
  !c d(9): epfy0y2 (kg/m/s2), d(10): epfz0z2 (kg/m/s2)
  !c+++ [internal work]
  integer(4) :: i, j, k !! loop variables
  integer(4) :: k1, k2
  real(4) :: ulonz(jmax,kmax), ulony(jmax,kmax)
  real(4) :: rhvtn2(imax,jmax,kmax), rhvtn2lon(jmax,kmax)
  !c+++ 3-D array of E-P flux
  real(4) :: epfy0(imax,jmax,kmax), epfz0(imax,jmax,kmax)
  !c+++ E-P flux & divergence
  real(4) :: epfylon(jmax,kmax)  !!  1: meridional E-P flux (kg/m/s2)
  real(4) :: epfzlon(jmax,kmax)  !!  2: vertical E-P flux (kg/m/s2)
  real(4) :: epfdiv(jmax,kmax)   !!  3: divergence of E-P flux (m/s2)
  real(4) :: epfylony(jmax,kmax) !!  4: y-divergence of E-P flux (m/s2)
  real(4) :: epfzlonz(jmax,kmax) !!  5: z-divergence of E-P flux (m/s2)
  real(4) :: epfylon2(jmax,kmax) !!  6: meridional E-P flux*a (kg/s2)
  real(4) :: epfzlon2(jmax,kmax) !!  7: vertical E-P flux*a (kg/s2)
  real(4) :: epfdiv2(jmax,kmax)  !!  8: divergence of E-P flux*a*rho0*cos(phi) (kg/m/s2)
  real(4) :: epfylony2(jmax,kmax)!!  9: y-divergence of E-P flux*a*rho0*cos(phi) (kg/m/s2)
  real(4) :: epfzlonz2(jmax,kmax)!! 10: z-divergence of E-P flux*a*rho0*cos(phi) (kg/m/s2)

  !c+++ R/H * [v'T']/N2
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (vt(i,j,k) == rmiss.or.n2(j,k) == rmiss) then
          rhvtn2(i,j,k) = rmiss
        else
          rhvtn2(i,j,k) = (rd / h * vt(i,j,k)) / n2(j,k)
        endif
      enddo !! i
    enddo !! j
  enddo !! k
  call ZONALmean(imax, jmax, kmax, rmiss, rhvtn2, rhvtn2lon)

  !c+++ du/dz, du/dy
  call diffz_p2z(1, jmax, kmax, rmiss, ulon, rho0, ulonz, p, g)
  call diffyf(1, jmax, kmax, rmiss, ulon, ulony, lat, a, cs)

  !c+++ meridional E-P flux (epfy)
  !c+++ epfy = rho0*cos(phi)*(u_z*R/H*[v't']/N^2 - [u'v'])
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (uv(i,j,k) == rmiss.or.ulonz(j,k) == rmiss.or.vt(i,j,k) == rmiss &
&         .or.rhvtn2(i,j,k) == rmiss) then
          epfy0(i,j,k) = rmiss
        else
          epfy0(i,j,k) = rho0(j,k) * cos(phi(j)) * (ulonz(j,k) * rhvtn2(i,j,k) - uv(i,j,k))
        endif
      enddo !! i
    enddo !! j
  enddo !! k
  call ZONALmean(imax, jmax, kmax, rmiss, epfy0, epfylon)
  !c+++ epfy2 = a*rho0*cos(phi)*(u_z*R/H*[v't']/N^2 - [u'v'])
  call add(1, jmax, kmax, rmiss, epfylon, epfylon, epfylon2, a, 0.d0, 0.d0, 0.d0)
  
  !c+++ vertical E-P flux (epfz)
  !c+++ epfz = rho0*cos(phi)*{(f-u_y)*R/H*[v't']/N^2 - [u'w']}
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (uv(i,j,k) == rmiss.or.ulony(j,k) == rmiss.or.uw(i,j,k) == rmiss &
&         .or.rhvtn2(i,j,k) == rmiss) then
          epfz0(i,j,k) = rmiss
        else
          epfz0(i,j,k) = rho0(j,k) * cos(phi(j)) *  &
&             ((f(j) - ulony(j,k)) * rhvtn2(i,j,k) - uw(i,j,k))
        endif
      enddo !! i
    enddo !! j
  enddo !! k
  call ZONALmean(imax, jmax, kmax, rmiss, epfz0, epfzlon)
  !c+++ epfz2 = a*rho0*cos(phi)*{(f-u_y)*R/H*[v't']/N^2 - [u'w']}
  call add(1, jmax, kmax, rmiss, epfzlon, epfzlon, epfzlon2, a, 0.d0, 0.d0, 0.d0)

  !c+++ E-P flux divergence
  !c+++ epfy0y_i = 1/(a*cos)*d(cos*epfy)/dphi
  call diffyf(1, jmax, kmax, rmiss, epfylon, epfylony, lat, a, cs)
  !c+++ epfz0z_i = -p/h*d(epfz)/dp
  do k = 1, kmax
    k1 = k - 1
    k2 = k + 1
    if (k == 1) k1 = 1
    if (k == kmax) k2 = kmax
    do j = 1, jmax
      if (epfzlon(j,k) == rmiss) then
        epfzlonz(j,k) = rmiss
      else
        epfzlonz(j,k) = - p(k) * 100.d0 / h * (epfzlon(j,k2) - epfzlon(j,k1)) &
&                       / (p(k2) - p(k1)) / 100.d0
      endif
    enddo !! j
  enddo !! k

  !c+++ epfdiv = epfy0y_i/rho0/cos(phi) + epfz0z_i/rho0/cos(phi)
  !c+++ epfdiv2 = epfy0y_i*a + epfz0z_i*a
  do k = 1, kmax
    do j = 1, jmax
      if (epfylony(j,k) == rmiss.or.epfzlonz(j,k) == rmiss) then
        epfdiv(j,k) = rmiss
        epfdiv2(j,k) = rmiss
        epfylony2(j,k) = rmiss
        epfzlonz2(j,k) = rmiss
      else
        epfylony2(j,k) = epfylony(j,k) * a
        epfzlonz2(j,k) = epfzlonz(j,k) * a
        epfylony(j,k) = epfylony(j,k) / rho0(j,k) / cos(phi(j))
        epfzlonz(j,k) = epfzlonz(j,k) / rho0(j,k) / cos(phi(j))
        epfdiv(j,k) = epfylony(j,k) + epfzlonz(j,k)
        epfdiv2(j,k) = epfylony2(j,k) + epfzlonz2(j,k)
      endif
    enddo !! j
  enddo !! k

  !c+++ set output data
  d(1:jmax,1:kmax, 1) = epfylon(1:jmax,1:kmax)   !! d(1): epfy (kg/m/s2)
  d(1:jmax,1:kmax, 2) = epfzlon(1:jmax,1:kmax)   !! d(2): epfz (kg/m/s2)
  d(1:jmax,1:kmax, 3) = epfdiv(1:jmax,1:kmax)    !! d(3): epfdiv (m/s2)
  d(1:jmax,1:kmax, 4) = epfylony(1:jmax,1:kmax)  !! d(4): epfy0y (m/s2)
  d(1:jmax,1:kmax, 5) = epfzlonz(1:jmax,1:kmax)  !! d(5): epfz0z (m/s2)
  d(1:jmax,1:kmax, 6) = epfylon2(1:jmax,1:kmax)  !! d(6): epfy2 (kg/s2)
  d(1:jmax,1:kmax, 7) = epfzlon2(1:jmax,1:kmax)  !! d(7): epfz2 (kg/s2)
  d(1:jmax,1:kmax, 8) = epfdiv2(1:jmax,1:kmax)   !! d(8): epfdiv2 (kg/m/s2)
  d(1:jmax,1:kmax, 9) = epfylony2(1:jmax,1:kmax) !! d(9): epfy0y2 (kg/m/s2)
  d(1:jmax,1:kmax,10) = epfzlonz2(1:jmax,1:kmax) !! d(10): epfz0z2 (kg/m/s2)

  return
end subroutine flux_pr

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine wsta_pr
!c  main program of Residual mean meridional circulation
!c  [v*] = [v] - 1/rho*d(rho * R/H * [v'T']/N^2)/dz
!c  [w*] = [w] + 1/{a*cos(phi)}*d{cos(phi) * R/H * [v'T']/N^2}/dphi
!c=====
subroutine wsta_pr(imax, jmax, kmax, nv, rmiss, &
& vt, ulon, vlon, wlon, n2, rho0, d, &
& lat, phi, f, cs, p, a, g, h, rd)
  use calculate, only: add, sub
  use diff, only: diffyf
  use diffp2z, only: diffz_p2z, diffrz_p2z
  use zmean, only: ZONALmean
  !c+++ [input]
  integer(4), intent(in) :: imax, jmax, kmax   !! x-, y-, z- size
  integer(4), intent(in) :: nv                 !! number of output variables
  real(8), intent(in)    :: rmiss              !! undefined value
  !c+++ data
  real(4), intent(in)    :: vt(imax,jmax,kmax) !! eddy v't' (Km/s)
  real(4), intent(in)    :: ulon(jmax,kmax)    !! [u] (m/s)
  real(4), intent(in)    :: vlon(jmax,kmax)    !! [v] (m/s)
  real(4), intent(in)    :: wlon(jmax,kmax)    !! [w] (m/s)
  real(4), intent(in)    :: n2(jmax,kmax)      !! N^2 (1/s2)
  real(4), intent(in)    :: rho0(jmax,kmax)    !! rho (kg/m3)
  !c+++ axis
  real(8), intent(in)    :: lat(jmax)          !! latitude (deg)
  real(8), intent(in)    :: phi(jmax)          !! latitude (rad)
  real(8), intent(in)    :: f(jmax)            !! Coriolis parameter (1/s)
  real(8), intent(in)    :: cs(jmax)           !! cosine of latitude ()
  real(8), intent(in)    :: p(kmax)            !! pressure (hPa)
  !c+++ constants
  real(8), intent(in)    :: a                  !! equatorial radius (m)
  real(8), intent(in)    :: g                  !! gravitational acceleration (m/s2)
  real(8), intent(in)    :: h                  !! scale height H(m)
  real(8), intent(in)    :: rd                 !! gas constant of dry air (J/K/kg)
  !c+++ [output]
  real(4), intent(out)   :: d(jmax,kmax,nv)    !! output variables
  !c d(1): vsta (m/s), d(2): wsta (m/s), d(3): rhvtn2 (m2/s)
  !c d(4): fv (m/s2), d(5): uzw (m/s2)
  !c d(6): fv2 (kg/m/s2), d(7): uzw2 (kg/m/s2)
  !c+++ [internal work]
  integer(4) :: i, j, k !! loop variables
  !ccc integer(4) :: k1, k2
  real(4) :: ulonz(jmax,kmax), ulony(jmax,kmax)
  real(4) :: rhvtn2(imax,jmax,kmax)
  !c+++ E-P flux & divergence
  real(4) :: vstalon(jmax,kmax)    !!  1: TEM residual v-velocity (m/s)
  real(4) :: wstalon(jmax,kmax)    !!  2: TEM residual w-velocity (m/s)
  real(4) :: rhvtn2lon(jmax,kmax)  !!  3: R/H*[v'T']/N^2 term (m2/s)
  real(4) :: fvstalon(jmax,kmax)   !!  4: f[v*] (m/s2)
  real(4) :: uzwstalon(jmax,kmax)  !!  5: du/dz*[w*] (m/s2)
  real(4) :: fvstalon2(jmax,kmax)  !!  6: rho0*a*cos(phi)*f[v*] (kg/m/s2)
  real(4) :: uzwstalon2(jmax,kmax) !!  7: rho0*a*cos(phi)*du/dz*[w*] (kg/m/s2)

  !c+++ R/H * [v'T']/N2
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (vt(i,j,k) == rmiss.or.n2(j,k) == rmiss) then
          rhvtn2(i,j,k) = rmiss
        else
          rhvtn2(i,j,k) = (rd / h * vt(i,j,k)) / n2(j,k)
        endif
      enddo !! i
    enddo !! j
  enddo !! k
  call ZONALmean(imax, jmax, kmax, rmiss, rhvtn2, rhvtn2lon)

  !c+++ du/dz, du/dy
  call diffz_p2z(1, jmax, kmax, rmiss, ulon, rho0, ulonz, p, g)
  call diffyf(1, jmax, kmax, rmiss, ulon, ulony, lat, a, cs)

  !c+++ 1/rho*d(rho * rhvtn2)/dz
  call diffrz_p2z(1, jmax, kmax, rmiss, rhvtn2lon, rho0, vstalon, p, g)
  !c+++ 1/(a*cos)*d(cos*rhvtn2)/dphi
  call diffyf(1, jmax, kmax, rmiss, rhvtn2lon, wstalon, lat, a, cs)

  !c+++ [v*] = [v] - 1/rho*d(rho * rhvtn2)/dz
  call sub(1, jmax, kmax, rmiss, vlon, vstalon, vstalon, 1.d0, 1.d0, 0.d0, 0.d0)
  !c+++ [w*] = [w] + 1/{a*cos(phi)}*d{cos(phi)*rhvtn2}/dphi
  call add(1, jmax, kmax, rmiss, wlon, wstalon, wstalon, 1.d0, 1.d0, 0.d0, 0.d0)

  !c+++ fvsta & uzw
  do k = 1, kmax
    do j = 1, jmax
      !c+++ fvsta = (f-u_y)*[v*]
      !c+++ fvsta2 = rho0*a*cos(phi)*{(f-u_y)*[v*]}
      if (ulony(j,k) == rmiss.or.vstalon(j,k) == rmiss) then
        fvstalon(j,k) = rmiss
        fvstalon2(j,k) = rmiss
      else
        fvstalon(j,k) = (f(j) - ulony(j,k)) * vstalon(j,k)
        fvstalon2(j,k) = rho0(j,k) * a * cos(phi(j)) * fvstalon(j,k)
      endif
      !c+++ uzwsta = -u_z*[w*]
      !c+++ uzwsta2 = rho0*a*cos(phi)*{-u_z*[w*]}
      if (ulonz(j,k) == rmiss.or.wstalon(j,k) == rmiss) then
        uzwstalon(j,k) = rmiss
        uzwstalon2(j,k) = rmiss
      else
        uzwstalon(j,k) = - ulonz(j,k) * wstalon(j,k)
        uzwstalon2(j,k) = rho0(j,k) * a * cos(phi(j)) * uzwstalon(j,k)
      endif
    enddo !! j
  enddo !! k

  !c+++ set output data
  d(1:jmax,1:kmax, 1) = vstalon(1:jmax,1:kmax)    !! d(1): vsta (m/s)
  d(1:jmax,1:kmax, 2) = wstalon(1:jmax,1:kmax)    !! d(2): wsta (m/s)
  d(1:jmax,1:kmax, 3) = rhvtn2lon(1:jmax,1:kmax)  !! d(3): rhvtn2 (m2/s)
  d(1:jmax,1:kmax, 4) = fvstalon(1:jmax,1:kmax)   !! d(4): fv (m/s2)
  d(1:jmax,1:kmax, 5) = uzwstalon(1:jmax,1:kmax)  !! d(5): uzw (m/s2)
  d(1:jmax,1:kmax, 6) = fvstalon2(1:jmax,1:kmax)  !! d(6): fv2 (kg/m/s2)
  d(1:jmax,1:kmax, 7) = uzwstalon2(1:jmax,1:kmax) !! d(7): uzw2 (kg/m/s2)

  return
end subroutine wsta_pr

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine conv_fourier
!c  convert from original data into wavenumber component
!c=====
subroutine conv_fourier(imax, jmax, kmax, iwn, rmiss, d)
  use fourier, only: fc_fwd, fc_bwd
  !c+++ [input]
  integer(4), intent(in)       :: imax               !! x-axis size
  integer(4), intent(in)       :: jmax               !! y-axis size
  integer(4), intent(in)       :: kmax               !! output z-axis size
  integer(4), intent(in)       :: iwn                !! output wavenumber
  real(8), intent(in)          :: rmiss              !! undefined value
  !c+++ [modify]
  real(4), intent(inout)       :: d(imax,jmax,kmax)  !! data
  !c+++ [internal work]
  integer(4)                   :: N                  !! total wavenumber
  integer(4)                   :: j, k
  real(8), allocatable         :: T(:), U(:)
  complex(8), allocatable      :: W(:)
  N = imax
  allocate (T(N), U(N), W(0:N/2))

  if (iwn < 0.or.iwn > N/2) then
    write(6, *) 'invalid input iwn'
    write(6, *) 'WN, N/2 =', iwn, N/2
    stop
  endif

  do k = 1, kmax
    do j = 1, jmax
      !c+++ input data
      T(1:N) = d(1:imax,j,k)

      !c+++ FORWARD TRANSFORM T(i,j) => W(k,j)
      call fc_fwd(N, T, W, rmiss, iwn, iwn)

      !c+++  BACKWARD TRANSFORM W(k,j) => U(i,j)
      call fc_bwd(N, W, U, iwn, iwn)

      !c+++ output data
      d(1:imax,j,k) = U(1:N)
    enddo !! jmax
  enddo !! kmax

  deallocate (T, U, W)
  return
end subroutine conv_fourier

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine getyaxis
!c  get y-axis points from axis information file
!c=====
subroutine getyaxis(jmax, haxisy, lat)
  use rwgtool, only: get_axis
  !c+++ [input]
  integer(4), intent(in) :: jmax
  character(len=*), intent(in) :: haxisy !! y-axis name
  !c+++ [output]
  real(8), intent(out) :: lat(jmax) !! latitude

  !c+++ read y-axis file
  call get_axis(jmax, haxisy, lat)

  return
end subroutine getyaxis

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine getzaxis
!c  get z-axis points from axis information file
!c=====
subroutine getzaxis(kmax, haxisz, p)
  use rwgtool, only: get_axis
  !c+++ [input]
  integer(4), intent(in) :: kmax
  character(len=*), intent(in) :: haxisz !! z-axis name
  !c+++ [output]
  real(8), intent(out) :: p(kmax) !! p[hPa] for p-lev, sig[] for sig-lev

  !c+++ read z-axis file
  call get_axis(kmax, haxisz, p)

  return
end subroutine getzaxis

!c----------------------------------------------------------------------c

!c======================================================================c

end module util_epf

