!c
!c  module util_pvont
!c  [history]
!c  2013/03/27 Yamashita: first ver. from pvseek in pvont.f by S. Sugata (2001/04/02)
!c  2013/03/28 Yamashita: f77 ==> f90
!c  2017/11/22 Yamashita: varonp
!c
!c  public: pvseek
!c    internal: varont　温位面上の変数
!c    internal: varonp　p面上の変数
!c    internal: relvor　相対渦度
!c    internal: thz　dth/dp
!c      internal: pseek 指定温位面上の圧力を計算
!c      internal: thzsub 鉛直内挿
!c
!c======================================================================c
module util_pvont
  use common_typedef, only: i2b, i4b, r4b, r8b
  implicit none
  private
  public :: pvseek, varont, varonp

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c
!c  subroutine pvseek
!c
!c  PVを求めるサブルーチンメイン
!c=====
subroutine pvseek(uu, vv, th, rth, plev, rlat, pv, rmiss, imax, jmax, kmax)
  !c+++ [input]
  real(kind=r4b), intent(in)    :: uu(imax,jmax,kmax)   !" zonal wind
  real(kind=r4b), intent(in)    :: vv(imax,jmax,kmax)   !" meridional wind
  real(kind=r4b), intent(in)    :: th(imax,jmax,kmax)   !" potential temp.
  real(kind=r4b), intent(in)    :: rth                  !" 指定温位面(この温位上のpvを求める)
  real(kind=r4b), intent(in)    :: plev(imax,jmax,kmax) !" pressure levels
  real(kind=r4b), intent(in)    :: rlat(jmax)           !" 緯度 in radian
  real(kind=r4b), intent(in)    :: rmiss                !" missing value
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !" x-, y- z- axis sizes
  !c+++ [output]
  real(kind=r4b), intent(out)   :: pv(imax,jmax)        !" 求めた温位(PV)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j
  real(kind=r4b)                :: u2(imax,jmax)   !" 温位面上のu
  real(kind=r4b)                :: v2(imax,jmax)   !" 温位面上のv
  real(kind=r4b)                :: uabs(imax,jmax) !" 温位面上のsqrt(u**2+v**2)
  real(kind=r4b)                :: relv(imax,jmax) !" 温位面上の相対渦度
  real(kind=r4b)                :: dtdp(imax,jmax) !" 温位面上のd theta/d p
  real(kind=r4b)                :: pi              !" 円周率
  real(kind=r4b)                :: romega          !" 地球回転角速度
  real(kind=r4b)                :: ff(jmax)        !" コリオリパラメター f = 2 * omega * sin(lat)
  real(kind=r4b)                :: grav            !" 重力加速度

  !c+++ set up
  pi = atan(1.e0) * 4.e0     !" 円周率
  romega = 2.0 * pi / 86400. !" 地球回転角速度
  grav = 9.8 !" 重力加速度
  !ccc write(6,*) 'Omega = ', romega
  do j = 1, jmax
    ff(j) = 2. * romega * sin(rlat(j))
  enddo !" j

  !c+++ ==> u, v on theta
  call varont(uu, th, rth, u2, rmiss, imax, jmax, kmax)
  call varont(vv, th, rth, v2, rmiss, imax, jmax, kmax)

  do j = 1, jmax
    do i = 1, imax
      if (u2(i,j) /= rmiss.and.v2(i,j) /= rmiss) then
        uabs(i,j) = sqrt(u2(i,j)**2 + v2(i,j)**2)
      else
        uabs(i,j) = rmiss
      endif
    enddo !! i
  enddo !! j

  !c+++ relative vorticity
  call relvor(u2, v2, rlat, relv, rmiss, imax, jmax)
  !c+++ dth/dp
  call thz(th, rth, plev, dtdp, rmiss, imax, jmax, kmax)

  do j = 1, jmax
    do i = 1, imax
      if (relv(i,j) /= rmiss.and.dtdp(i,j) /= rmiss) then
        pv(i,j) = - grav * (relv(i,j) + ff(j)) * dtdp(i,j)
      else
        pv(i,j) = rmiss
      endif
    enddo !" i
  enddo !" j

  return
end subroutine pvseek

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine varont
!c  [history]
!c  2001/04/02 S. Sugata: subroutine varont (thintp.f)
!c  2005/11/27 Yoshiki  : subroutine varont (thintp.f)
!c  2013/03/27 Yamashita: f77 ==> f90
!c
!c  p面上の変数から指定温位面上の変数を計算するサブルーチン
!c=====
subroutine varont(uu, th, rth, u2, rmiss, imax, jmax, kmax)
  !c+++ [input]
  real(kind=r4b), intent(in)    :: uu(imax,jmax,kmax) !" din
  real(kind=r4b), intent(in)    :: th(imax,jmax,kmax) !" theta
  real(kind=r4b), intent(in)    :: rth                !" 指定温位面
  real(kind=r4b), intent(in)    :: rmiss              !" missing value
  integer(kind=i4b), intent(in) :: imax, jmax, kmax   !" x-, y- z- axis sizes
  !c+++ [output]
  real(kind=r4b), intent(out)   :: u2(imax,jmax)      !" 温位面上の変数
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k            !"

  do j = 1, jmax
    do i = 1, imax
      k = 1
      if (th(i,j,k) > rth) then !" theta(z=1)が指定温位より大
        u2(i,j) = rmiss
        cycle
      endif
      !c+++ linear interpolation
 110  continue
      if (th(i,j,k+1) >= rth) then !" th(k+1) >= rth > th(k)
        if (uu(i,j,k) /= rmiss.and.uu(i,j,k+1) /= rmiss &
&         .and.th(i,j,k) /= rmiss.and.th(i,j,k+1)/= rmiss) then
          u2(i,j) = uu(i,j,k) &
&                 + (uu(i,j,k+1) - uu(i,j,k)) &
&                 * (rth - th(i,j,k)) / (th(i,j,k+1) - th(i,j,k))
        else
          u2(i,j) = rmiss
        endif
      elseif (k < kmax-1) then
         k = k + 1
         goto 110
      else  !" theta(kmax)が指定温位より小
        u2(i,j) = rmiss
      end if
    enddo !" i
  enddo !" j

  return
end subroutine varont

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine varonp
!c  [history]
!c  2017/11/22 Yamashita: varonp from varont
!c
!c  温位面上の変数から指定p面上の変数を計算するサブルーチン
!c=====
subroutine varonp(uu, p, rp, u2, rmiss, imax, jmax, kmax)
  !c+++ [input]
  real(kind=r4b), intent(in)    :: uu(imax,jmax,kmax) !" din on theta
  real(kind=r4b), intent(in)    :: p(imax,jmax,kmax)  !" p on theta
  real(kind=r4b), intent(in)    :: rp                 !" 指定p面
  real(kind=r4b), intent(in)    :: rmiss              !" missing value
  integer(kind=i4b), intent(in) :: imax, jmax, kmax   !" x-, y- z- axis sizes
  !c+++ [output]
  real(kind=r4b), intent(out)   :: u2(imax,jmax)      !" p面上の変数
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k            !"

  do j = 1, jmax
    do i = 1, imax
      k = 1
      if (p(i,j,k) < rp) then !" p(z=1)が指定気圧より小
        u2(i,j) = rmiss
        cycle
      endif
      !c+++ linear interpolation
 110  continue
      if (p(i,j,k+1) <= rp) then !" p(k+1) <= rp < p(k)
        if (uu(i,j,k) /= rmiss.and.uu(i,j,k+1) /= rmiss &
&         .and.p(i,j,k) /= rmiss.and.p(i,j,k+1)/= rmiss) then
          u2(i,j) = uu(i,j,k) &
&                 + (uu(i,j,k+1) - uu(i,j,k)) &
&                 * (rp - p(i,j,k)) / (p(i,j,k+1) - p(i,j,k))
        else
          u2(i,j) = rmiss
        endif
      elseif (k < kmax-1) then
         k = k + 1
         goto 110
      else  !" theta(kmax)が指定温位より小
        u2(i,j) = rmiss
      end if
    enddo !" i
  enddo !" j

  return
end subroutine varonp

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine relvor
!c
!c  相対渦度を計算するサブルーチン
!c=====
subroutine relvor(u2, v2, rlat, relv, rmiss, imax, jmax )
  !c+++ [input]
  real(kind=r4b), intent(in)    :: u2(imax,jmax)        !" 温位面上のu
  real(kind=r4b), intent(in)    :: v2(imax,jmax)        !" 温位面上のv
  real(kind=r4b), intent(in)    :: rlat(jmax)           !" 緯度 in radian
  real(kind=r4b), intent(in)    :: rmiss                !" missing value
  integer(kind=i4b), intent(in) :: imax, jmax           !" x-, y- axis sizes
  !c+++ [output]
  real(kind=r4b), intent(out)   :: relv(imax,jmax)      !" 温位面上の相対渦度
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j                 !"
  integer(kind=i4b)             :: i1, i2, j1, j2       !"
  real(kind=r8b)                :: ER                   !" 地球半径(m)
  real(kind=r8b)                :: pi                   !" 円周率
  real(kind=r8b)                :: unix                 !" 赤道でのグリッド経度一つ分
  real(kind=r8b)                :: polen, poles         !" 北極点と南極点での循環の値

  ER = 6370.d3
  pi = atan(1.d0) * 4.d0

  unix = 2.d0 * pi * ER / real(imax)

  !" ここから極以外の計算
  do j = 2, jmax-1
    j1 = j - 1
    j2 = j + 1
    do i = 1, imax
      i1 = i - 1
      i2 = i + 1
      if (i == 1) then
        i1 = imax
      else if (i == imax) then
        i2 = 1
      endif

      if (u2(i ,j1) /= rmiss.and.u2(i ,j ) /= rmiss.and. &
&         u2(i ,j2) /= rmiss.and.u2(i1,j1) /= rmiss.and. &
&         u2(i1,j ) /= rmiss.and.u2(i1,j2) /= rmiss.and. &
&         u2(i2,j1) /= rmiss.and.u2(i2,j ) /= rmiss.and. &
&         u2(i2,j2) /= rmiss.and. &
&         v2(i1,j1) /= rmiss.and.v2(i1,j ) /= rmiss.and. &
&         v2(i1,j2) /= rmiss.and.v2(i2,j1) /= rmiss.and. &
&         v2(i2,j ) /= rmiss.and.v2(i2,j2) /= rmiss) then

!c similar to
!c &         + 0.125d0 * ( u2(i1,j2) - u2(i1,j1) + 2.d0 * (u2(i,j2) - u2(i,j1)) & 
!c &                     + u2(i2,j2) - u2(i2,j1) ) &
!c &                   * unix * cos((rlat(j1) + rlat(j2)) * 0.5d0) &
        relv(i,j) = &
&         - 0.125d0 * ( u2(i1,j1) + u2(i1,j ) + 2.d0 * (u2(i ,j1) + u2(i ,j )) &
&                     + u2(i2,j1) + u2(i2,j ) ) &
&                   * unix * cos((rlat(j1) + rlat(j )) * 0.5d0) &
&         + 0.125d0 * ( u2(i1,j2) + u2(i1,j ) + 2.d0 * (u2(i ,j2) + u2(i ,j )) &
&                     + u2(i2,j2) + u2(i2,j ) ) &
&                   * unix * cos((rlat(j2) + rlat(j )) * 0.5d0) &
&         + 0.125d0 * ( v2(i2,j1) - v2(i1,j1) + 2.d0 * (v2(i2,j ) - v2(i1,j )) & 
&                     + v2(i2,j2) - v2(i1,j2) ) &
&                   * (ER * (rlat(j1) - rlat(j2)) * 0.5d0)

        relv(i,j) = relv(i,j) &
&                 / ( 0.5d0 * unix * ( cos((rlat(j1) + rlat(j )) * 0.5d0) &
&                                    + cos((rlat(j2) + rlat(j )) * 0.5d0) ) &
&                 * (ER * (rlat(j1) - rlat(j2)) * 0.5d0) )
      else if (u2(i ,j1) /= rmiss.and.u2(i ,j ) /= rmiss.and. &
&              u2(i ,j2) /= rmiss.and. &
&              v2(i1,j ) /= rmiss.and.v2(i2,j ) /= rmiss) then

        relv(i,j) = &
&                 - 0.5d0 * (u2(i ,j1) + u2(i ,j )) &
&                         * unix * cos((rlat(j1) + rlat(j )) * 0.5d0) &
&                 + 0.5d0 * (u2(i ,j2) + u2(i ,j )) &
&                         * unix * cos((rlat(j2) + rlat(j )) * 0.5d0) &
&                 + 0.5d0 * (v2(i2,j) - v2(i1,j)) &
&                         * (ER * (rlat(j1) - rlat(j2)) * 0.5d0)
        relv(i,j) = relv(i,j) &
&                 / ( 0.5d0 * unix * ( cos((rlat(j1) + rlat(j )) * 0.5d0) &
&                                    + cos((rlat(j2) + rlat(j )) * 0.5d0) ) &
&                 * (ER * (rlat(j1) - rlat(j2)) * 0.5d0) )
      else
        relv(i,j) = rmiss
      endif
    enddo !" i
  enddo !" j

  !" ここから極での計算 polar
  polen = 0.d0
  poles = 0.d0
  do i = 1, imax
    if (u2(i,1   ) == rmiss) then
      polen = rmiss
      exit
    else
      polen = polen + u2(i,1   ) * unix * cos(rlat(1   ))
    endif
  enddo !" i
  do i = 1, imax
    if (u2(i,jmax) == rmiss) then
      poles = rmiss
      exit
    else
      poles = poles - u2(i,jmax) * unix * cos(rlat(jmax))
    endif
  enddo !" i

  if (polen /= rmiss) then
    polen = polen / (pi * (ER * cos(rlat(1   )))**2)
  endif
  if (poles /= rmiss) then
    poles = poles / (pi * (ER * cos(rlat(jmax)))**2)
  endif
  !" values for j=1&jmax are calculated by interpolation
  !" of values for j=2&jmax-1 and pole's value
  do i = 1, imax
    relv(i,1   ) = ( polen * (rlat(1) - rlat(2)) &
&                  + relv(i,2) * (0.5d0 * pi - rlat(1)) ) &
&                / (0.5d0 * pi - rlat(2))
  enddo !" i
  do i = 1, imax
    relv(i,jmax) = ( poles * (rlat(jmax-1) - rlat(jmax)) &
&                  + relv(i,jmax-1) * (rlat(jmax) + 0.5d0 * pi) ) &
&                / (rlat(jmax-1) + 0.5d0 * pi)
  enddo !" i

  return
end subroutine relvor

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine thz
!c
!c  指定温位面上のd theta/d pを計算するサブルーチン
!c=====
subroutine thz(th, rth, plev, dtdp, rmiss, imax, jmax, kmax)
  !c+++ [input]
  real(kind=r4b), intent(in)    :: th(imax,jmax,kmax)     !" 温位
  real(kind=r4b), intent(in)    :: rth                    !" 指定温位面(この温位上のdtheta/dpを求める)
  real(kind=r4b), intent(in)    :: plev(imax,jmax,kmax)   !" pressure levels
  real(kind=r4b), intent(in)    :: rmiss                  !" missing value
  integer(kind=i4b), intent(in) :: imax, jmax, kmax       !" x-, y- z- axis sizes
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dtdp(imax,jmax)        !" 温位面上のd theta/d p
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k                !"
  integer(kind=i4b)             :: ip2                    !" from pseek
  real(kind=r4b)                :: sfp                    !" from pseek
  real(kind=r4b)                :: plev0(100)             !" to pseek
  real(kind=r4b)                :: zp1, zp2, zp3, zp4     !"
  integer(kind=i4b)             :: ip1, ip3, ip4          !"
  real(kind=r4b)                :: zth1, zth2, zth3, zth4 !"

  do j = 1, jmax
    do i = 1, imax
      do k = 1, kmax
        plev0(k) = plev(i,j,k)
      enddo !" k
      !c+++ ==> sfp, ip2
      call pseek(th, i, j, rth, plev0, sfp, ip2, rmiss, imax, jmax, kmax)

      if (ip2 /= rmiss) then
        ip1 = ip2 - 1
        ip3 = ip2 + 1
        ip4 = ip2 + 2
      else
        ip1 = rmiss
        ip3 = rmiss
        ip4 = rmiss
      endif

      if (ip1 >= 1.and.ip1 <= kmax) then
        zth1 =   th(i,j,ip1)
        zp1  = plev(i,j,ip1)
      else
        zth1 = rmiss
        zp1  = rmiss
      endif
      if (ip2 >= 1.and.ip2 <= kmax) then
        zth2 =   th(i,j,ip2)
        zp2  = plev(i,j,ip2)
      else
        zth2 = rmiss
        zp2  = rmiss
      endif
      if (ip3 >= 1.and.ip3 <= kmax) then
        zth3 =   th(i, j, ip3)
        zp3  = plev(i, j, ip3)
      else
        zth3 = rmiss
        zp3  = rmiss
      endif
      if (ip4 >= 1.and.ip4 <= kmax) then
        zth4 =   th(i, j, ip4)
        zp4  = plev(i, j, ip4)
      else
        zth4 = rmiss
        zp4  = rmiss
      endif

      !c+++ 内挿 ==> dtdp
      call thzsub(zth1, zth2, zth3, zth4, sfp, zp1, zp2, zp3, zp4, dtdp(i,j), rmiss)
    enddo !" i
  enddo !" j

  return
end subroutine thz

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine thzsub
!c
!c  鉛直内挿を行うサブルーチン
!c=====
subroutine thzsub(zth1, zth2, zth3, zth4, sfp, zp1, zp2, zp3, zp4, thgrad, rmiss)
  !c+++ [input]
  real(kind=r4b), intent(in)  :: zth1, zth2, zth3, zth4 !"
  real(kind=r4b), intent(in)  :: sfp                    !"
  real(kind=r4b), intent(in)  :: zp1, zp2, zp3, zp4     !"
  real(kind=r4b), intent(in)  :: rmiss                  !" missing value
  !c+++ [output]
  real(kind=r4b), intent(out) :: thgrad                 !" dth/dp

  real(kind=r4b)              :: zf, z1f                !"
  real(kind=r4b)              :: zgu, zgl, zgc          !"
  real(kind=r4b)              :: zwu, zwl, zwc          !"
  integer(kind=i4b)           :: ivert                  !" 鉛直内挿をどの方法でやるか
  data ivert / 12 /   !" 2:cubic, 3:deep linear +10: with logP

  if (zp1 /= rmiss.and.zp2 /= rmiss.and.zp3 /= rmiss.and.zp4 /= rmiss) then
    if (ivert < 10) then
      zgl = (zth3 - zth1) / (zp3 - zp1)
      zgu = (zth4 - zth2) / (zp4 - zp2)
      zgc = (zth3 - zth2) / (zp3 - zp2)
    else
      zgl = (zth3 - zth1) / (log(zp3) - log(zp1))
      zgu = (zth4 - zth2) / (log(zp4) - log(zp2))
      zgc = (zth3 - zth2) / (log(zp3) - log(zp2))
    endif
    zf = (sfp - zp2) / (zp3 - zp2)
    if (ivert == 2.or.ivert == 12) then
      z1f = 1.0 - zf
      zwl = z1f - 3.0 * zf * z1f
      zwu = zf - 3.0 * zf * z1f
      zwc = 1.0 - (zwu + zwl)
      thgrad = 0.01 * (zwl * zgl + zwu * zgu + zwc * zgc)
    else
      thgrad = 0.01 * (zgl + zf * (zgu - zgl))  !" deep linear
    endif
!ccc  zf=(log(sfp)-log(zp2))/(log(zp3)-log(zp2))
!ccc  z1f=1.0-zf
!ccc  zwl=z1f-3.0*zf*z1f
!ccc  zwu=zf-3.0*zf*z1f
!ccc  zwc=1.0-(zwu+zwl)
!ccc  thgrad=0.01*(zwl*zgl+zwu*zgu+zwc*zgc)/sfp
    if (ivert > 10) then
      thgrad = thgrad / sfp
    endif
  elseif (zp2 /= rmiss.and.zp3 /= rmiss) then
    thgrad = 0.01 * (zth3 - zth2) / (log(zp3) - log(zp2)) / sfp
  else
    thgrad = rmiss
  endif

  return
end subroutine thzsub

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine pseek
!c
!c  指定温位面上の圧力を計算するサブルーチン
!c=====
subroutine pseek(th, i, j, rth, plev0, p2, k, rmiss, imax, jmax, kmax)
  !c+++ [input]
  real(kind=r4b), intent(in)     :: th(imax,jmax,kmax)  !" 温位
  integer(kind=i4b), intent(in)  :: i, j
  real(kind=r4b), intent(in)     :: rth                 !" 指定温位面(この温位上の圧力を求める)
  real(kind=r4b), intent(in)     :: plev0(100)          !" 圧力レベル(hPa)
  real(kind=r4b), intent(in)     :: rmiss               !" missing value
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax    !" x-, y- z- axis sizes
  !c+++ [output]
  real(kind=r4b), intent(out)    :: p2                  !" 圧力(hPa)
  integer(kind=i4b), intent(out) :: k                   !"
  !c+++ [internal work]

  k = 1
  if (th(i,j,k) > rth) then
    k  = rmiss
    p2 = rmiss
    return
  endif

 110 continue
  if (th(i,j,k+1) >= rth) then
    p2 = exp(log(plev0(k)) + (log(plev0(k+1)) - log(plev0(k))) &
&                           * (rth - th(i,j,k)) / (th(i,j,k+1) - th(i,j,k)) )
  else if (k < kmax-1) then
    k = k + 1
    goto 110
  else
    k  = rmiss
    p2 = rmiss
  endif

  return
end subroutine pseek

!c----------------------------------------------------------------------c

!c======================================================================c

end module util_pvont
