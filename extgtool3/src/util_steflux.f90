!c
!c  module util_steflux
!c  [history]
!c  2013/03/27 Yamashita: first ver. from pvseek in pvont.f by S. Sugata (2001/04/02)
!c  2013/03/28 Yamashita: f77 ==> f90
!c  2016/11/14 Yamashita: conv_t2th
!c  2016/11/15 Yamashita: trop_wmo, trop_miroc, trop_pv
!c  2016/11/16 Yamashita: conv_th2t
!c  2016/11/18 Yamashita: trop_lrtpv, trop_tmin
!c  2016/11/30 Yamashita: smth9, minmax
!c  2017/07/28 Yamashita: use common_typedef
!c
!c  public: conv_t2th 温度 ==> 温位
!c  public: conv_th2t 温位 ==> 温度
!c  public: trop_wmo 圏界面高度の計算(WMO)
!c  public: trop_miroc 圏界面高度の計算(MIROC)
!c  public: trop_tmin 最低温度で定義した圏界面高度の計算
!c  public: trop_pv 圏界面高度の計算(2 PVU)
!c    internal: varonpv　温位面上の変数
!c  public: trop_lrtpv 圏界面高度の計算(3.5 PVU < LRT < 380 K)
!c    internal: trop_wmo, trop_pv
!c  public: pvseek2
!c    internal: varont2　温位面上の変数
!c    internal: relvor　相対渦度
!c    internal: thz　dth/dp
!c      internal: pseek 指定温位面上の圧力を計算
!c      internal: thzsub 鉛直内挿
!c    internal smth9: smoothing filter
!c    internal minmax: eliminate extream values
!c
!c======================================================================c
module util_steflux
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: conv_t2th, conv_th2t, trop_wmo, trop_miroc, trop_tmin, trop_pv, trop_lrtpv, pvseek2

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine conv_t2th
!c
!c=====
subroutine conv_t2th(imax, jmax, kmax, rmiss, T, pres, TH, rkappa)
  !c+++ [parameter]
  real(kind=r8b), parameter     :: P00  = 1000.d0       !! reference surface pressure [hPa]
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-sizes
  real(kind=r4b), intent(in)    :: rmiss                !! missing value
  real(kind=r4b), intent(in)    :: T(imax,jmax,kmax)    !! T (K)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! p (hPa)
  real(kind=r8b), intent(in)    :: rkappa               !! rkappa = rd / cp
  !c+++ [output]
  real(kind=r4b), intent(out)   :: TH(imax,jmax,kmax)   !! THETA (K)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k              !!

  !c+++ for eta-level data
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        !c+++ TH = T * (Ps/p)**KAP
        if (T(i,j,k) /= rmiss.and.pres(i,j,k) /= rmiss) then
          TH(i,j,k) = T(i,j,k) * (P00 / pres(i,j,k)) ** rkappa
        else
          TH(i,j,k) = rmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine conv_t2th

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine conv_th2t
!c
!c=====
subroutine conv_th2t(imax, jmax, kmax, rmiss, TH, pres, T, rkappa)
  !c+++ [parameter]
  real(kind=r8b), parameter     :: P00  = 1000.d0       !! reference surface pressure [hPa]
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-sizes
  real(kind=r4b), intent(in)    :: rmiss                !! missing value
  real(kind=r4b), intent(in)    :: TH(imax,jmax,kmax)   !! THETA (K)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! p (hPa)
  real(kind=r8b), intent(in)    :: rkappa               !! rkappa = rd / cp
  !c+++ [output]
  real(kind=r4b), intent(out)   :: T(imax,jmax,kmax)    !! T (K)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k              !!

  !c+++ for eta-level data
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        !c+++ T = TH * (p/Ps)**KAP
        if (TH(i,j,k) /= rmiss) then
          T(i,j,k) = TH(i,j,k) * (pres(i,j,k) / P00) ** rkappa
        else
          T(i,j,k) = rmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine conv_th2t

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine trop_wmo
!c  2013/09/20: Yamashita: first ver. in gttrop
!c  2016/11/18: Yamashita: modify lapse rate calc.
!c  LRT of WMO
!c=====
subroutine trop_wmo(nx, ny, np, rmiss, T, Z, P, kout, pout, tout, zout)
  !c+++ [parameter]
  real(kind=r8b), parameter      :: tp_max = 400.d0  !! max. pres. of trop. [hPa] PSTRMX
  real(kind=r8b), parameter      :: tp_min = 50.d0   !! min. pres. of trop. [hPa] PSTRMN
  real(kind=r8b), parameter      :: str_lr = 2.d-3   !! crit. dT/dz lapse rate [K/m]
  real(kind=r8b), parameter      :: str_ly = 2000.d0 !! min. length of tropopause [m]
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np       !! z-, y-, z-sizes
  real(kind=r4b), intent(in)     :: rmiss            !! missing values
  real(kind=r4b), intent(in)     :: T(nx,ny,np)      !! T [K]
  real(kind=r4b), intent(in)     :: Z(nx,ny,np)      !! Z [m]
  real(kind=r4b), intent(in)     :: P(nx,ny,np)      !! p [hPa]
  !c+++ [output]
  integer(kind=i4b), intent(out) :: kout(nx,ny)      !! trop. grid-id
  real(kind=r4b), intent(out)    :: pout(nx,ny)      !! trop. p [hPa]
  real(kind=r4b), intent(out)    :: tout(nx,ny)      !! trop. T [K]
  real(kind=r4b), intent(out)    :: zout(nx,ny)      !! trop. Z [m]
  !c+++ [internal work]
  integer(kind=i4b)              :: i, j, k          !! loop variables
  integer(kind=i4b)              :: iflag(nx,ny)     !!
  real(kind=r4b)                 :: minlev(nx,ny)    !! min. lev. of trop.
  real(kind=r4b)                 :: maxlev(nx,ny)    !! max. lev. of trop.
  real(kind=r8b)                 :: gdtm1, gdtm2     !! temp.
  real(kind=r8b)                 :: gdzm1, gdzm2     !! gpm
  real(kind=r8b)                 :: dtdz             !! dT/dz

  !c+++ init
  iflag(1:nx,1:ny) = 0
  kout(1:nx,1:ny) = np

  !c+++ dT/dZ
  do k = 2, np-1
    do j = 1, ny
      do i = 1, nx
        if (P(i,j,k) <= tp_max.and.P(i,j,k) >= tp_min.and.P(i,j,k) /= rmiss) then
          if ((T(i,j,k+1) /= rmiss).and.(T(i,j,k  ) /= rmiss).and.        &
&             (T(i,j,k-1) /= rmiss).and.(Z(i,j,k-1) /= rmiss).and.        &
&             (Z(i,j,k+1) /= rmiss).and.(Z(i,j,k  ) /= rmiss)) then   
            gdtm1 = exp(0.5d0 * (log(T(i,j,k+1)) + log(T(i,j,k  ))))
            gdtm2 = exp(0.5d0 * (log(T(i,j,k  )) + log(T(i,j,k-1))))
            gdzm1 = exp(0.5d0 * (log(Z(i,j,k+1)) + log(Z(i,j,k  ))))
            gdzm2 = exp(0.5d0 * (log(Z(i,j,k  )) + log(Z(i,j,k-1))))
            dtdz  = -(gdtm1 - gdtm2) / (gdzm1 - gdzm2)
            !ccc dtdz = -(T(i,j,k+1)-T(i,j,k-1)) / (Z(i,j,k+1)-Z(i,j,k-1))
            if (dtdz <= str_lr) then
              iflag(i,j) = iflag(i,j) + 1
              kout(i,j) = min(k,kout(i,j)) !! save min. lev. of trop.
            endif 
          endif
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  do j = 1, ny
    do i = 1, nx
      if (iflag(i,j) == 0) then
        kout(i,j) = np
      else
        minlev(i,j) = Z(i,j,kout(i,j)) 
        maxlev(i,j) = Z(i,j,(kout(i,j)+iflag(i,j)-1)) 
        if ((maxlev(i,j) - minlev(i,j)) < str_ly) then !! thin layer
          kout(i,j) = np
        endif
      endif
      if (kout(i,j) /= np) then
        pout(i,j) = P(i,j,kout(i,j))
        tout(i,j) = T(i,j,kout(i,j))
        zout(i,j) = Z(i,j,kout(i,j))
      else
        pout(i,j) = rmiss
        tout(i,j) = rmiss
        zout(i,j) = rmiss
      endif
    enddo !! i
  enddo !! j

  return
end subroutine trop_wmo

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine trop_miroc
!c  LRT of MIROC
!c  [history]
!c  2013/09/20: Yamashita: first ver. in gttrop2
!c  2016/11/18: Yamashita: exact same thing to MIROC
!c=====
subroutine trop_miroc(nx, ny, np, rmiss, T, Z, P, kout, pout, tout, zout)
  !c+++ [parameter]
  real(kind=r8b), parameter      :: tp_max = 400.d0 !! max. pres. of trop. [hPa] PSTRMX
  real(kind=r8b), parameter      :: tp_min = 50.d0  !! min. pres. of trop. [hPa] PSTRMN
  real(kind=r8b), parameter      :: gcrstr = 1.d-4  !! crit. dT/dz tropopause [K/m]
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np      !! z-, y-, z-sizes
  real(kind=r4b), intent(in)     :: rmiss           !! missing values
  real(kind=r4b), intent(in)     :: T(nx,ny,np)     !! T [K]
  real(kind=r4b), intent(in)     :: Z(nx,ny,np)     !! Z [m]
  real(kind=r4b), intent(in)     :: P(nx,ny,np)     !! p [hPa]
  !c+++ [output]
  integer(kind=i4b), intent(out) :: kout(nx,ny)     !! trop. grid-id
  real(kind=r4b), intent(out)    :: pout(nx,ny)     !! trop. p [hPa]
  real(kind=r4b), intent(out)    :: tout(nx,ny)     !! trop. T [K]
  real(kind=r4b), intent(out)    :: zout(nx,ny)     !! trop. Z [m]
  !c+++ [internal work]
  integer(kind=i4b)              :: i, j, k         !! loop variables
  real(kind=r8b)                 :: gdtm1, gdtm2    !! temp.
  real(kind=r8b)                 :: gdzm1, gdzm2    !! gpm
  real(kind=r8b)                 :: gam             !! gamma

  !c+++ init
  kout(1:nx,1:ny) = np

  !c+++ dT/dZ
  do j = 1, ny
    do i = 1, nx
      do k = 1, np-1
        if ((T(i,j,k+1) /= rmiss).and.(T(i,j,k  ) /= rmiss).and.        &
&           (T(i,j,k-1) /= rmiss).and.(Z(i,j,k-1) /= rmiss).and.        &
&           (Z(i,j,k+1) /= rmiss).and.(Z(i,j,k  ) /= rmiss)) then   
          gdtm1 = exp(0.5d0 * (log(T(i,j,k+1)) + log(T(i,j,k  ))))
          gdtm2 = exp(0.5d0 * (log(T(i,j,k  )) + log(T(i,j,k-1))))
          gdzm1 = exp(0.5d0 * (log(Z(i,j,k+1)) + log(Z(i,j,k  ))))
          gdzm2 = exp(0.5d0 * (log(Z(i,j,k  )) + log(Z(i,j,k-1))))
          gam = (gdtm1 - gdtm2) / (gdzm1 - gdzm2)
          !ccc gam = (T(i,j,k+1) - T(i,j,k)) / (Z(i,j,k+1) - Z(i,j,k))
        else
          gam = 0.0
        endif
        if ((P(i,j,k) < tp_max.and.gam > gcrstr).or.P(i,j,k) < tp_min) then
          kout(i,j) = min(k,kout(i,j)) !! save min. lev.
        endif
      enddo !! k
      if (kout(i,j) /= np) then
        pout(i,j) = P(i,j,kout(i,j))
        tout(i,j) = T(i,j,kout(i,j))
        zout(i,j) = Z(i,j,kout(i,j))
      else
        pout(i,j) = rmiss
        tout(i,j) = rmiss
        zout(i,j) = rmiss
      endif
    enddo !! i
  enddo !! j

  return
end subroutine trop_miroc

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine trop_tmin
!c  2016/11/18: Yamashita: first ver.
!c  LRT of WMO
!c=====
subroutine trop_tmin(nx, ny, np, rmiss, T, Z, P, kout, pout, tout, zout)
  !c+++ [parameter]
  real(kind=r8b), parameter      :: tp_max = 400.d0  !! max. pres. of trop. [hPa] PSTRMX
  real(kind=r8b), parameter      :: tp_min = 50.d0   !! min. pres. of trop. [hPa] PSTRMN
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np       !! z-, y-, z-sizes
  real(kind=r4b), intent(in)     :: rmiss            !! missing values
  real(kind=r4b), intent(in)     :: T(nx,ny,np)      !! T [K]
  real(kind=r4b), intent(in)     :: Z(nx,ny,np)      !! Z [m]
  real(kind=r4b), intent(in)     :: P(nx,ny,np)      !! p [hPa]
  !c+++ [output]
  integer(kind=i4b), intent(out) :: kout(nx,ny)      !! trop. grid-id
  real(kind=r4b), intent(out)    :: pout(nx,ny)      !! trop. p [hPa]
  real(kind=r4b), intent(out)    :: tout(nx,ny)      !! trop. T [K]
  real(kind=r4b), intent(out)    :: zout(nx,ny)      !! trop. Z [m]
  !c+++ [internal work]
  integer(kind=i4b)              :: i, j, k          !! loop variables
  real(kind=r4b)                 :: tmin(nx,ny)      !! min. temp. at trop.

  !c+++ init
  kout(1:nx,1:ny) = np
  tmin(1:nx,1:ny) = 1000.0

  !c+++ min. of temperature
  do k = 2, np-1
    do j = 1, ny
      do i = 1, nx
        if (P(i,j,k) <= tp_max.and.P(i,j,k) >= tp_min.and.P(i,j,k) /= rmiss) then
          if (T(i,j,k) /= rmiss.and.T(i,j,k) < tmin(i,j)) then
            tmin(i,j) = T(i,j,k)
            kout(i,j) = k !! save min. lev. of trop.
          endif
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  do j = 1, ny
    do i = 1, nx
      if (kout(i,j) /= np) then
        pout(i,j) = P(i,j,kout(i,j))
        tout(i,j) = T(i,j,kout(i,j))
        zout(i,j) = Z(i,j,kout(i,j))
      else
        pout(i,j) = rmiss
        tout(i,j) = rmiss
        zout(i,j) = rmiss
      endif
    enddo !! i
  enddo !! j

  return
end subroutine trop_tmin

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine trop_pv
!c  2016/11/15: Yamashita: first ver.
!c  tropopause definition of PV surface
!c=====
subroutine trop_pv(nx, ny, nz, rmiss, rpv, pv, T, Z, P, pout, tout, zout)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, nz   !! z-, y-, z-sizes (z: num of theta-levs.)
  real(kind=r4b), intent(in)     :: rmiss        !! missing values
  real(kind=r4b), intent(in)     :: rpv          !! 指定PV面(PVU, このPV上の気圧を求める)
  real(kind=r4b), intent(in)     :: pv(nx,ny,nz) !! PV [PVU]
  real(kind=r4b), intent(in)     :: T(nx,ny,nz)  !! T [K]
  real(kind=r4b), intent(in)     :: Z(nx,ny,nz)  !! Z [m]
  real(kind=r4b), intent(in)     :: P(nx,ny,nz)  !! p [hPa]
  !c+++ [output]
  real(kind=r4b), intent(out)    :: pout(nx,ny)  !! trop. p [hPa]
  real(kind=r4b), intent(out)    :: tout(nx,ny)  !! trop. T [K]
  real(kind=r4b), intent(out)    :: zout(nx,ny)  !! trop. Z [m]
  !c+++ [internal work]

  pout(1:nx,1:ny) = rmiss
  tout(1:nx,1:ny) = rmiss
  zout(1:nx,1:ny) = rmiss

  !c+++ ==> T on PV
  call varonpv(nx, ny, nz, rmiss, T, pv, rpv, tout, .true.)
  !c+++ ==> P on PV
  call varonpv(nx, ny, nz, rmiss, P, pv, rpv, pout, .true.)
  !c+++ ==> Z on PV
  call varonpv(nx, ny, nz, rmiss, Z, pv, rpv, zout, .true.)

  return
end subroutine trop_pv

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine trop_lrtpv
!c  2016/11/18: Yamashita: first ver.
!c  tropopause definition of PV surface
!c  PVUの圏界面を使用
!c  LRTの圏界面がPVUの圏界面を下回る時はLRTの圏界面を使用
!c  380 Kの高さを上回る時は、380 K面を使用
!c  e.g. 3.5 PVU < LRT < 380 K
!c=====
subroutine trop_lrtpv(nx, ny, np, nth, ilevub, rmiss, rpv, &
&  pv_th, T_th, Z_th, P_th, T, Z, P, kout, pout, tout, zout)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nx, ny, np       !! z-, y-, z-sizes (z: num of p-levs.)
  integer(kind=i4b), intent(in)  :: nth              !! th-sizes (z: num of theta-levs.)
  integer(kind=i4b), intent(in)  :: ilevub           !! level of upper boundary
  real(kind=r4b), intent(in)     :: rmiss            !! missing values
  real(kind=r4b), intent(in)     :: rpv              !! 指定PV面(PVU, このPV上の気圧を求める)
  real(kind=r4b), intent(in)     :: pv_th(nx,ny,nth) !! 温位面PV [PVU]
  real(kind=r4b), intent(in)     :: T_th(nx,ny,nth)  !! 温位面T [K]
  real(kind=r4b), intent(in)     :: Z_th(nx,ny,nth)  !! 温位面Z [m]
  real(kind=r4b), intent(in)     :: P_th(nx,ny,nth)  !! 温位面p [hPa]
  real(kind=r4b), intent(in)     :: T(nx,ny,np)      !! 気圧面T [K]
  real(kind=r4b), intent(in)     :: Z(nx,ny,np)      !! 気圧面Z [m]
  real(kind=r4b), intent(in)     :: P(nx,ny,np)      !! 気圧面p [hPa]
  !c+++ [output]
  integer(kind=i4b), intent(out) :: kout(nx,ny)      !! trop. grid-id
  real(kind=r4b), intent(out)    :: pout(nx,ny)      !! trop. p [hPa]
  real(kind=r4b), intent(out)    :: tout(nx,ny)      !! trop. T [K]
  real(kind=r4b), intent(out)    :: zout(nx,ny)      !! trop. Z [m]
  !c+++ [internal work]
  integer(kind=i4b)              :: i, j, k          !!
  !c+++ PV380K
  integer(kind=i4b)              :: kout0(nx,ny)     !! frop. flag of PV380K
  !c+++ PV
  integer(kind=i4b)              :: kout1(nx,ny)     !! trop. grid-id PV
  real(kind=r4b)                 :: pout1(nx,ny)     !! trop. p [hPa] PV
  real(kind=r4b)                 :: tout1(nx,ny)     !! trop. T [K] PV
  real(kind=r4b)                 :: zout1(nx,ny)     !! trop. Z [m] PV
  !c+++ LRT
  integer(kind=i4b)              :: kout2(nx,ny)     !! trop. grid-id LRT
  real(kind=r4b)                 :: pout2(nx,ny)     !! trop. p [hPa] LRT
  real(kind=r4b)                 :: tout2(nx,ny)     !! trop. T [K] LRT
  real(kind=r4b)                 :: zout2(nx,ny)     !! trop. Z [m] LRT

  !c+++ init
  kout(1:nx,1:ny) = np
  pout(1:nx,1:ny) = rmiss
  tout(1:nx,1:ny) = rmiss
  zout(1:nx,1:ny) = rmiss
  do j = 1, ny
    do i = 1, nx
      !c+++ not rmiss, kout0 = 0
      if (P_th(i,j,ilevub) /= rmiss.and.T_th(i,j,ilevub) /= rmiss.and.T_th(i,j,ilevub) /= rmiss) then
        kout0(i,j) = 0
      else
        kout0(i,j) = np
      endif
    enddo !! i
  enddo !! j

  !c+++ PV trop. ==> T, P, Z on 'rpv' PV
  call trop_pv(nx, ny, nth, rmiss, rpv, pv_th, T_th, Z_th, P_th, pout1, tout1, zout1)
  do j = 1, ny
    do i = 1, nx
      !c+++ not rmiss, kout1 = 0
      if (pout1(i,j) /= rmiss.and.zout1(i,j) /= rmiss.and.tout1(i,j) /= rmiss) then
        kout1(i,j) = 0
      else
        kout1(i,j) = np
      endif
    enddo !! i
  enddo !! j

  !c+++ LRT of WMO
  call trop_wmo(nx, ny, np, rmiss, T, Z, P, kout2, pout2, tout2, zout2)

  !c+++  e.g. 3.5 PVU < LRT < 380 K
  do j = 1, ny
    do i = 1, nx
      if (kout0(i,j) /= np.and.kout2(i,j) /= np.and.Z_th(i,j,ilevub) < zout2(i,j)) then
        !c+++ LRT < 380 K,  PV380 is used
        kout2(i,j) = 0
        pout2(i,j) = P_th(i,j,ilevub)
        tout2(i,j) = T_th(i,j,ilevub)
        zout2(i,j) = Z_th(i,j,ilevub)
      else if (kout0(i,j) /= np.and.kout2(i,j) == np) then
        !c+++ in case, LRT: rmiss,  PV380 is used
        kout2(i,j) = 0
        pout2(i,j) = P_th(i,j,ilevub)
        tout2(i,j) = T_th(i,j,ilevub)
        zout2(i,j) = Z_th(i,j,ilevub)
      endif

      !c+++  e.g. 3.5 PVU < LRT
      if (kout1(i,j) == np.and.kout2(i,j) == np) then
        !c+++ in case, PV380: rmiss
        kout(i,j) = np
        pout(i,j) = rmiss
        tout(i,j) = rmiss
        zout(i,j) = rmiss
      else if (kout1(i,j) == np.and.kout2(i,j) /= np) then
        !c+++ in case, PV: rmiss, LRT: defined,  LRT is used
        kout(i,j) = kout2(i,j)
        pout(i,j) = pout2(i,j)
        tout(i,j) = tout2(i,j)
        zout(i,j) = zout2(i,j)
      else if (kout1(i,j) /= np.and.kout2(i,j) == np) then
        !c+++ in case, PV: rmiss, LRT: defined, PV is used
        kout(i,j) = kout1(i,j) 
        pout(i,j) = pout1(i,j) 
        tout(i,j) = tout1(i,j)
        zout(i,j) = zout1(i,j)
      else
        if (zout1(i,j) > zout2(i,j)) then
          !c+++ e.g. 3.5 PVU > LRT, LRT is used as tropopause
          kout(i,j) = kout2(i,j)
          pout(i,j) = pout2(i,j)
          tout(i,j) = tout2(i,j)
          zout(i,j) = zout2(i,j)
        else
          !c+++ e.g. LRT > 3.5 PVU, 3.5 PVU is used as tropopause
          kout(i,j) = kout1(i,j)
          pout(i,j) = pout1(i,j)
          tout(i,j) = tout1(i,j)
          zout(i,j) = zout1(i,j)
        endif
      endif
      if (kout(i,j) == 0) then
        !c+++ calc. kout
        do k = 1, np
          kout(i,j) = k
          if (pout(i,j) >= P(i,j,k)) exit
        enddo !! k
      endif
    enddo !! i
  enddo !! j

  return
end subroutine trop_lrtpv

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine pvseek
!c
!c  PVを求めるサブルーチンメイン
!c=====
subroutine pvseek2(imax, jmax, kmax, rmiss, uu, vv, tt, zz, th, rth, pres, diab, rlat, &
&  pv_th, pv_thpv, p_th, mf_th, t_th, z_th, &
&  g, ar, rkappa, PVU)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !" x-, y- z- axis sizes
  real(kind=r4b), intent(in)    :: rmiss                !" missing value
  real(kind=r4b), intent(in)    :: uu(imax,jmax,kmax)   !" 東西風(m/s)
  real(kind=r4b), intent(in)    :: vv(imax,jmax,kmax)   !" 南北風(m/s)
  real(kind=r4b), intent(in)    :: tt(imax,jmax,kmax)   !" 気温(K)
  real(kind=r4b), intent(in)    :: zz(imax,jmax,kmax)   !" ジオポテンシャル高度(m)
  real(kind=r4b), intent(in)    :: th(imax,jmax,kmax)   !" 温位 (K)
  real(kind=r4b), intent(in)    :: rth                  !" 指定温位面(K, この温位上のpvを求める)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !" pressure levels (hPa)
  real(kind=r4b), intent(in)    :: diab(imax,jmax,kmax) !" diabatic heating rate J (K/s)
  real(kind=r4b), intent(in)    :: rlat(jmax)           !" 緯度 (rad)
  real(kind=r8b), intent(in)    :: g                    !" 重力加速度(m/s2)
  real(kind=r8b), intent(in)    :: ar                   !" 地球半径(m)
  real(kind=r8b), intent(in)    :: rkappa               !" rkappa = rd / cp
  real(kind=r8b), intent(in)    :: PVU                  !" 1 [PVU] = 1e-6 [K m2/s/kg]
  !c+++ [output]
  real(kind=r4b), intent(out)   :: pv_th(imax,jmax)     !" 求めた温位(PV) (K m2/s/kg)
  real(kind=r4b), intent(out)   :: pv_thpv(imax,jmax)   !" 求めた温位(PV) (PVU)
  real(kind=r4b), intent(out)   :: p_th(imax,jmax)      !" 温位に対応する気圧面(hPa)
  real(kind=r4b), intent(out)   :: mf_th(imax,jmax)     !" 温位に対応する質量フラックス(kg/m2/s)
  real(kind=r4b), intent(out)   :: t_th(imax,jmax)      !" 温位に対応する気温(K)
  real(kind=r4b), intent(out)   :: z_th(imax,jmax)      !" 温位に対応するジオポテンシャル高度(m)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j                 !"
  !c+++ theta-lev
  real(kind=r4b)                :: u2(imax,jmax)        !" 温位面上のu
  real(kind=r4b)                :: v2(imax,jmax)        !" 温位面上のv
  real(kind=r4b)                :: uabs(imax,jmax)      !" 温位面上のsqrt(u**2+v**2)
  real(kind=r4b)                :: relv(imax,jmax)      !" 温位面上の相対渦度
  real(kind=r4b)                :: dtdp(imax,jmax)      !" 温位面上のd theta/d p
  !c+++ original-lev
  real(kind=r4b), allocatable   :: qq(:,:,:)            !" diabatic heating rate Q (K/s)
  !c+++ [internal const]
  real(kind=r8b)                :: pi                   !" 円周率
  real(kind=r8b)                :: romega               !" 地球回転角速度
  real(kind=r8b)                :: cs(jmax)             !" cos(lat)
  real(kind=r8b)                :: ff(jmax)             !" コリオリパラメター f = 2 * omega * sin(lat)
  real(kind=r8b)                :: coef                 !"

  !c+++ set up
  pi = atan(1.d0) * 4.d0
  romega = 2.d0 * pi / 86400.d0
  !ccc write(6,*) 'Omega = ', romega
  do j = 1, jmax
    cs(j) = cos(dble(rlat(j)))
    ff(j) = 2.d0 * romega * sin(dble(rlat(j)))
  enddo !" j

  !c+++ ==> u, v on theta
  call varont2(imax, jmax, kmax, rmiss, uu, th, rth, u2, .false.)
  call varont2(imax, jmax, kmax, rmiss, vv, th, rth, v2, .false.)
  !c+++ ==> p on theta
  call varont2(imax, jmax, kmax, rmiss, pres, th, rth, p_th, .true.)
  !c+++ ==> mf on theta
  allocate(qq(imax,jmax,kmax))
  qq(1:imax,1:jmax,1:kmax) = diab(1:imax,1:jmax,1:kmax)
  !ccc call smth9(imax, jmax, kmax, rmiss, qq)
  call varont2(imax, jmax, kmax, rmiss, qq, th, rth, mf_th, .false.)
  !call smth9(imax, jmax, kmax, rmiss, qq, qq)
  !call varont2(imax, jmax, kmax, rmiss, qq, th, rth, mf_th, .false.)
  !call varont2(imax, jmax, kmax, rmiss, diab, th, rth, mf_th, .false.)
  !ccc call smth9(imax, jmax, 1, rmiss, mf_th, mf_th)
  deallocate(qq)
  !c+++ ==> t on theta
  call varont2(imax, jmax, kmax, rmiss, tt, th, rth, t_th, .false.)
  !c+++ ==> z on theta
  call varont2(imax, jmax, kmax, rmiss, zz, th, rth, z_th, .false.)

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
  call relvor(u2, v2, rlat, relv, rmiss, imax, jmax, ar)
  !c+++ dth/dp
  call thz(th, rth, pres, dtdp, rmiss, imax, jmax, kmax)

  !c+++ ==> PV on theta
  do j = 1, jmax
    do i = 1, imax
      if (relv(i,j) /= rmiss.and.dtdp(i,j) /= rmiss) then
        pv_th(i,j) = - g * (relv(i,j) + ff(j)) * dtdp(i,j) 
        pv_thpv(i,j) = pv_th(i,j) * PVU
      else
        pv_th(i,j) = rmiss
        pv_thpv(i,j) = rmiss
      endif
    enddo !" i
  enddo !" j

  !c+++ ==> mf on theta (-cos(lat) * dQdt/g * dp/dtheta)
  do j = 1, jmax
    do i = 1, imax
      if (mf_th(i,j) /= rmiss.and.p_th(i,j) /= rmiss.and.dtdp(i,j) /= rmiss) then
        coef = (p_th(i,j) / 1000.d0)**rkappa
        mf_th(i,j) = -1.d0 / g / dtdp(i,j) * (mf_th(i,j) / coef) * cs(j)
      else
        mf_th(i,j) = rmiss
      endif
    enddo !" i
  enddo !" j

  return
end subroutine pvseek2

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine varont2
!c  [history]
!c  2001/04/02 S. Sugata: subroutine varont (thintp.f)
!c  2005/11/27 Yoshiki  : subroutine varont (thintp.f)
!c  2013/03/27 Yamashita: f77 ==> f90
!c  2016/11/15 Yamashita: logl option
!c
!c  p面上の変数から温位面上の変数を計算するサブルーチン
!c=====
subroutine varont2(imax, jmax, kmax, rmiss, dd, th, rth, d2, logl)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax   !" x-, y- z- axis sizes
  real(kind=r4b), intent(in)    :: rmiss              !" missing value
  real(kind=r4b), intent(in)    :: dd(imax,jmax,kmax) !" din
  real(kind=r4b), intent(in)    :: th(imax,jmax,kmax) !" theta
  real(kind=r4b), intent(in)    :: rth                !" 指定温位面(この温位上の圧力を求める)
  logical, intent(in)           :: logl               !" logl option
  !c+++ [output]
  real(kind=r4b), intent(out)   :: d2(imax,jmax)      !" PV面上の変数
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k            !"

  d2(1:imax,1:jmax) = rmiss
  do j = 1, jmax
    do i = 1, imax
      if (th(i,j,1) > rth) then !" theta(z=1)が指定温位より大
        cycle
      else if (th(i,j,kmax) < rth) then !" theta(kmax)が指定温位より小
        cycle
      endif

      do k = 1, kmax-1
        if (th(i,j,k+1) >= rth) then !" th(k+1) >= rth > th(k)
          if (dd(i,j,k) /= rmiss.and.dd(i,j,k+1) /= rmiss &
&           .and.th(i,j,k) /= rmiss.and.th(i,j,k+1)/= rmiss) then
            if (logl) then
              !c+++ linear interpolation (log)
              d2(i,j) = exp(log(dd(i,j,k)) &
&                     + (log(dd(i,j,k+1)) - log(dd(i,j,k))) &
&                     * (rth - th(i,j,k)) / (th(i,j,k+1) - th(i,j,k)) )
            else
              !c+++ linear interpolation
              d2(i,j) = dd(i,j,k) &
&                     + (dd(i,j,k+1) - dd(i,j,k)) &
&                     * (rth - th(i,j,k)) / (th(i,j,k+1) - th(i,j,k))
            endif
          else
            d2(i,j) = rmiss
          endif
          exit
        endif
      enddo !" k
    enddo !" i
  enddo !" j

  return
end subroutine varont2

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine varonpv
!c  [history]
!c  2016/11/15 Yamashita: from varont
!c
!c  温位面上の変数からPV面上の変数を計算するサブルーチン
!c=====
subroutine varonpv(imax, jmax, kmax, rmiss, dd, pv, rpv, d2, logl)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax   !" x-, y- z- axis sizes
  real(kind=r4b), intent(in)     :: rmiss              !" missing value
  real(kind=r4b), intent(in)     :: dd(imax,jmax,kmax) !" din
  real(kind=r4b), intent(in)     :: pv(imax,jmax,kmax) !" PV
  real(kind=r4b), intent(in)     :: rpv                !" 指定PV面(このPV上の圧力を求める)
  logical, intent(in)            :: logl               !" logl option
  !c+++ [output]
  real(kind=r4b), intent(out)    :: d2(imax,jmax)      !" PV面上の変数
  !c+++ [internal work]
  integer(kind=i4b)              :: i, j, k            !"

  d2(1:imax,1:jmax) = rmiss
  do j = 1, jmax
    do i = 1, imax
      if (abs(pv(i,j,1)) > rpv) then !" pv(z=1)が指定PVより大
        cycle
      else if (abs(pv(i,j,kmax)) < rpv) then !" pv(kmax)が指定PVより小
        cycle
      endif

      do k = 1, kmax-1
        if (abs(pv(i,j,k+1)) >= rpv) then !" pv(k+1) >= rpv > pv(k)
          if (dd(i,j,k) /= rmiss.and.dd(i,j,k+1) /= rmiss &
&           .and.abs(pv(i,j,k)) /= rmiss.and.abs(pv(i,j,k+1)) /= rmiss) then
            if (logl) then
              !c+++ linear interpolation (log)
              d2(i,j) = exp(log(dd(i,j,k)) &
&                     + (log(dd(i,j,k+1)) - log(dd(i,j,k))) &
&                     * (rpv - abs(pv(i,j,k))) / (abs(pv(i,j,k+1)) - abs(pv(i,j,k))) )
            else
              !c+++ linear interpolation
              d2(i,j) = dd(i,j,k) &
&                     + (dd(i,j,k+1) - dd(i,j,k)) &
&                     * (rpv - abs(pv(i,j,k))) / (abs(pv(i,j,k+1)) - abs(pv(i,j,k)))
            endif
          else
            d2(i,j) = rmiss
          endif
          exit
        endif
      enddo !" k
    enddo !" i
  enddo !" j

  return
end subroutine varonpv

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c
!c  subroutine relvor
!c
!c  相対渦度を計算するサブルーチン
!c=====
subroutine relvor(u2, v2, rlat, relv, rmiss, imax, jmax, ER)
  !c+++ [input]
  real(kind=r4b), intent(in)    :: u2(imax,jmax)    !" 温位面上のu
  real(kind=r4b), intent(in)    :: v2(imax,jmax)    !" 温位面上のv
  real(kind=r4b), intent(in)    :: rlat(jmax)       !" 緯度 in radian
  real(kind=r4b), intent(in)    :: rmiss            !" missing value
  integer(kind=i4b), intent(in) :: imax, jmax       !" x-, y- axis sizes
  real(kind=r8b), intent(in)    :: ER               !" 地球半径(m)
  !c+++ [output]
  real(kind=r4b), intent(out)   :: relv(imax,jmax)  !" 温位面上の相対渦度
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j             !"
  integer(kind=i4b)             :: i1, i2, j1, j2   !"
  real(kind=r8b)                :: pi               !" 円周率
  real(kind=r8b)                :: unix             !" 赤道でのグリッド経度一つ分
  real(kind=r8b)                :: polen, poles     !" 北極点と南極点での循環の値

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
subroutine thz(th, rth, pres, dtdp, rmiss, imax, jmax, kmax)
  !c+++ [input]
  real(kind=r4b), intent(in)    :: th(imax,jmax,kmax)     !" 温位
  real(kind=r4b), intent(in)    :: rth                    !" 指定温位面(この温位上のdtheta/dpを求める)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax)   !" pressure levels
  real(kind=r4b), intent(in)    :: rmiss                  !" missing value
  integer(kind=i4b), intent(in) :: imax, jmax, kmax       !" x-, y- z- axis sizes
  !c+++ [output]
  real(kind=r4b), intent(out)   :: dtdp(imax,jmax)        !"温位面上のd theta/d p
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
        plev0(k) = pres(i,j,k)
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
        zp1  = pres(i,j,ip1)
      else
        zth1 = rmiss
        zp1  = rmiss
      endif
      if (ip2 >= 1.and.ip2 <= kmax) then
        zth2 =   th(i,j,ip2)
        zp2  = pres(i,j,ip2)
      else
        zth2 = rmiss
        zp2  = rmiss
      endif
      if (ip3 >= 1.and.ip3 <= kmax) then
        zth3 =   th(i, j, ip3)
        zp3  = pres(i, j, ip3)
      else
        zth3 = rmiss
        zp3  = rmiss
      endif
      if (ip4 >= 1.and.ip4 <= kmax) then
        zth4 =   th(i, j, ip4)
        zp4  = pres(i, j, ip4)
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
  !c+++ [internal work]
  real(kind=r4b)              :: zf, z1f                !"
  real(kind=r4b)              :: zgu, zgl, zgc          !"
  real(kind=r4b)              :: zwu, zwl, zwc          !"
  integer(kind=i4b)           :: ivert                  !" 鉛直内挿をどの方法でやるか
  data                           ivert / 12 /           !" 2:cubic, 3:deep linear +10: with logP

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

!c---------------------------------------------------------------------c
!c  subroutine smth9
!c  smoothing filter
!c
!c    i1  i2  i3
!c j1 0.3 0.5 0.3
!c j2 0.5 1.0 0.5
!c j3 0.3 0.5 0.3
!c
!c=====
subroutine smth9(nx, ny, nz, undef, dio)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz     !! x-, y- z- axis sizes
  real(kind=r4b), intent(in)    :: undef          !! missing value
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: dio(nx,ny,nz)  !! 
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k        !! loop variables
  integer(kind=i4b)             :: i1, i2, i3     !!
  integer(kind=i4b)             :: j1, j2, j3     !!
  real(kind=r4b)                :: din(nx,ny,nz)  !! input data
  real(kind=r8b)                :: work(nx,ny,nz) !!
  real(kind=r8b)                :: rsum           !!

  din(1:nx,1:ny,1:nz) = dio(1:nx,1:ny,1:nz)
  work(1:nx,1:ny,1:nz) = 0.0
  do k = 1, nz
    do j = 1, ny
      j1 = j - 1
      j2 = j
      j3 = j + 1
      if (j == 1) j1 = 1
      if (j == ny) j3 = ny
      do i = 1, nx
        rsum = 0.0
        i1 = i - 1
        i2 = i
        i3 = i + 1
        if (i == 1) i1 = nx
        if (i == nx) i3 = 1
        !c+++ weight = 1.0
        if (din(i2,j2,k) /= undef) then
          work(i,j,k) = work(i,j,k) + din(i2,j2,k)
          rsum = rsum + 1.d0
        endif
        !c+++ weight = 0.5
        if (din(i2,j1,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.5d0 * din(i2,j1,k)
          rsum = rsum + 0.5d0
        endif
        if (din(i1,j2,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.5d0 * din(i1,j2,k)
          rsum = rsum + 0.5d0
        endif
        if (din(i3,j2,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.5d0 * din(i3,j2,k)
          rsum = rsum + 0.5d0
        endif
        if (din(i2,j3,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.5d0 * din(i2,j3,k)
          rsum = rsum + 0.5d0
        endif
        !c+++ weight = 0.3
        if (din(i1,j1,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.3d0 * din(i1,j1,k)
          rsum = rsum + 0.3d0
        endif
        if (din(i3,j1,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.3d0 * din(i3,j1,k)
          rsum = rsum + 0.3d0
        endif
        if (din(i1,j3,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.3d0 * din(i1,j3,k)
          rsum = rsum + 0.3d0
        endif
        if (din(i3,j3,k) /= undef) then
          work(i,j,k) = work(i,j,k) + 0.3d0 * din(i3,j3,k)
          rsum = rsum + 0.3d0
        endif
        !c+++ average
        if (rsum > 1.d-10) then
          dio(i,j,k) = work(i,j,k) / rsum
        else
          dio(i,j,k) = undef
        endif
      enddo !! i
    enddo !! j
  enddo !! k
   
  return
end subroutine smth9

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine minmax
!c  eliminate extream values
!c  output: rmin <= din <= rmax
!c
!c=====
subroutine minmax(nx, ny, nz, undef, rmin, rmax, dio)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz    !! x-, y- z- axis sizes
  real(kind=r4b), intent(in)    :: undef         !! missing value
  real(kind=r4b), intent(in)    :: rmin, rmax    !! minimum and maximum values
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: dio(nx,ny,nz) !!
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k       !! loop variables

  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        if (dio(i,j,k) == undef) then
        else if (dio(i,j,k) < rmin) then
          !ccc dio(i,j,k) = rmin
          dio(i,j,k) = undef
        else if (dio(i,j,k) > rmax) then
          !ccc dio(i,j,k) = rmax
          dio(i,j,k) = undef
        else
        endif
      enddo !! k
    enddo !! j
  enddo !! i

  return
end subroutine minmax

!c----------------------------------------------------------------------c

!c======================================================================c

end module util_steflux
