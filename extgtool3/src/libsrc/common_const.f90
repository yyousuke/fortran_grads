!c
!c  module common_const
!c  [history]
!c  2017/05/01 Yamashita: first ver.
!c
!c======================================================================c
module common_const
  use common_typedef, only: i4b, r8b

  !c+++ [static parameters]
  !c+++ Definition of PI
  real(kind=r8b), public, parameter :: cnst_pi = 3.14159265358979323846d0

  !c+++ 1 [PVU] = 1e-6 [K m2/s/kg]
  real(kind=r8b), public, parameter :: cnst_PVU  = 1d6

  !c+++ Avogadro number (num/mol)
  real(kind=r8b), public, parameter :: cnst_NA = 6.0221415d23
  !c+++ molecular weight of air (kg/mol)
  real(kind=r8b), public, parameter :: cnst_mair = 2.8966d-2
  !c+++ definition of Dobson unit (num/m2)
  real(kind=r8b), public, parameter :: cnst_DU = 2.6868d20

  !c+++ Stefan-Boltzman constant
  real(kind=r8b), public, parameter :: cnst_stb   = 5.67d-8

  !c+++ Karman constant
  real(kind=r8b), public, parameter :: cnst_karman = 0.4d0

  !c+++ Plank constant
  real(kind=r8b), public, parameter :: cnst_plank = 6.6260755d-34

  !c+++ light speed [m/s]
  real(kind=r8b), public, parameter :: cnst_light = 2.99792458d+8

  !c+++ lapse rate (K/m)
  real(kind=r8b), public, parameter :: cnst_lapl = 6.5d-3

  !c+++ specific heat of water (J/K/kg)
  real(kind=r8b), public, parameter :: cnst_cl = 4218.0d0
  !c+++ specific heat of ice (J/K/kg)
  real(kind=r8b), public, parameter :: cnst_ci = 2106.0d0
  !ccc real(kind=r8b), public, parameter :: cnst_ci = 2006.0d0

  !c+++ Saturate pressure of water vapor at 0C (Pa)
  real(kind=r8b), public, parameter :: cnst_psat0 = 610.7d0
  !ccc real(kind=r8b), parameter :: es0 = 611.d0

  !c+++ Latent heat of melting (kg/m3)
  real(kind=r8b), public, parameter :: cnst_emelt = 3.40d5
  !ccc real(kind=r8b), parameter :: emelt = 3.40d5
  
  !c+++ Melting temperature of water (K)
  real(kind=r8b), public, parameter :: cnst_tmelt = 273.15d0
  !ccc real(kind=r8b), parameter :: Tmelt = 273.15d0

  !c+++ Melting temperature of water (K)
  real(kind=r8b), public, parameter :: cnst_tfrzs = 271.35d0

  !c+++ Wet-bulb temp. rain/snow (K)
  real(kind=r8b), public, parameter :: cnst_tqice = 273.15d0
  !ccc real(kind=r8b), parameter :: Tqice = 273.15d0

  !c+++ density of liquid water (kg/m3)
  real(kind=r8b), public, parameter :: cnst_dwatr = 1000.0D0
  !c+++ density of ice water (kg/m3)
  real(kind=r8b), public, parameter :: cnst_dice = 916.8d0

  !c+++ Surface pressure (Pa)
  real(kind=r8b), public, parameter :: cnst_pres0 = 101325.0d0
  !c+++ Surface temperature (K)
  real(kind=r8b), public, parameter :: cnst_tems0 = 300.0d0

  !c+++ Standard pressure (Pa)
  real(kind=r8b), public, parameter :: cnst_pre00 = 1.0d+5
  !c+++ Standard temperature (K)
  real(kind=r8b), public, parameter :: cnst_tem00 = 273.15d0


  !c+++ [save, enable modication]
  !c+++ scale height H (m)
  real(kind=r8b), public, save      :: cnst_h = 6950.d0 !

  !c+++ equatorial radius of the Earth (m)
  real(kind=r8b), public, save      :: cnst_eradius = 6371.22d3 !
  !ccc real(kind=r8b), public, parameter :: cnst_eradius = 6.370d6
  !ccc real(kind=r8b), parameter:: a = 6.370d6      !! equatorial radius (m)

  !c+++ Angular velocity of the Earth (1/s)
  real(kind=r8b), public, save      :: cnst_eohm = 7.2920d-5 !

  !c+++ gravitational acceleration of the Earth (m/s2)
  real(kind=r8b), public, save      :: cnst_egrav = 9.80616d0 !
  !ccc real(kind=r8b), parameter :: g = 9.81d0

  !c+++ gas constant of dry air (J/K/kg)
  real(kind=r8b), public, save      :: cnst_rair = 287.04d0 !
  !ccc real(kind=r8b), parameter :: rd = 287.04d0

  !c+++ gas constant of water vapor (J/K/kg)
  real(kind=r8b), public, save      :: cnst_rvap = 461.50d0 !
  !ccc real(kind=r8b), parameter :: Rv = 461.50d0

  !c+++ specific heat at constant pressure of air (J/K/kg)
  real(kind=r8b), public, save      :: cnst_cp = 1004.6d0 !
  !ccc real(kind=r8b), public, parameter :: cnst_cp = 1004.d0
  !ccc real(kind=r8b), parameter :: cp = 1004.d0     !! specific heat at constant pressure of air (J/K/kg)

  !c+++ specific heat at constant pressure of vapor (J/K/kg)
  real(kind=r8b), public, save      :: cnst_cpv = 1850.0d0 !

  !c+++ latent heat for evap. at 0C (kg/m3)
  real(kind=r8b), public, save      :: cnst_lh0   = 2.5008d+6 !
  !ccc real(kind=r8b), parameter :: Lq = 2.501d6

  !c+++ Latent heat of sublimation at 0C (kg/m3)
  real(kind=r8b), public, save      :: cnst_lhs0 = 2.8342d+6 !


  !c+++ [save, variables]
  !c+++ Specific heat at consant volume of air (J/K/kg)
  real(kind=r8b), public, save      :: cnst_cv
  !c+++ Specific heat at consant volume of vapor (J/K/kg)
  real(kind=r8b), public, save      :: cnst_cvv

  !c+++ Cp/Cv
  real(kind=r8b), public, save      :: cnst_gamma
  !c+++ R/Cp
  real(kind=r8b), public, save      :: cnst_kappa
  !ccc real(kind=r8b), parameter:: rkappa = rd / cp

  !c+++ molecular weight ( water/air )
  real(kind=r8b), public, save      :: cnst_epsv
  !c+++ 1/epsv-1
  real(kind=r8b), public, save      :: cnst_epsvt

  !c+++ Standard density (kg/m3)
  real(kind=r8b), public, save      :: cnst_rho00

  !c+++ Latent heat of vaporizaion at 0K (kg/m3)
  real(kind=r8b), public, save      :: cnst_lh00
  !c+++ Latent heat of sublimation at 0K (kg/m3)
  real(kind=r8b), public, save      :: cnst_lhs00
  !c+++ Latent heat of fusion at 0K (kg/m3)
  real(kind=r8b), public, save      :: cnst_lhf0
  !c+++ Latent heat of fusion at 0K (kg/m3)
  real(kind=r8b), public, save      :: cnst_lhf00

  public const_setup

contains

!c======================================================================c

!c---------------------------------------------------------------------c
!c subroutine const_setup
!c=====
subroutine const_setup(scale_height, earth_radius, earth_angvel, earth_gravity, gas_cnst, &
&  gas_cnst_vap, specific_heat_pre, specific_heat_pre_vap, latent_heat_vap, latent_heat_sub)
  implicit none
  real(kind=r8b), optional :: scale_height          !! Scale height
  real(kind=r8b), optional :: earth_radius          !! Earth radius
  real(kind=r8b), optional :: earth_angvel          !! Anguler velocity of the earth
  real(kind=r8b), optional :: earth_gravity         !! Gravitational accelaration
  real(kind=r8b), optional :: gas_cnst              !! Gas constant of dry air
  real(kind=r8b), optional :: gas_cnst_vap          !! Gas constant of water vapour
  real(kind=r8b), optional :: specific_heat_pre     !! Specific heat of air (const pre)
  real(kind=r8b), optional :: specific_heat_pre_vap !! Specific heat of water vapour (const pre)
  real(kind=r8b), optional :: latent_heat_vap       !! latent heat of vaporization LH0 (0 deg)
  real(kind=r8b), optional :: latent_heat_sub       !! latent heat of sublimation LHS0 (0 deg)

  if (present(scale_height)) cnst_h = scale_height
  if (present(earth_radius)) cnst_eradius = earth_radius
  if (present(earth_angvel)) cnst_eohm = earth_angvel
  if (present(earth_gravity)) cnst_egrav = earth_gravity 
  if (present(gas_cnst)) cnst_rair = gas_cnst 
  if (present(gas_cnst_vap)) cnst_rvap = gas_cnst_vap
  if (present(specific_heat_pre)) cnst_cp = specific_heat_pre
  if (present(specific_heat_pre_vap)) cnst_cpv = specific_heat_pre_vap
  if (present(latent_heat_vap)) cnst_lh0 = latent_heat_vap
  if (present(latent_heat_sub)) cnst_lhs0 = latent_heat_sub

  !c+++ calculate parameters
  cnst_cv    = cnst_cp - cnst_rair
  cnst_gamma = cnst_cp / cnst_cv
  cnst_kappa = cnst_rair / cnst_cp
  cnst_rho00 = cnst_pre00 / cnst_rair / cnst_tem00
  !c+++
  cnst_cvv   = cnst_cpv - cnst_rvap
  cnst_epsv  = cnst_rair / cnst_rvap
  cnst_epsvt = 1.0d0 / cnst_epsv - 1.0d0
  !c+++
  cnst_lh00  = cnst_lh0 - ( cnst_cpv - cnst_cl ) * cnst_tem00
  cnst_lhs00 = cnst_lhs0 - ( cnst_cpv - cnst_ci ) * cnst_tem00
  cnst_lhf0  = cnst_lhs0 - cnst_lh0
  !c+++
  cnst_lhf00 = cnst_lhf0 - ( cnst_cl - cnst_ci ) * cnst_tem00

  return
end subroutine const_setup

!c---------------------------------------------------------------------c

!c======================================================================c

end module common_const

