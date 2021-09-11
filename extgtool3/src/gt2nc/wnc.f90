!c
!c  module wnc
!c  [history]
!c  2003/08/23 Yamashita: first ver.
!c  2008/05/30 Yamashita: f77 ==> f90
!c  2013/04/06 Yamashita: for other data
!c  2013/11/25 Yamashita: add writenc_eta
!c  2013/11/26 Yamashita: add writenc_sig
!c  2014/11/26 Yamashita: add writenc_sig
!c  2014/02/18 Yamashita: 64-bit offset netCDF file
!c  2014/08/26 Yamashita: add time1 output
!c  2014/08/29 Yamashita: add CMOR cell_measures & cell_method
!c  2014/09/02 Yamashita: add time_bnds & ccmi_date outputs
!c  2015/01/13 Yamashita: option to add time_bnds
!c  2015/05/26 Yamashita: option to remove standard_name (-s none)
!c  2015/05/26 Yamashita: add writenc_z
!c  2021/08/28 Yamashita: add data type of GATTR
!c======================================================================c
module wnc
  use common_typedef, only: i2b, i4b, r4b, r8b
  implicit none
  private
  public :: writenc, writenc_z, writenc_sig, writenc_eta

  !c+++ [internal common]
  !c+++ NETCDF
  include 'netcdf.inc' 
  !c+++ axis CMOR
  integer(kind=i4b), parameter :: ncmor = 2
  character(len=*), parameter  :: CMOR(ncmor) = &
&   (/ 'must_have_bounds', 'bounds          ' /)
  character(len=*), parameter  :: LVL_CMOR(ncmor) = &
&   (/ 'no ',  'N/A'/)
  character(len=*), parameter  :: LAT_CMOR(ncmor) = &
&   (/ 'no ',  'N/A'/)
  character(len=*), parameter  :: LON_CMOR(ncmor) = &
&   (/ 'no ',  'N/A'/)
  character(len=*), parameter  :: REC_CMOR(ncmor) = &
&   (/ 'yes      ',  'time_bnds'/)
  character(len=*), parameter  :: REC1_CMOR(ncmor) = &
&   (/ 'no ',  'N/A'/)
  character(len=*), parameter  :: RECB_CMOR(ncmor) = &
&   (/ 'no ',  'N/A'/)
  character(len=*), parameter  :: ETA_CMOR(ncmor) = &
&   (/ 'no ',  'N/A'/)
  character(len=*), parameter  :: ETB_CMOR(ncmor) = &
&   (/ 'no ',  'N/A'/)
  !c+++ data CMOR
  character(len=*), parameter  :: DCMOR(ncmor) = &
&   (/ 'cell_measures', 'cell_methods ' /)
  character(len=*), parameter  :: PS_MESR_i = 'area: areacella'
  character(len=200)           :: PS_CMOR(ncmor), DATA_CMOR(ncmor)
  character(len=30)            :: hcmor

  !c+++ data nbnds
  integer(kind=i4b), parameter :: nb = 2
  real(kind=r8b), parameter    :: bnds(2) = (/0.d0, 1.d0/)

  !c+++ axis defs.
  character(len=*), parameter  :: AXIS = 'axis'
  character(len=*), parameter  :: LVL_AXIS = 'Z'
  character(len=*), parameter  :: LAT_AXIS = 'Y'
  character(len=*), parameter  :: LON_AXIS = 'X'
  character(len=*), parameter  :: REC_AXIS = 'T'
  !c+++ axis names
  character(len=*), parameter  :: LAT_NAME = 'lat'
  character(len=*), parameter  :: LON_NAME = 'lon'
  character(len=*), parameter  :: REC_NAME = 'time'
  character(len=*), parameter  :: BND_NAME = 'bnds'
  character(len=*), parameter  :: REC1_NAME = 'time1'
  character(len=*), parameter  :: RECB_NAME = 'time_bnds'
  !c+++ axis standard_name
  character(len=*), parameter  :: SNAME = 'standard_name'
  character(len=*), parameter  :: LAT_SNAME = 'latitude'
  character(len=*), parameter  :: LON_SNAME = 'longitude'
  character(len=*), parameter  :: REC_SNAME = 'time'
  character(len=*), parameter  :: BND_SNAME = 'bnds'
  character(len=*), parameter  :: REC1_SNAME = 'time1'
  character(len=*), parameter  :: RECB_SNAME = 'time_bnds'
  !c+++ axis long_name
  character(len=*), parameter  :: LNAME = 'long_name'
  character(len=*), parameter  :: LAT_LNAME = 'Latitude'
  character(len=*), parameter  :: LON_LNAME = 'Longitude'
  character(len=*), parameter  :: REC_LNAME = 'Time'
  character(len=*), parameter  :: BND_LNAME = 'Boundaries'
  character(len=*), parameter  :: REC1_LNAME = 'Time1'
  character(len=*), parameter  :: RECB_LNAME = 'Time Boundaries'
  !c+++ axis units
  character(len=*), parameter  :: UNITS = 'units'
  character(len=*), parameter  :: LAT_UNITS = 'degrees_north'
  character(len=*), parameter  :: LON_UNITS = 'degrees_east'
  character(len=*), parameter  :: BND_UNITS = '1'

  !c+++ for CCMI timestamp
  integer(kind=i4b), parameter :: ni = 5
  character(len=*), parameter  :: CCMI_UNITS(ni) = &
&   (/ 'year  ', 'month ', 'day   ', 'hour  ', 'minute' /)
  character(len=*), parameter  :: CCMI_NAME(ni) = &
&   (/ 'ccmi_year  ', 'ccmi_month ', 'ccmi_day   ', 'ccmi_hour  ', 'ccmi_minute' /)
  character(len=*), parameter  :: CCMI_LNAME(ni) = &
&   (/ 'CCMI Year  ', 'CCMI Month ', 'CCMI Day   ', 'CCMI Hour  ', 'CCMI Minute' /)
  character(len=*), parameter  :: CCMI_SNAME(ni) = &
&   (/ 'actual_year  ', 'actual_month ', 'actual_day   ', 'actual_hour  ', 'actual_minute' /)

  !c+++ for ps data
  character(len=*), parameter  :: PS_NAME  = 'ps' !! variable name of ps
  character(len=*), parameter  :: PS_LNAME = 'Surface Pressure' !! long_name of ps
  character(len=*), parameter  :: PS_SNAME = 'surface_air_pressure' !! standard_name of ps
  integer(kind=i2b), parameter :: PS_GRIB = 1

  !c+++ for levels
  character(len=*), parameter  :: GRIB = 'GRIB_id'
  integer(kind=i2b), parameter :: LVL_GRIB = 100
  character(len=*), parameter  :: GRNM = 'GRIB_name'
  !c+++ for time
  character(len=*), parameter  :: CALN = 'calendar'
  character(len=*), parameter  :: REC_CALN = 'standard'
  !c+++ for data
  integer(kind=i2b), parameter :: DATA_GRIB = 33
  !c+++ common
  character(len=*), parameter  :: RANG = 'actual_range'
  real(kind=r4b)               :: COM_RANG(2)
  real(kind=r8b)               :: COM_DRANG(2)


  !c+++ [internal work]
  integer(kind=i4b)            :: i
  integer(kind=i4b)            :: sta !! return code
  !c+++ dimid
  integer(kind=i4b)            :: dimid_lev, dimid_lat, dimid_lon
  integer(kind=i4b)            :: dimid_rec, dimid_bnd, dimid_rec1
  integer(kind=i4b)            :: dimid_eta, dimid_etb !! for eta-levels
  !c+++ field for time_bnds write
  integer(kind=i4b), parameter :: ndims_recb = 2
  integer(kind=i4b)            :: start_recb(ndims_recb), range_recb(ndims_recb)
  integer(kind=i4b)            :: dimids_recb(ndims_recb)
  !c+++ field for data write
  integer(kind=i4b), parameter :: ndims = 4
  integer(kind=i4b)            :: start(ndims), range(ndims) 
  integer(kind=i4b)            :: dimids(ndims)
  !c+++ field for ps write
  integer(kind=i4b), parameter :: ndims_ps = 3
  integer(kind=i4b)            :: start_ps(ndims_ps), range_ps(ndims_ps)
  integer(kind=i4b)            :: dimids_ps(ndims_ps)
  !c+++ work for GATTR
  integer(kind=i2b)            :: wgi2
  integer(kind=i4b)            :: wgi4
  real(kind=r4b)               :: wgr4
  real(kind=r8b)               :: wgr8

  !c+++ [internal save]
  !c+++ file id
  integer(kind=i4b), save      :: fileid
  !c+++ variable id
  integer(kind=i4b), save      :: varid_lev, varid_lon, varid_lat
  integer(kind=i4b), save      :: varid_rec, varid_bnd, varid_rec1
  integer(kind=i4b), save      :: varid_recb, varid_data
  integer(kind=i4b), save      :: varid_ps             !! for ps
  integer(kind=i4b), save      :: varid_eta, varid_etb !! for eta-levels
  integer(kind=i4b)            :: varid_ccm(ni)        !! for CCMI timestamp

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c subroutine writenc
!c itype = 0: real(4), itype = 1: integer(2)
!c====
subroutine writenc(nx, ny, np, nt, itype, it, lon, lat, lev, &
& time, time1, timeb, jtime, dout, &
& afile, scale, offset, rmiss, rmin, rmax, LVL_NAME, REC_UNITS, &
& DATA_UNITS, DATA_NAME, DATA_LNAME, DATA_SNAME, DATA_MESR, DATA_METD, &
& otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt    !! x-, y-, z-, t- axis sizes
  integer(kind=i4b), intent(in) :: itype             !! output data format
  integer(kind=i4b), intent(in) :: it                !! record number
  real(kind=r8b), intent(in)    :: lon(nx)           !! lon(deg)
  real(kind=r8b), intent(in)    :: lat(ny)           !! lat(deg)
  real(kind=r8b), intent(in)    :: lev(np)           !! levels(hPa)
  real(kind=r8b), intent(in)    :: time(nt)          !! time(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: time1(nt)         !! time1(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: timeb(nb,nt)      !! time boundaries
  integer(kind=i4b), intent(in) :: jtime(ni,nt)      !! ccmi timestamp
  real(kind=r4b), intent(in)    :: dout(nx,ny,np)    !! output data
  character(len=*), intent(in)  :: afile             !! output file name
  !c+++ for output data
  real(kind=r4b), intent(in)    :: scale, offset     !! output: dout/scale - offset
  real(kind=r4b), intent(in)    :: rmiss             !! missing values
  real(kind=r4b), intent(in)    :: rmin              !! min. values
  real(kind=r4b), intent(in)    :: rmax              !! max. values
  !c+++ axis name
  character(len=*), intent(in)  :: LVL_NAME          !! z-axis name
  !c+++ unit of output time (e.g. hours since 1860-1-1 00:00:0.0)
  character(len=*), intent(in)  :: REC_UNITS         !! time units
  !c+++ descriptions of output data
  character(len=*), intent(in)  :: DATA_UNITS        !! units of data
  character(len=*), intent(in)  :: DATA_NAME         !! variable name of data
  character(len=*), intent(in)  :: DATA_LNAME        !! long_name of data
  character(len=*), intent(in)  :: DATA_SNAME        !! standard_name of data
  character(len=*), intent(in)  :: DATA_MESR         !! output data cell_measures
  character(len=*), intent(in)  :: DATA_METD         !! output data cell_method
  !c+++ switch
  logical, intent(in)           :: otbnd             !! t: write time_bnd, f: do nothing
  !c+++ global attributes
  integer(kind=i4b), intent(in) :: nattr             !! num. of attributes
  character(len=*), intent(in)  :: GATTR(nattr)      !! fields
  character(len=*), intent(in)  :: DATA_GATTR(nattr) !! values
  character(len=*), intent(in)  :: TYPE_GATTR(nattr) !! data type

  !c+++ [internal]
  !c+++ data for write 
  integer(kind=i2b)             :: sdata(nx,ny,np,1) 
  real(kind=r4b)                :: rdata(nx,ny,np,1) 
  !c+++ axis name
  !ccc character(len=*), parameter  :: LVL_NAME = 'lev' !! 2015/07/17
  !c+++ axis unit
  character(len=*), parameter   :: LVL_UNITS = 'hPa'
  !ccc character(len=*), parameter :: REC_UNITS = 'hours since 1-1-1 00:00:0.0'
  !c+++ axis long_name
  character(len=*), parameter   :: LVL_LNAME = 'Pressure'
  !c+++ axis standard_name
  character(len=*), parameter   :: LVL_SNAME = 'air_pressure'
  !c+++ for levels
  character(len=*), parameter   :: POSI = 'positive'
  character(len=*), parameter   :: LVL_POSI = 'down'
  character(len=*), parameter   :: LVL_GRNM = 'hPa'
  !c+++ internal save
  logical, save                 :: ofirst = .true.   !!
  logical, save                 :: owtim1 = .true.   !! t: write time1
  logical, save                 :: owtccm = .true.   !! t: write ccmi_time

  start(1:4) = (/1,1,1,1/)
  range(1:4) = (/nx,ny,np,1/)
  start_recb(1:2) = (/1,1/)
  range_recb(1:2) = (/nb,nt/)

  if (ofirst) then
    ofirst = .false.
    !c+++ time1
    if (time(1) == time1(1)) owtim1 = .false.
    !c+++ ccmi time
    if (jtime(1,1) < 0) owtccm = .false.

    !c+++ CMOR
    PS_CMOR(1) = PS_MESR_i
    PS_CMOR(2) = DATA_METD
    DATA_CMOR(1) = DATA_MESR
    DATA_CMOR(2) = DATA_METD

    !c+++ open
    sta = nf_create(afile, ior(nf_clobber, nf_64bit_offset), fileid) 
    if (sta /= NF_NOERR) call err_handler(1, 'error creating file ')

    !c+++ set dimensions
    sta = nf_def_dim(fileid, LVL_NAME, np, dimid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error lev')
    sta = nf_def_dim(fileid, LAT_NAME, ny, dimid_lat) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lat')
    sta = nf_def_dim(fileid, LON_NAME, nx, dimid_lon) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lon')
    !ccc sta = nf_def_dim(fileid, REC_NAME, NF_UNLIMITED, dimid_rec) 
    sta = nf_def_dim(fileid, REC_NAME, nt, dimid_rec) 
    if (sta /= NF_NOERR) call err_handler(1, 'error rec')
    if (otbnd) then
      sta = nf_def_dim(fileid, BND_NAME, nb, dimid_bnd) 
      if (sta /= NF_NOERR) call err_handler(1, 'error bnds')
    endif
    if (owtim1) then
      sta = nf_def_dim(fileid, REC1_NAME, nt, dimid_rec1) 
      if (sta /= NF_NOERR) call err_handler(1, 'error rec1')
    endif

    !c+++ define the coordinate dimensions
    sta = nf_def_var(fileid, LVL_NAME, NF_DOUBLE, 1, dimid_lev, varid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev coordinate')
    sta = nf_def_var(fileid, LAT_NAME, NF_DOUBLE, 1, dimid_lat, varid_lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat coordinate')
    sta = nf_def_var(fileid, LON_NAME, NF_DOUBLE, 1, dimid_lon, varid_lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon coordinate')
    sta = nf_def_var(fileid, REC_NAME, NF_DOUBLE, 1, dimid_rec, varid_rec)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time coordinate')
    if (otbnd) then
      sta = nf_def_var(fileid, BND_NAME, NF_DOUBLE, 1, dimid_bnd, varid_bnd)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds coordinate')
    endif
    if (owtim1) then
      sta = nf_def_var(fileid, REC1_NAME, NF_DOUBLE, 1, dimid_rec1, varid_rec1)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 coordinate')
    endif

    !c+++ definition of axis
    sta = nf_put_att_text(fileid, dimid_lev, AXIS, len(LVL_AXIS), LVL_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev axis')
    sta = nf_put_att_text(fileid, dimid_lat, AXIS, len(LAT_AXIS), LAT_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat axis')
    sta = nf_put_att_text(fileid, dimid_lon, AXIS, len(LON_AXIS), LON_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon axis')
    sta = nf_put_att_text(fileid, dimid_rec, AXIS, len(REC_AXIS), REC_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time axis')

    !c+++ units for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, UNITS, len(LVL_UNITS), LVL_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev units')
    sta = nf_put_att_text(fileid, dimid_lat, UNITS, len(LAT_UNITS), LAT_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat units')
    sta = nf_put_att_text(fileid, dimid_lon, UNITS, len(LON_UNITS), LON_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon units')
    sta = nf_put_att_text(fileid, dimid_rec, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define time units')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, UNITS, len(trim(BND_UNITS)), trim(BND_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds units')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 units')
    endif

    !c+++ long names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, LNAME, len(LVL_LNAME), LVL_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev long name')
    sta = nf_put_att_text(fileid, dimid_lat, LNAME, len(LAT_LNAME), LAT_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat long name')
    sta = nf_put_att_text(fileid, dimid_lon, LNAME, len(LON_LNAME), LON_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon long name')
    sta = nf_put_att_text(fileid, dimid_rec, LNAME, len(REC_LNAME), REC_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time long name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, LNAME, len(BND_LNAME), BND_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds long name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, LNAME, len(REC1_LNAME), REC1_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 long name')
    endif

    !c+++ standard names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, SNAME, len(LVL_SNAME), LVL_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev standard name')
    sta = nf_put_att_text(fileid, dimid_lat, SNAME, len(LAT_SNAME), LAT_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat standard name')
    sta = nf_put_att_text(fileid, dimid_lon, SNAME, len(LON_SNAME), LON_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon standard name')
    sta = nf_put_att_text(fileid, dimid_rec, SNAME, len(REC_SNAME), REC_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time standard name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, SNAME, len(BND_SNAME), BND_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds standard name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, SNAME, len(REC1_SNAME), REC1_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 standard name')
    endif

    !c+++ CMOR for the coordinate variables
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, dimid_lev, CMOR(i), len(trim(LVL_CMOR(i))), trim(LVL_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lev '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lat, CMOR(i), len(trim(LAT_CMOR(i))), trim(LAT_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lat '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lon, CMOR(i), len(trim(LON_CMOR(i))), trim(LON_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lon '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_rec, CMOR(i), len(trim(REC_CMOR(i))), trim(REC_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time '//trim(hcmor))
      if (owtim1) then
        sta = nf_put_att_text(fileid, dimid_rec1, CMOR(i), len(trim(REC1_CMOR(i))), trim(REC1_CMOR(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define time1 '//trim(hcmor))
      endif
    enddo !! i

    !c+++ for levels
    sta = nf_put_att_text(fileid, dimid_lev, POSI, len(LVL_POSI), LVL_POSI)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev positive')
    sta = nf_put_att_int2(fileid, dimid_lev, GRIB, NF_INT2, 1, LVL_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_id')
    sta = nf_put_att_text(fileid, dimid_lev, GRNM, len(LVL_GRNM), LVL_GRNM)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_name')

    !c+++ for time
    sta = nf_put_att_text(fileid, dimid_rec, CALN, len(REC_CALN), REC_CALN)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time calendar')
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 calendar')
    endif

    !c+++ axis ranges
    COM_DRANG(1) = minval(lev)
    COM_DRANG(2) = maxval(lev)
    sta = nf_put_att_double(fileid, dimid_lev, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev range')
    COM_DRANG(1) = minval(lat)
    COM_DRANG(2) = maxval(lat)
    sta = nf_put_att_double(fileid, dimid_lat, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat range')
    COM_DRANG(1) = minval(lon)
    COM_DRANG(2) = maxval(lon)
    sta = nf_put_att_double(fileid, dimid_lon, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon range')
    !ccc COM_RANG(1) = lon(1)
    !ccc COM_RANG(2) = lon(nx)
    !ccc sta = nf_put_att_real(fileid, dimid_lon, RANG, NF_REAL, 2, COM_RANG)
    COM_DRANG(1) = time(1)
    COM_DRANG(2) = time(nt)
    sta = nf_put_att_double(fileid, dimid_rec, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time range')
    if (otbnd) then
      COM_DRANG(1) = bnds(1)
      COM_DRANG(2) = bnds(nb)
      sta = nf_put_att_double(fileid, dimid_bnd, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds range')
    endif
    if (owtim1) then
      COM_DRANG(1) = time1(1)
      COM_DRANG(2) = time1(nt)
      sta = nf_put_att_double(fileid, dimid_rec1, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 range')
    endif

    !c+++ CCMI time data
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        !c+++ define the netCDF variables for the CCMI data
        sta = nf_def_var(fileid, CCMI_NAME(i), NF_INT, 1, dimid_rec, varid_ccm(i))
        if (sta /= NF_NOERR) call err_handler(1, 'error define coordinate '//trim(hcmor))
        !c+++ write units attributes for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), UNITS, len_trim(CCMI_UNITS(i)), trim(CCMI_UNITS(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define units '//trim(hcmor))
        !c+++ write long name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), LNAME, len_trim(CCMI_LNAME(i)), trim(CCMI_LNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define long name '//trim(hcmor))
        !c+++ write standard name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), SNAME, len_trim(CCMI_SNAME(i)), trim(CCMI_SNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define standard name '//trim(hcmor))
        !c+++ write scale factor for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), 'scale_factor', NF_INT, 1, 1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write scale factor '//trim(hcmor))
        sta = nf_put_att_int(fileid, varid_ccm(i), 'add_offset', NF_INT, 1, 0)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write offset '//trim(hcmor))
        !c+++ write missing value for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), '_FillValue', NF_INT, 1, -1)
        sta = nf_put_att_int(fileid, varid_ccm(i), 'missing_value', NF_INT, 1, -1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write missing_value '//trim(hcmor))
      enddo
    endif
 
    !c+++ time_bnds data
    if (otbnd) then
      !c+++ set dim. id for the time_bnds data
      dimids_recb(1) = dimid_bnd
      dimids_recb(2) = dimid_rec
      !c+++ define the netCDF variables for the time_bnds data
      sta = nf_def_var(fileid, RECB_NAME, NF_DOUBLE, ndims_recb, dimids_recb, varid_recb)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds coordinate')
      !c+++ write units attributes for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds units')
      !c+++ write long name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, LNAME, len(trim(RECB_LNAME)), trim(RECB_LNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds long name') 
      !c+++ write standard name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, SNAME, len(trim(RECB_SNAME)), trim(RECB_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds standard name')
      !c+++ write calendar for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds calendar')
      !c+++ write min., and max. values of time_bnds data
      COM_DRANG(1) = minval(timeb)
      COM_DRANG(2) = maxval(timeb)
      sta = nf_put_att_double(fileid, varid_recb, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(sta, 'error write time_bnds actual_range')
    endif

    !c+++ set dim. id for the output data
    dimids(1) = dimid_lon
    dimids(2) = dimid_lat
    dimids(3) = dimid_lev
    dimids(4) = dimid_rec
    !c+++ define the netCDF variables for the output data
    if (itype == 0) then
      sta = nf_def_var(fileid, DATA_NAME, NF_REAL, ndims, dimids, varid_data)
    else
      sta = nf_def_var(fileid, DATA_NAME, NF_INT2, ndims, dimids, varid_data)
    endif
    if (sta /= NF_NOERR) call err_handler(1, 'error define data coordinate')

    !c+++ assign units attributes to the netCDF variable
    sta = nf_put_att_text(fileid, varid_data, UNITS, len(trim(DATA_UNITS)), trim(DATA_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data units')
    !c+++ write long name
    sta = nf_put_att_text(fileid, varid_data, LNAME, len(trim(DATA_LNAME)), trim(DATA_LNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data long name')

    !c+++ write standard name
    if (trim(DATA_SNAME) /= 'none') then
      sta = nf_put_att_text(fileid, varid_data, SNAME, len(trim(DATA_SNAME)), trim(DATA_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data standard name')
    endif

    !c+++ write CMOR
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, varid_data, DCMOR(i), len(trim(DATA_CMOR(i))), trim(DATA_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data '//trim(hcmor))
    enddo !! i

    !c+++ write grib id
    sta = nf_put_att_int2(fileid, varid_data, GRIB, NF_INT2, 1, DATA_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define data GRIB_id')

    !c+++ write scale factor and offset
    sta = nf_put_att_real(fileid, varid_data, 'scale_factor', NF_REAL, 1, scale)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data scale factor')
    sta = nf_put_att_real(fileid, varid_data, 'add_offset', NF_REAL, 1, offset)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data offset')
    !c+++ write missing values
    if (itype == 0) then
      rdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_real(fileid, varid_data, '_FillValue', NF_REAL, 1, rdata(1,1,1,1))
      sta = nf_put_att_real(fileid, varid_data, 'missing_value', NF_REAL, 1, rdata(1,1,1,1))
    else
      sdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_int2(fileid, varid_data, '_FillValue', NF_INT2, 1, sdata(1,1,1,1))
      sta = nf_put_att_int2(fileid, varid_data, 'missing_value', NF_INT2, 1, sdata(1,1,1,1))
    endif
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data missing_value')

    !c+++ write min., and max. values
    COM_RANG(1) = minval(dout)
    COM_RANG(2) = maxval(dout)
    sta = nf_put_att_real(fileid, varid_data, RANG, NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data actual_range')
    COM_RANG(1) = rmin
    COM_RANG(2) = rmax
    sta = nf_put_att_real(fileid, varid_data, 'valid_range', NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data valid_range')

    !c+++ write global attributes
    do i = 1, nattr
      if (trim(TYPE_GATTR(i)) == 'TEXT') then !! character
        sta = nf_put_att_text(fileid, NF_GLOBAL, trim(GATTR(i)), len(trim(DATA_GATTR(i))), trim(DATA_GATTR(i)))
      else if (trim(TYPE_GATTR(i)) == 'INT') then !! integer(4)
        read(DATA_GATTR(i), *) wgi4
        sta = nf_put_att_int(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT, 1, wgi4)
      else if (trim(TYPE_GATTR(i)) == 'INT2') then !! integer(2)
        read(DATA_GATTR(i), *) wgi2
        sta = nf_put_att_int2(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT2, 1, wgi2)
      else if (trim(TYPE_GATTR(i)) == 'REAL') then !! real(4)
        read(DATA_GATTR(i), *) wgr4
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), 1, wgr4)
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), NF_REAL, 1, wgr4)
      else if (trim(TYPE_GATTR(i)) == 'DOUBLE') then !! real(8)
        read(DATA_GATTR(i), *) wgr8
        sta = nf_put_att_double(fileid, NF_GLOBAL, trim(GATTR(i)), NF_DOUBLE, 1, wgr8)
      else !! error
        sta = NF_NOERR + 1
      endif
      if (sta /= NF_NOERR) call err_handler(i, 'error define global attributes')
    enddo !! i

    !c+++ end define mode
    sta = nf_enddef(fileid)
    if (sta /= NF_NOERR) call err_handler(1, 'error enddef')

    !c+++ write the coordinate variable data
    sta = nf_put_var_double(fileid, varid_lev, lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error write level data')
    sta = nf_put_var_double(fileid, varid_lat, lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error write latitude data')
    sta = nf_put_var_double(fileid, varid_lon, lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error write longitude data')
    !ccc sta = nf_put_var_real(fileid, varid_lon, lon)
    sta = nf_put_var_double(fileid, varid_rec, time)
    if (sta /= NF_NOERR) call err_handler(1, 'error write time data')
    if (otbnd) then
      sta = nf_put_var_double(fileid, varid_bnd, bnds)
      if (sta /= NF_NOERR) call err_handler(1, 'error write bnds data')
    endif
    if (owtim1) then
      sta = nf_put_var_double(fileid, varid_rec1, time1)
      if (sta /= NF_NOERR) call err_handler(1, 'error write time1 data')
    endif
    !c+++ write CCMI time
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        sta = nf_put_var_int(fileid, varid_ccm(i), jtime(i,1:nt))
        if (sta /= NF_NOERR) call err_handler(1, 'error write data '//trim(hcmor))
      enddo
    endif
    !c+++ write time_bnds
    if (otbnd) then
      sta = nf_put_vara_double(fileid, varid_recb, start_recb, range_recb, timeb) 
      if (sta /= NF_NOERR) call err_handler(1, 'error write output time_bnds double')
    endif
  endif

  !c+++ write the output data
  start(4) = it
  if (itype == 0) then
    rdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_real(fileid, varid_data, start, range, rdata)
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data real')
  else
    sdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_int2(fileid, varid_data, start, range, sdata) 
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data short')
  endif

  !c+++ close
  if (it == nt) sta = nf_close(fileid) 

  return 
end subroutine writenc

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c subroutine writenc_z
!c itype = 0: real(4), itype = 1: integer(2)
!c====
subroutine writenc_z(nx, ny, np, nt, itype, it, lon, lat, lev, &
& time, time1, timeb, jtime, dout, &
& afile, scale, offset, rmiss, rmin, rmax, REC_UNITS, &
& DATA_UNITS, DATA_NAME, DATA_LNAME, DATA_SNAME, DATA_MESR, DATA_METD, &
& otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt    !! x-, y-, z-, t- axis sizes
  integer(kind=i4b), intent(in) :: itype             !! output data format
  integer(kind=i4b), intent(in) :: it                !! record number
  real(kind=r8b), intent(in)    :: lon(nx)           !! lon(deg)
  real(kind=r8b), intent(in)    :: lat(ny)           !! lat(deg)
  real(kind=r8b), intent(in)    :: lev(np)           !! levels(m)
  real(kind=r8b), intent(in)    :: time(nt)          !! time(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: time1(nt)         !! time1(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: timeb(nb,nt)      !! time boundaries
  integer(kind=i4b), intent(in) :: jtime(ni,nt)      !! ccmi timestamp
  real(kind=r4b), intent(in)    :: dout(nx,ny,np)    !! output data
  character(len=*), intent(in)  :: afile             !! output file name
  !c+++ for output data
  real(kind=r4b), intent(in)    :: scale, offset     !! output: dout/scale - offset
  real(kind=r4b), intent(in)    :: rmiss             !! missing values
  real(kind=r4b), intent(in)    :: rmin              !! min. values
  real(kind=r4b), intent(in)    :: rmax              !! max. values
  !c+++ unit of output time (e.g. hours since 1860-1-1 00:00:0.0)
  character(len=*), intent(in)  :: REC_UNITS         !! time units
  !c+++ descriptions of output data
  character(len=*), intent(in)  :: DATA_UNITS        !! units of data
  character(len=*), intent(in)  :: DATA_NAME         !! variable name of data
  character(len=*), intent(in)  :: DATA_LNAME        !! long_name of data
  character(len=*), intent(in)  :: DATA_SNAME        !! standard_name of data
  character(len=*), intent(in)  :: DATA_MESR         !! output data cell_measures
  character(len=*), intent(in)  :: DATA_METD         !! output data cell_method
  !c+++ switch
  logical, intent(in)           :: otbnd             !! t: write time_bnd, f: do nothing
  !c+++ global attributes
  integer(kind=i4b), intent(in) :: nattr             !! num. of attributes
  character(len=*), intent(in)  :: GATTR(nattr)      !! fields
  character(len=*), intent(in)  :: DATA_GATTR(nattr) !! values
  character(len=*), intent(in)  :: TYPE_GATTR(nattr) !! data type

  !c+++ [internal]
  !c+++ data for write 
  integer(kind=i2b)             :: sdata(nx,ny,np,1) 
  real(kind=r4b)                :: rdata(nx,ny,np,1) 
  !c+++ axis name
  character(len=*), parameter   :: LVL_NAME = 'height'
  !c+++ axis unit
  character(len=*), parameter   :: LVL_UNITS = 'm'
  !ccc character(len=*), parameter :: REC_UNITS = 'hours since 1-1-1 00:00:0.0'
  !c+++ axis long_name
  character(len=*), parameter   :: LVL_LNAME = 'height'
  !c+++ axis standard_name
  character(len=*), parameter   :: LVL_SNAME = 'height'
  !c+++ for levels
  character(len=*), parameter   :: POSI = 'positive'
  character(len=*), parameter   :: LVL_POSI = 'down'
  character(len=*), parameter   :: LVL_GRNM = 'm'
  !c+++ internal save
  logical, save                 :: ofirst = .true.   !!
  logical, save                 :: owtim1 = .true.   !! t: write time1
  logical, save                 :: owtccm = .true.   !! t: write ccmi_time

  start(1:4) = (/1,1,1,1/)
  range(1:4) = (/nx,ny,np,1/)
  start_recb(1:2) = (/1,1/)
  range_recb(1:2) = (/nb,nt/)

  if (ofirst) then
    ofirst = .false.
    !c+++ time1
    if (time(1) == time1(1)) owtim1 = .false.
    !c+++ ccmi time
    if (jtime(1,1) < 0) owtccm = .false.

    !c+++ CMOR
    PS_CMOR(1) = PS_MESR_i
    PS_CMOR(2) = DATA_METD
    DATA_CMOR(1) = DATA_MESR
    DATA_CMOR(2) = DATA_METD

    !c+++ open
    sta = nf_create(afile, ior(nf_clobber, nf_64bit_offset), fileid) 
    if (sta /= NF_NOERR) call err_handler(1, 'error creating file ')

    !c+++ set dimensions
    sta = nf_def_dim(fileid, LVL_NAME, np, dimid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error lev')
    sta = nf_def_dim(fileid, LAT_NAME, ny, dimid_lat) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lat')
    sta = nf_def_dim(fileid, LON_NAME, nx, dimid_lon) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lon')
    !ccc sta = nf_def_dim(fileid, REC_NAME, NF_UNLIMITED, dimid_rec) 
    sta = nf_def_dim(fileid, REC_NAME, nt, dimid_rec) 
    if (sta /= NF_NOERR) call err_handler(1, 'error rec')
    if (otbnd) then
      sta = nf_def_dim(fileid, BND_NAME, nb, dimid_bnd) 
      if (sta /= NF_NOERR) call err_handler(1, 'error bnds')
    endif
    if (owtim1) then
      sta = nf_def_dim(fileid, REC1_NAME, nt, dimid_rec1) 
      if (sta /= NF_NOERR) call err_handler(1, 'error rec1')
    endif

    !c+++ define the coordinate dimensions
    sta = nf_def_var(fileid, LVL_NAME, NF_DOUBLE, 1, dimid_lev, varid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev coordinate')
    sta = nf_def_var(fileid, LAT_NAME, NF_DOUBLE, 1, dimid_lat, varid_lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat coordinate')
    sta = nf_def_var(fileid, LON_NAME, NF_DOUBLE, 1, dimid_lon, varid_lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon coordinate')
    sta = nf_def_var(fileid, REC_NAME, NF_DOUBLE, 1, dimid_rec, varid_rec)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time coordinate')
    if (otbnd) then
      sta = nf_def_var(fileid, BND_NAME, NF_DOUBLE, 1, dimid_bnd, varid_bnd)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds coordinate')
    endif
    if (owtim1) then
      sta = nf_def_var(fileid, REC1_NAME, NF_DOUBLE, 1, dimid_rec1, varid_rec1)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 coordinate')
    endif

    !c+++ definition of axis
    sta = nf_put_att_text(fileid, dimid_lev, AXIS, len(LVL_AXIS), LVL_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev axis')
    sta = nf_put_att_text(fileid, dimid_lat, AXIS, len(LAT_AXIS), LAT_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat axis')
    sta = nf_put_att_text(fileid, dimid_lon, AXIS, len(LON_AXIS), LON_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon axis')
    sta = nf_put_att_text(fileid, dimid_rec, AXIS, len(REC_AXIS), REC_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time axis')

    !c+++ units for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, UNITS, len(LVL_UNITS), LVL_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev units')
    sta = nf_put_att_text(fileid, dimid_lat, UNITS, len(LAT_UNITS), LAT_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat units')
    sta = nf_put_att_text(fileid, dimid_lon, UNITS, len(LON_UNITS), LON_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon units')
    sta = nf_put_att_text(fileid, dimid_rec, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define time units')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, UNITS, len(trim(BND_UNITS)), trim(BND_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds units')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 units')
    endif

    !c+++ long names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, LNAME, len(LVL_LNAME), LVL_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev long name')
    sta = nf_put_att_text(fileid, dimid_lat, LNAME, len(LAT_LNAME), LAT_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat long name')
    sta = nf_put_att_text(fileid, dimid_lon, LNAME, len(LON_LNAME), LON_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon long name')
    sta = nf_put_att_text(fileid, dimid_rec, LNAME, len(REC_LNAME), REC_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time long name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, LNAME, len(BND_LNAME), BND_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds long name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, LNAME, len(REC1_LNAME), REC1_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 long name')
    endif

    !c+++ standard names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, SNAME, len(LVL_SNAME), LVL_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev standard name')
    sta = nf_put_att_text(fileid, dimid_lat, SNAME, len(LAT_SNAME), LAT_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat standard name')
    sta = nf_put_att_text(fileid, dimid_lon, SNAME, len(LON_SNAME), LON_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon standard name')
    sta = nf_put_att_text(fileid, dimid_rec, SNAME, len(REC_SNAME), REC_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time standard name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, SNAME, len(BND_SNAME), BND_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds standard name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, SNAME, len(REC1_SNAME), REC1_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 standard name')
    endif

    !c+++ CMOR for the coordinate variables
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, dimid_lev, CMOR(i), len(trim(LVL_CMOR(i))), trim(LVL_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lev '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lat, CMOR(i), len(trim(LAT_CMOR(i))), trim(LAT_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lat '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lon, CMOR(i), len(trim(LON_CMOR(i))), trim(LON_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lon '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_rec, CMOR(i), len(trim(REC_CMOR(i))), trim(REC_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time '//trim(hcmor))
      if (owtim1) then
        sta = nf_put_att_text(fileid, dimid_rec1, CMOR(i), len(trim(REC1_CMOR(i))), trim(REC1_CMOR(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define time1 '//trim(hcmor))
      endif
    enddo !! i

    !c+++ for levels
    sta = nf_put_att_text(fileid, dimid_lev, POSI, len(LVL_POSI), LVL_POSI)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev positive')
    sta = nf_put_att_int2(fileid, dimid_lev, GRIB, NF_INT2, 1, LVL_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_id')
    sta = nf_put_att_text(fileid, dimid_lev, GRNM, len(LVL_GRNM), LVL_GRNM)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_name')

    !c+++ for time
    sta = nf_put_att_text(fileid, dimid_rec, CALN, len(REC_CALN), REC_CALN)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time calendar')
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 calendar')
    endif

    !c+++ axis ranges
    COM_DRANG(1) = minval(lev)
    COM_DRANG(2) = maxval(lev)
    sta = nf_put_att_double(fileid, dimid_lev, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev range')
    COM_DRANG(1) = minval(lat)
    COM_DRANG(2) = maxval(lat)
    sta = nf_put_att_double(fileid, dimid_lat, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat range')
    COM_DRANG(1) = minval(lon)
    COM_DRANG(2) = maxval(lon)
    sta = nf_put_att_double(fileid, dimid_lon, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon range')
    !ccc COM_RANG(1) = lon(1)
    !ccc COM_RANG(2) = lon(nx)
    !ccc sta = nf_put_att_real(fileid, dimid_lon, RANG, NF_REAL, 2, COM_RANG)
    COM_DRANG(1) = time(1)
    COM_DRANG(2) = time(nt)
    sta = nf_put_att_double(fileid, dimid_rec, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time range')
    if (otbnd) then
      COM_DRANG(1) = bnds(1)
      COM_DRANG(2) = bnds(nb)
      sta = nf_put_att_double(fileid, dimid_bnd, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds range')
    endif
    if (owtim1) then
      COM_DRANG(1) = time1(1)
      COM_DRANG(2) = time1(nt)
      sta = nf_put_att_double(fileid, dimid_rec1, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 range')
    endif

    !c+++ CCMI time data
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        !c+++ define the netCDF variables for the CCMI data
        sta = nf_def_var(fileid, CCMI_NAME(i), NF_INT, 1, dimid_rec, varid_ccm(i))
        if (sta /= NF_NOERR) call err_handler(1, 'error define coordinate '//trim(hcmor))
        !c+++ write units attributes for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), UNITS, len_trim(CCMI_UNITS(i)), trim(CCMI_UNITS(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define units '//trim(hcmor))
        !c+++ write long name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), LNAME, len_trim(CCMI_LNAME(i)), trim(CCMI_LNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define long name '//trim(hcmor))
        !c+++ write standard name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), SNAME, len_trim(CCMI_SNAME(i)), trim(CCMI_SNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define standard name '//trim(hcmor))
        !c+++ write scale factor for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), 'scale_factor', NF_INT, 1, 1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write scale factor '//trim(hcmor))
        sta = nf_put_att_int(fileid, varid_ccm(i), 'add_offset', NF_INT, 1, 0)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write offset '//trim(hcmor))
        !c+++ write missing value for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), '_FillValue', NF_INT, 1, -1)
        sta = nf_put_att_int(fileid, varid_ccm(i), 'missing_value', NF_INT, 1, -1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write missing_value '//trim(hcmor))
      enddo
    endif
 
    !c+++ time_bnds data
    if (otbnd) then
      !c+++ set dim. id for the time_bnds data
      dimids_recb(1) = dimid_bnd
      dimids_recb(2) = dimid_rec
      !c+++ define the netCDF variables for the time_bnds data
      sta = nf_def_var(fileid, RECB_NAME, NF_DOUBLE, ndims_recb, dimids_recb, varid_recb)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds coordinate')
      !c+++ write units attributes for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds units')
      !c+++ write long name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, LNAME, len(trim(RECB_LNAME)), trim(RECB_LNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds long name') 
      !c+++ write standard name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, SNAME, len(trim(RECB_SNAME)), trim(RECB_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds standard name')
      !c+++ write calendar for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds calendar')
      !c+++ write min., and max. values of time_bnds data
      COM_DRANG(1) = minval(timeb)
      COM_DRANG(2) = maxval(timeb)
      sta = nf_put_att_double(fileid, varid_recb, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(sta, 'error write time_bnds actual_range')
    endif

    !c+++ set dim. id for the output data
    dimids(1) = dimid_lon
    dimids(2) = dimid_lat
    dimids(3) = dimid_lev
    dimids(4) = dimid_rec
    !c+++ define the netCDF variables for the output data
    if (itype == 0) then
      sta = nf_def_var(fileid, DATA_NAME, NF_REAL, ndims, dimids, varid_data)
    else
      sta = nf_def_var(fileid, DATA_NAME, NF_INT2, ndims, dimids, varid_data)
    endif
    if (sta /= NF_NOERR) call err_handler(1, 'error define data coordinate')

    !c+++ assign units attributes to the netCDF variable
    sta = nf_put_att_text(fileid, varid_data, UNITS, len(trim(DATA_UNITS)), trim(DATA_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data units')
    !c+++ write long name
    sta = nf_put_att_text(fileid, varid_data, LNAME, len(trim(DATA_LNAME)), trim(DATA_LNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data long name')

    !c+++ write standard name
    if (trim(DATA_SNAME) /= 'none') then
      sta = nf_put_att_text(fileid, varid_data, SNAME, len(trim(DATA_SNAME)), trim(DATA_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data standard name')
    endif

    !c+++ write CMOR
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, varid_data, DCMOR(i), len(trim(DATA_CMOR(i))), trim(DATA_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data '//trim(hcmor))
    enddo !! i

    !c+++ write grib id
    sta = nf_put_att_int2(fileid, varid_data, GRIB, NF_INT2, 1, DATA_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define data GRIB_id')

    !c+++ write scale factor and offset
    sta = nf_put_att_real(fileid, varid_data, 'scale_factor', NF_REAL, 1, scale)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data scale factor')
    sta = nf_put_att_real(fileid, varid_data, 'add_offset', NF_REAL, 1, offset)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data offset')
    !c+++ write missing values
    if (itype == 0) then
      rdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_real(fileid, varid_data, '_FillValue', NF_REAL, 1, rdata(1,1,1,1))
      sta = nf_put_att_real(fileid, varid_data, 'missing_value', NF_REAL, 1, rdata(1,1,1,1))
    else
      sdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_int2(fileid, varid_data, '_FillValue', NF_INT2, 1, sdata(1,1,1,1))
      sta = nf_put_att_int2(fileid, varid_data, 'missing_value', NF_INT2, 1, sdata(1,1,1,1))
    endif
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data missing_value')

    !c+++ write min., and max. values
    COM_RANG(1) = minval(dout)
    COM_RANG(2) = maxval(dout)
    sta = nf_put_att_real(fileid, varid_data, RANG, NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data actual_range')
    COM_RANG(1) = rmin
    COM_RANG(2) = rmax
    sta = nf_put_att_real(fileid, varid_data, 'valid_range', NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data valid_range')

    !c+++ write global attributes
    do i = 1, nattr
      if (trim(TYPE_GATTR(i)) == 'TEXT') then !! character
        sta = nf_put_att_text(fileid, NF_GLOBAL, trim(GATTR(i)), len(trim(DATA_GATTR(i))), trim(DATA_GATTR(i)))
      else if (trim(TYPE_GATTR(i)) == 'INT') then !! integer(4)
        read(DATA_GATTR(i), *) wgi4
        sta = nf_put_att_int(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT, 1, wgi4)
      else if (trim(TYPE_GATTR(i)) == 'INT2') then !! integer(2)
        read(DATA_GATTR(i), *) wgi2
        sta = nf_put_att_int2(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT2, 1, wgi2)
      else if (trim(TYPE_GATTR(i)) == 'REAL') then !! real(4)
        read(DATA_GATTR(i), *) wgr4
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), 1, wgr4)
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), NF_REAL, 1, wgr4)
      else if (trim(TYPE_GATTR(i)) == 'DOUBLE') then !! real(8)
        read(DATA_GATTR(i), *) wgr8
        sta = nf_put_att_double(fileid, NF_GLOBAL, trim(GATTR(i)), NF_DOUBLE, 1, wgr8)
      else !! error
        sta = NF_NOERR + 1
      endif
      if (sta /= NF_NOERR) call err_handler(i, 'error define global attributes')
    enddo !! i

    !c+++ end define mode
    sta = nf_enddef(fileid)
    if (sta /= NF_NOERR) call err_handler(1, 'error enddef')

    !c+++ write the coordinate variable data
    sta = nf_put_var_double(fileid, varid_lev, lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error write level data')
    sta = nf_put_var_double(fileid, varid_lat, lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error write latitude data')
    sta = nf_put_var_double(fileid, varid_lon, lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error write longitude data')
    !ccc sta = nf_put_var_real(fileid, varid_lon, lon)
    sta = nf_put_var_double(fileid, varid_rec, time)
    if (sta /= NF_NOERR) call err_handler(1, 'error write time data')
    if (otbnd) then
      sta = nf_put_var_double(fileid, varid_bnd, bnds)
      if (sta /= NF_NOERR) call err_handler(1, 'error write bnds data')
    endif
    if (owtim1) then
      sta = nf_put_var_double(fileid, varid_rec1, time1)
      if (sta /= NF_NOERR) call err_handler(1, 'error write time1 data')
    endif
    !c+++ write CCMI time
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        sta = nf_put_var_int(fileid, varid_ccm(i), jtime(i,1:nt))
        if (sta /= NF_NOERR) call err_handler(1, 'error write data '//trim(hcmor))
      enddo
    endif
    !c+++ write time_bnds
    if (otbnd) then
      sta = nf_put_vara_double(fileid, varid_recb, start_recb, range_recb, timeb) 
      if (sta /= NF_NOERR) call err_handler(1, 'error write output time_bnds double')
    endif
  endif

  !c+++ write the output data
  start(4) = it
  if (itype == 0) then
    rdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_real(fileid, varid_data, start, range, rdata)
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data real')
  else
    sdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_int2(fileid, varid_data, start, range, sdata) 
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data short')
  endif

  !c+++ close
  if (it == nt) sta = nf_close(fileid) 

  return 
end subroutine writenc_z

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c subroutine writenc_sig
!c itype = 0: real(4), itype = 1: integer(2)
!c====
subroutine writenc_sig(nx, ny, np, nt, itype, it, lon, lat, lev, &
& time, time1, timeb, jtime, dout, &
& ps, PS_UNITS, rmin_ps, rmax_ps, &
& afile, scale, offset, rmiss, rmin, rmax, REC_UNITS, &
& DATA_UNITS, DATA_NAME, DATA_LNAME, DATA_SNAME, DATA_MESR, DATA_METD, &
& otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt    !! x-, y-, z-, t- axis sizes
  integer(kind=i4b), intent(in) :: itype             !! output data format
  integer(kind=i4b), intent(in) :: it                !! record number
  real(kind=r8b), intent(in)    :: lon(nx)           !! lon(deg)
  real(kind=r8b), intent(in)    :: lat(ny)           !! lat(deg)
  real(kind=r8b), intent(in)    :: lev(np)           !! sigma-levels()
  real(kind=r8b), intent(in)    :: time(nt)          !! time(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: time1(nt)         !! time1(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: timeb(nb,nt)      !! time boundaries
  integer(kind=i4b), intent(in) :: jtime(ni,nt)      !! ccmi timestamp
  !c+++ for output data
  real(kind=r4b), intent(in)    :: dout(nx,ny,np)    !! output data
  real(kind=r4b), intent(in)    :: ps(nx,ny)         !! ps data
  character(len=*), intent(in)  :: PS_UNITS          !! units of Ps data
  real(kind=r4b), intent(in)    :: rmin_ps           !! min. values
  real(kind=r4b), intent(in)    :: rmax_ps           !! max. values
  character(len=*), intent(in)  :: afile             !! output file name
  real(kind=r4b), intent(in)    :: scale, offset     !! output: dout/scale - offset
  real(kind=r4b), intent(in)    :: rmiss             !! missing values
  real(kind=r4b), intent(in)    :: rmin              !! min. values
  real(kind=r4b), intent(in)    :: rmax              !! max. values
  !c+++ unit of output time (e.g. hours since 1860-1-1 00:00:0.0)
  character(len=*), intent(in)  :: REC_UNITS         !! time units
  !c+++ descriptions of output data
  character(len=*), intent(in)  :: DATA_UNITS        !! units of data
  character(len=*), intent(in)  :: DATA_NAME         !! variable name of data
  character(len=*), intent(in)  :: DATA_LNAME        !! long_name of data
  character(len=*), intent(in)  :: DATA_SNAME        !! standard_name of data
  character(len=*), intent(in)  :: DATA_MESR         !! output data cell_measures
  character(len=*), intent(in)  :: DATA_METD         !! output data cell_method
  !c+++ switch
  logical, intent(in)           :: otbnd             !! t: write time_bnd, f: do nothing
  !c+++ global attributes
  integer(kind=i4b), intent(in) :: nattr             !! num. of attributes
  character(len=*), intent(in)  :: GATTR(nattr)      !! fields
  character(len=*), intent(in)  :: DATA_GATTR(nattr) !! values
  character(len=*), intent(in)  :: TYPE_GATTR(nattr) !! data type

  !c+++ [internal]
  !c+++ data for write
  integer(kind=i2b)             :: sdata(nx,ny,np,1) !! data
  integer(kind=i2b)             :: sdata_ps(nx,ny,1) !! ps-data
  real(kind=r4b)                :: rdata(nx,ny,np,1) !! data
  real(kind=r4b)                :: rdata_ps(nx,ny,1) !! ps-data
  !c+++ axis name
  character(len=*), parameter   :: LVL_NAME = 'sigma'
  !c+++ axis unit
  character(len=*), parameter   :: LVL_UNITS = '1'
  !ccc character(len=*), parameter :: REC_UNITS = 'hours since 1-1-1 00:00:0.0'
  !c+++ axis long_name
  !ccc character(len=*), parameter :: LVL_LNAME = 'Pressure'
  character(len=*), parameter   :: LVL_LNAME = 'Model Level'
  !c+++ axis standard_name
  !ccc character(len=*), parameter :: LVL_SNAME = 'air_pressure'
  character(len=*), parameter   :: LVL_SNAME = 'atmosphere_sigma_coordinate'
  !c+++ for levels
  character(len=*), parameter   :: POSI = 'positive'
  character(len=*), parameter   :: LVL_POSI = 'down'
  character(len=*), parameter   :: LVL_GRNM = '1'
  character(len=*), parameter   :: FMLA = 'formula'
  character(len=*), parameter   :: LVL_FMLA = 'p(n,k,j,i) = sigma(k)*ps(n,j,i)'
  !c+++ internal save
  logical, save                 :: ofirst = .true.   !!
  logical, save                 :: owtim1 = .true.   !! t: write time1
  logical, save                 :: owtccm = .true.   !! t: write ccmi_time

  start(1:4) = (/1,1,1,1/)
  range(1:4) = (/nx,ny,np,1/)
  start_ps(1:3) = (/1,1,1/)
  range_ps(1:3) = (/nx,ny,1/)
  start_recb(1:2) = (/1,1/)
  range_recb(1:2) = (/nb,nt/)

  if (ofirst) then
    ofirst = .false.
    !c+++ time1
    if (time(1) == time1(1)) owtim1 = .false.
    !c+++ ccmi time
    if (jtime(1,1) < 0) owtccm = .false.

    !c+++ CMOR
    PS_CMOR(1) = PS_MESR_i
    PS_CMOR(2) = DATA_METD
    DATA_CMOR(1) = DATA_MESR
    DATA_CMOR(2) = DATA_METD

    !c+++ open
    sta = nf_create(afile, ior(nf_clobber, nf_64bit_offset), fileid) 
    if (sta /= NF_NOERR) call err_handler(1, 'error creating file ')

    !c+++ set dimensions
    sta = nf_def_dim(fileid, LVL_NAME, np, dimid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error lev')
    sta = nf_def_dim(fileid, LAT_NAME, ny, dimid_lat) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lat')
    sta = nf_def_dim(fileid, LON_NAME, nx, dimid_lon) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lon')
    !ccc sta = nf_def_dim(fileid, REC_NAME, NF_UNLIMITED, dimid_rec) 
    sta = nf_def_dim(fileid, REC_NAME, nt, dimid_rec) 
    if (sta /= NF_NOERR) call err_handler(1, 'error rec')
    if (otbnd) then
      sta = nf_def_dim(fileid, BND_NAME, nb, dimid_bnd)
      if (sta /= NF_NOERR) call err_handler(1, 'error bnds')
    endif
    if (owtim1) then
      sta = nf_def_dim(fileid, REC1_NAME, nt, dimid_rec1) 
      if (sta /= NF_NOERR) call err_handler(1, 'error rec1')
    endif

    !c+++ define the coordinate dimensions
    sta = nf_def_var(fileid, LVL_NAME, NF_DOUBLE, 1, dimid_lev, varid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev coordinate')
    sta = nf_def_var(fileid, LAT_NAME, NF_DOUBLE, 1, dimid_lat, varid_lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat coordinate')
    sta = nf_def_var(fileid, LON_NAME, NF_DOUBLE, 1, dimid_lon, varid_lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon coordinate')
    sta = nf_def_var(fileid, REC_NAME, NF_DOUBLE, 1, dimid_rec, varid_rec)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time coordinate')
    if (otbnd) then
      sta = nf_def_var(fileid, BND_NAME, NF_DOUBLE, 1, dimid_bnd, varid_bnd)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds coordinate')
    endif
    if (owtim1) then
      sta = nf_def_var(fileid, REC1_NAME, NF_DOUBLE, 1, dimid_rec1, varid_rec1)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 coordinate')
    endif

    !c+++ definition of axis
    sta = nf_put_att_text(fileid, dimid_lev, AXIS, len(LVL_AXIS), LVL_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev axis')
    sta = nf_put_att_text(fileid, dimid_lat, AXIS, len(LAT_AXIS), LAT_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat axis')
    sta = nf_put_att_text(fileid, dimid_lon, AXIS, len(LON_AXIS), LON_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon axis')
    sta = nf_put_att_text(fileid, dimid_rec, AXIS, len(REC_AXIS), REC_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time axis')

    !c+++ units for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, UNITS, len(LVL_UNITS), LVL_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev units')
    sta = nf_put_att_text(fileid, dimid_lat, UNITS, len(LAT_UNITS), LAT_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat units')
    sta = nf_put_att_text(fileid, dimid_lon, UNITS, len(LON_UNITS), LON_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon units')
    sta = nf_put_att_text(fileid, dimid_rec, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define time units')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, UNITS, len(trim(BND_UNITS)), trim(BND_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds units')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 units')
    endif

    !c+++ long names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, LNAME, len(LVL_LNAME), LVL_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev long name')
    sta = nf_put_att_text(fileid, dimid_lat, LNAME, len(LAT_LNAME), LAT_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat long name')
    sta = nf_put_att_text(fileid, dimid_lon, LNAME, len(LON_LNAME), LON_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon long name')
    sta = nf_put_att_text(fileid, dimid_rec, LNAME, len(REC_LNAME), REC_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time long name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, LNAME, len(BND_LNAME), BND_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds long name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, LNAME, len(REC1_LNAME), REC1_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 long name')
    endif

    !c+++ standard names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, SNAME, len(LVL_SNAME), LVL_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev standard name')
    sta = nf_put_att_text(fileid, dimid_lat, SNAME, len(LAT_SNAME), LAT_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat standard name')
    sta = nf_put_att_text(fileid, dimid_lon, SNAME, len(LON_SNAME), LON_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon standard name')
    sta = nf_put_att_text(fileid, dimid_rec, SNAME, len(REC_SNAME), REC_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time standard name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, SNAME, len(BND_SNAME), BND_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds standard name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, SNAME, len(REC1_SNAME), REC1_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 standard name')
    endif

    !c+++ CMOR for the coordinate variables
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, dimid_lev, CMOR(i), len(trim(LVL_CMOR(i))), trim(LVL_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lev '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lat, CMOR(i), len(trim(LAT_CMOR(i))), trim(LAT_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lat '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lon, CMOR(i), len(trim(LON_CMOR(i))), trim(LON_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lon '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_rec, CMOR(i), len(trim(REC_CMOR(i))), trim(REC_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time '//trim(hcmor))
      if (owtim1) then
        sta = nf_put_att_text(fileid, dimid_rec1, CMOR(i), len(trim(REC1_CMOR(i))), trim(REC1_CMOR(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define time1 '//trim(hcmor))
      endif
    enddo !! i

    !c+++ for levels
    sta = nf_put_att_text(fileid, dimid_lev, POSI, len(LVL_POSI), LVL_POSI)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev positive')
    sta = nf_put_att_int2(fileid, dimid_lev, GRIB, NF_INT2, 1, LVL_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_id')
    sta = nf_put_att_text(fileid, dimid_lev, GRNM, len(LVL_GRNM), LVL_GRNM)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_name')
    sta = nf_put_att_text(fileid, dimid_lev, FMLA, len(LVL_FMLA), LVL_FMLA)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev formula')

    !c+++ for time
    sta = nf_put_att_text(fileid, dimid_rec, CALN, len(REC_CALN), REC_CALN)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time calendar')
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 calendar')
    endif

    !c+++ axis ranges
    COM_DRANG(1) = minval(lev)
    COM_DRANG(2) = maxval(lev)
    sta = nf_put_att_double(fileid, dimid_lev, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev range')
    COM_DRANG(1) = minval(lat)
    COM_DRANG(2) = maxval(lat)
    sta = nf_put_att_double(fileid, dimid_lat, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat range')
    COM_DRANG(1) = minval(lon)
    COM_DRANG(2) = maxval(lon)
    sta = nf_put_att_double(fileid, dimid_lon, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon range')
    COM_DRANG(1) = time(1)
    COM_DRANG(2) = time(nt)
    sta = nf_put_att_double(fileid, dimid_rec, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time range')
    if (otbnd) then
      COM_DRANG(1) = bnds(1)
      COM_DRANG(2) = bnds(nb)
      sta = nf_put_att_double(fileid, dimid_bnd, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds range')
    endif
    if (owtim1) then
      COM_DRANG(1) = time1(1)
      COM_DRANG(2) = time1(nt)
      sta = nf_put_att_double(fileid, dimid_rec1, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 range')
    endif

    !c+++ CCMI time data
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        !c+++ define the netCDF variables for the CCMI data
        sta = nf_def_var(fileid, CCMI_NAME(i), NF_INT, 1, dimid_rec, varid_ccm(i))
        if (sta /= NF_NOERR) call err_handler(1, 'error define coordinate '//trim(hcmor))
        !c+++ write units attributes for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), UNITS, len_trim(CCMI_UNITS(i)), trim(CCMI_UNITS(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define units '//trim(hcmor))
        !c+++ write long name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), LNAME, len_trim(CCMI_LNAME(i)), trim(CCMI_LNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define long name '//trim(hcmor))
        !c+++ write standard name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), SNAME, len_trim(CCMI_SNAME(i)), trim(CCMI_SNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define standard name '//trim(hcmor))
        !c+++ write scale factor for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), 'scale_factor', NF_INT, 1, 1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write scale factor '//trim(hcmor))
        sta = nf_put_att_int(fileid, varid_ccm(i), 'add_offset', NF_INT, 1, 0)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write offset '//trim(hcmor))
        !c+++ write missing value for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), '_FillValue', NF_INT, 1, -1)
        sta = nf_put_att_int(fileid, varid_ccm(i), 'missing_value', NF_INT, 1, -1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write missing_value '//trim(hcmor))
      enddo
    endif

    !c+++ time_bnds data
    if (otbnd) then
      !c+++ set dim. id for the time_bnds data
      dimids_recb(1) = dimid_bnd
      dimids_recb(2) = dimid_rec
      !c+++ define the netCDF variables for the time_bnds data
      sta = nf_def_var(fileid, RECB_NAME, NF_DOUBLE, ndims_recb, dimids_recb, varid_recb)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds coordinate')
      !c+++ write units attributes for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds units')
      !c+++ write long name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, LNAME, len(trim(RECB_LNAME)), trim(RECB_LNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds long name')
      !c+++ write standard name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, SNAME, len(trim(RECB_SNAME)), trim(RECB_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds standard name')
      !c+++ write calendar for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds calendar')
      !c+++ write min., and max. values of time_bnds data
      COM_DRANG(1) = minval(timeb)
      COM_DRANG(2) = maxval(timeb)
      sta = nf_put_att_double(fileid, varid_recb, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(sta, 'error write time_bnds actual_range')
    endif

    !c+++ set dim. id for the output data
    dimids(1) = dimid_lon
    dimids(2) = dimid_lat
    dimids(3) = dimid_lev
    dimids(4) = dimid_rec
    !c+++ define the netCDF variables for the output data
    if (itype == 0) then
      sta = nf_def_var(fileid, DATA_NAME, NF_REAL, ndims, dimids, varid_data)
    else
      sta = nf_def_var(fileid, DATA_NAME, NF_INT2, ndims, dimids, varid_data)
    endif
    if (sta /= NF_NOERR) call err_handler(1, 'error define data coordinate')

    !c+++ set dim. id for the ps data
    dimids_ps(1) = dimid_lon
    dimids_ps(2) = dimid_lat
    dimids_ps(3) = dimid_rec
    !c+++ define the netCDF variables for the ps data
    if (itype == 0) then
      sta = nf_def_var(fileid, PS_NAME, NF_REAL, ndims_ps, dimids_ps, varid_ps)
    else
      sta = nf_def_var(fileid, PS_NAME, NF_INT2, ndims_ps, dimids_ps, varid_ps)
    endif
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps coordinate')

    !c+++ assign units attributes to the netCDF variable
    sta = nf_put_att_text(fileid, varid_data, UNITS, len(trim(DATA_UNITS)), trim(DATA_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data units')
    sta = nf_put_att_text(fileid, varid_ps, UNITS, len(trim(PS_UNITS)), trim(PS_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps units')

    !c+++ write long name
    sta = nf_put_att_text(fileid, varid_data, LNAME, len(trim(DATA_LNAME)), trim(DATA_LNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data long name')
    sta = nf_put_att_text(fileid, varid_ps, LNAME, len(trim(PS_LNAME)), trim(PS_LNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps long name')

    !c+++ write standard name
    if (trim(DATA_SNAME) /= 'none') then
      sta = nf_put_att_text(fileid, varid_data, SNAME, len(trim(DATA_SNAME)), trim(DATA_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data standard name')
    endif
    sta = nf_put_att_text(fileid, varid_ps, SNAME, len(trim(PS_SNAME)), trim(PS_SNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps standard name')

    !c+++ write CMOR
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, varid_data, DCMOR(i), len(trim(DATA_CMOR(i))), trim(DATA_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data '//trim(hcmor))
      sta = nf_put_att_text(fileid, varid_ps, DCMOR(i), len(trim(PS_CMOR(i))), trim(PS_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define ps '//trim(hcmor))
    enddo !! i

    !c+++ write grib id
    sta = nf_put_att_int2(fileid, varid_data, GRIB, NF_INT2, 1, DATA_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define data GRIB_id')
    sta = nf_put_att_int2(fileid, varid_ps, GRIB, NF_INT2, 1, PS_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps GRIB_id')

    !c+++ write scale factor and offset of data
    sta = nf_put_att_real(fileid, varid_data, 'scale_factor', NF_REAL, 1, scale)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data scale factor')
    sta = nf_put_att_real(fileid, varid_data, 'add_offset', NF_REAL, 1, offset)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data offset')

    !c+++ write scale factor and offset of ps data
    sta = nf_put_att_real(fileid, varid_ps, 'scale_factor', NF_REAL, 1, scale)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps scale factor')
    sta = nf_put_att_real(fileid, varid_ps, 'add_offset', NF_REAL, 1, offset)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps offset')

    !c+++ write missing values of data
    if (itype == 0) then
      rdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_real(fileid, varid_data, '_FillValue', NF_REAL, 1, rdata(1,1,1,1))
      sta = nf_put_att_real(fileid, varid_data, 'missing_value', NF_REAL, 1, rdata(1,1,1,1))
    else
      sdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_int2(fileid, varid_data, '_FillValue', NF_INT2, 1, sdata(1,1,1,1))
      sta = nf_put_att_int2(fileid, varid_data, 'missing_value', NF_INT2, 1, sdata(1,1,1,1))
    endif
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data missing_value')

    !c+++ write missing values of ps data
    if (itype == 0) then
      rdata_ps(1,1,1) = rmiss / scale - offset
      sta = nf_put_att_real(fileid, varid_ps, '_FillValue', NF_REAL, 1, rdata_ps(1,1,1))
      sta = nf_put_att_real(fileid, varid_ps, 'missing_value', NF_REAL, 1, rdata_ps(1,1,1))
    else
      sdata_ps(1,1,1) = rmiss / scale - offset
      sta = nf_put_att_int2(fileid, varid_ps, '_FillValue', NF_INT2, 1, sdata_ps(1,1,1))
      sta = nf_put_att_int2(fileid, varid_ps, 'missing_value', NF_INT2, 1, sdata_ps(1,1,1))
    endif
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps missing_value')

    !c+++ write min., and max. values of data
    COM_RANG(1) = minval(dout)
    COM_RANG(2) = maxval(dout)
    sta = nf_put_att_real(fileid, varid_data, RANG, NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data actual_range')
    COM_RANG(1) = rmin
    COM_RANG(2) = rmax
    sta = nf_put_att_real(fileid, varid_data, 'valid_range', NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data valid_range')

    !c+++ write min., and max. values of ps data
    COM_RANG(1) = minval(ps)
    COM_RANG(2) = maxval(ps)
    sta = nf_put_att_real(fileid, varid_ps, RANG, NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps actual_range')
    COM_RANG(1) = rmin_ps
    COM_RANG(2) = rmax_ps
    sta = nf_put_att_real(fileid, varid_ps, 'valid_range', NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps valid_range')

    !c+++ write global attributes
    do i = 1, nattr
      if (trim(TYPE_GATTR(i)) == 'TEXT') then !! character
        sta = nf_put_att_text(fileid, NF_GLOBAL, trim(GATTR(i)), len(trim(DATA_GATTR(i))), trim(DATA_GATTR(i)))
      else if (trim(TYPE_GATTR(i)) == 'INT') then !! integer(4)
        read(DATA_GATTR(i), *) wgi4
        sta = nf_put_att_int(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT, 1, wgi4)
      else if (trim(TYPE_GATTR(i)) == 'INT2') then !! integer(2)
        read(DATA_GATTR(i), *) wgi2
        sta = nf_put_att_int2(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT2, 1, wgi2)
      else if (trim(TYPE_GATTR(i)) == 'REAL') then !! real(4)
        read(DATA_GATTR(i), *) wgr4
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), 1, wgr4)
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), NF_REAL, 1, wgr4)
      else if (trim(TYPE_GATTR(i)) == 'DOUBLE') then !! real(8)
        read(DATA_GATTR(i), *) wgr8
        sta = nf_put_att_double(fileid, NF_GLOBAL, trim(GATTR(i)), NF_DOUBLE, 1, wgr8)
      else !! error
        sta = NF_NOERR + 1
      endif
      if (sta /= NF_NOERR) call err_handler(i, 'error define global attributes')
    enddo !! i

    !c+++ end define mode
    sta = nf_enddef(fileid)
    if (sta /= NF_NOERR) call err_handler(1, 'error enddef')

    !c+++ write the coordinate variable data
    sta = nf_put_var_double(fileid, varid_lev, lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error write level data')
    sta = nf_put_var_double(fileid, varid_lat, lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error write latitude data')
    sta = nf_put_var_double(fileid, varid_lon, lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error write longitude data')
    sta = nf_put_var_double(fileid, varid_rec, time)
    if (sta /= NF_NOERR) call err_handler(1, 'error write time data')
    if (otbnd) then
      sta = nf_put_var_double(fileid, varid_bnd, bnds)
      if (sta /= NF_NOERR) call err_handler(1, 'error write bnds data')
    endif
    if (owtim1) then
      sta = nf_put_var_double(fileid, varid_rec1, time1)
      if (sta /= NF_NOERR) call err_handler(1, 'error write time1 data')
    endif
    !c+++ write CCMI time
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        sta = nf_put_var_int(fileid, varid_ccm(i), jtime(i,1:nt))
        if (sta /= NF_NOERR) call err_handler(1, 'error write data '//trim(hcmor))
      enddo
    endif
    !c+++ write time_bnds
    if (otbnd) then
      sta = nf_put_vara_double(fileid, varid_recb, start_recb, range_recb, timeb) 
      if (sta /= NF_NOERR) call err_handler(1, 'error write output time_bnds double')
    endif
  endif

  !c+++ write output data
  start(4) = it
  if (itype == 0) then
    rdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_real(fileid, varid_data, start, range, rdata)
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data real')
  else
    sdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_int2(fileid, varid_data, start, range, sdata) 
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data short')
  endif

  !c+++ write output ps data
  start_ps(3) = it
  if (itype == 0) then
    rdata_ps(1:nx,1:ny,1) = ps(1:nx,1:ny) / scale - offset
    sta = nf_put_vara_real(fileid, varid_ps, start_ps, range_ps, rdata_ps)
    if (sta /= NF_NOERR) call err_handler(1, 'error write output ps real')
  else
    sdata_ps(1:nx,1:ny,1) = ps(1:nx,1:ny) / scale - offset
    sta = nf_put_vara_int2(fileid, varid_ps, start_ps, range_ps, sdata_ps) 
    if (sta /= NF_NOERR) call err_handler(1, 'error write output ps short')
  endif

  !c+++ close
  if (it == nt) sta = nf_close(fileid) 

  return 
end subroutine writenc_sig

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c subroutine writenc_eta
!c itype = 0: real(4), itype = 1: integer(2)
!c====
subroutine writenc_eta(nx, ny, np, nt, itype, it, lon, lat, lev, &
& time, time1, timeb, jtime, dout, &
& eta_fa, eta_fb, ps, PS_UNITS, rmin_ps, rmax_ps, &
& afile, scale, offset, rmiss, rmin, rmax, REC_UNITS, &
& DATA_UNITS, DATA_NAME, DATA_LNAME, DATA_SNAME, DATA_MESR, DATA_METD, &
& otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, np, nt    !! x-, y-, z-, t- axis sizes
  integer(kind=i4b), intent(in) :: itype             !! output data format
  integer(kind=i4b), intent(in) :: it                !! record number
  real(kind=r8b), intent(in)    :: lon(nx)           !! lon(deg)
  real(kind=r8b), intent(in)    :: lat(ny)           !! lat(deg)
  real(kind=r8b), intent(in)    :: lev(np)           !! levels(hPa)
  real(kind=r8b), intent(in)    :: time(nt)          !! time(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: time1(nt)         !! time1(hours since asyy-1-1)
  real(kind=r8b), intent(in)    :: timeb(nb,nt)      !! time boundaries
  integer(kind=i4b), intent(in) :: jtime(ni,nt)      !! ccmi timestamp
  !c+++ for output data
  real(kind=r4b), intent(in)    :: dout(nx,ny,np)    !! output data
  real(kind=r8b), intent(in)    :: eta_fa(np)        !! eta levels()
  real(kind=r8b), intent(in)    :: eta_fb(np)        !! eta levels()
  real(kind=r4b), intent(in)    :: ps(nx,ny)         !! ps data
  character(len=*), intent(in)  :: PS_UNITS          !! units of Ps data
  real(kind=r4b), intent(in)    :: rmin_ps           !! min. values
  real(kind=r4b), intent(in)    :: rmax_ps           !! max. values
  character(len=*), intent(in)  :: afile             !! output file name
  real(kind=r4b), intent(in)    :: scale, offset     !! output: dout/scale - offset
  real(kind=r4b), intent(in)    :: rmiss             !! missing values
  real(kind=r4b), intent(in)    :: rmin              !! min. values
  real(kind=r4b), intent(in)    :: rmax              !! max. values
  !c+++ unit of output time (e.g. hours since 1860-1-1 00:00:0.0)
  character(len=*), intent(in)  :: REC_UNITS         !! time units
  !c+++ descriptions of output data
  character(len=*), intent(in)  :: DATA_UNITS        !! units of data
  character(len=*), intent(in)  :: DATA_NAME         !! variable name of data
  character(len=*), intent(in)  :: DATA_LNAME        !! long_name of data
  character(len=*), intent(in)  :: DATA_SNAME        !! standard_name of data
  character(len=*), intent(in)  :: DATA_MESR         !! output data cell_measures
  character(len=*), intent(in)  :: DATA_METD         !! output data cell_method
  !c+++ switch
  logical, intent(in)           :: otbnd             !! t: write time_bnd, f: do nothing
  !c+++ global attributes
  integer(kind=i4b), intent(in) :: nattr             !! num. of attributes
  character(len=*), intent(in)  :: GATTR(nattr)      !! fields
  character(len=*), intent(in)  :: DATA_GATTR(nattr) !! values
  character(len=*), intent(in)  :: TYPE_GATTR(nattr) !! data type

  !c+++ [internal]
  !c+++ data for write
  integer(kind=i2b)             :: sdata(nx,ny,np,1) !! data
  integer(kind=i2b)             :: sdata_ps(nx,ny,1) !! ps-data
  real(kind=r4b)                :: rdata(nx,ny,np,1) !! data
  real(kind=r4b)                :: rdata_ps(nx,ny,1) !! ps-data
  !c+++ axis name
  character(len=*), parameter   :: LVL_NAME = 'lev'
  character(len=*), parameter   :: ETA_NAME = 'ap'
  character(len=*), parameter   :: ETB_NAME = 'b'
  !c+++ axis unit
  character(len=*), parameter   :: LVL_UNITS = 'hPa'
  character(len=*), parameter   :: ETA_UNITS = 'Pa'
  character(len=*), parameter   :: ETB_UNITS = '1'
  !ccc character(len=*), parameter :: REC_UNITS = 'hours since 1-1-1 00:00:0.0'
  !c+++ axis long_name
  character(len=*), parameter   :: LVL_LNAME = 'Model Level'
  character(len=*), parameter   :: ETA_LNAME = 'Model a coefficient'
  character(len=*), parameter   :: ETB_LNAME = 'Model b coefficient'
  !c+++ axis standard_name
  !ccc character(len=*), parameter :: LVL_SNAME = 'air_pressure'
  character(len=*), parameter   :: LVL_SNAME = 'atmosphere_hybrid_sigma_pressure_coordinate'
  character(len=*), parameter   :: ETA_SNAME = 'atmosphere_hybrid_sigma_coordinate'
  character(len=*), parameter   :: ETB_SNAME = 'atmosphere_hybrid_sigma_coordinate'
  !c+++ for levels
  character(len=*), parameter   :: HTAP = 'HTAP_name'
  character(len=*), parameter   :: LVL_HTAP = 'atmosphere_hybrid_sigma_pressure_coordinate'
  character(len=*), parameter   :: POSI = 'positive'
  character(len=*), parameter   :: LVL_POSI = 'down'
  character(len=*), parameter   :: LVL_GRNM = 'hPa'
  character(len=*), parameter   :: FMLA = 'formula'
  character(len=*), parameter   :: LVL_FMLA = 'p(n,k,j,i) = ap(k) + b(k)*ps(n,j,i)'
  !c+++ internal save
  logical, save                 :: ofirst = .true.   !!
  logical, save                 :: owtim1 = .true.   !! t: write time1
  logical, save                 :: owtccm = .true.   !! t: write ccmi_time

  start(1:4) = (/1,1,1,1/)
  range(1:4) = (/nx,ny,np,1/)
  start_ps(1:3) = (/1,1,1/)
  range_ps(1:3) = (/nx,ny,1/)
  start_recb(1:2) = (/1,1/)
  range_recb(1:2) = (/nb,nt/)

  if (ofirst) then
    ofirst = .false.
    !c+++ time1
    if (time(1) == time1(1)) owtim1 = .false.
    !c+++ ccmi time
    if (jtime(1,1) < 0) owtccm = .false.

    !c+++ CMOR
    PS_CMOR(1) = PS_MESR_i
    PS_CMOR(2) = DATA_METD
    DATA_CMOR(1) = DATA_MESR
    DATA_CMOR(2) = DATA_METD

    !c+++ open
    sta = nf_create(afile, ior(nf_clobber, nf_64bit_offset), fileid) 
    if (sta /= NF_NOERR) call err_handler(1, 'error creating file ')

    !c+++ set dimensions
    sta = nf_def_dim(fileid, LVL_NAME, np, dimid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error lev')
    sta = nf_def_dim(fileid, LAT_NAME, ny, dimid_lat) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lat')
    sta = nf_def_dim(fileid, LON_NAME, nx, dimid_lon) 
    if (sta /= NF_NOERR) call err_handler(1, 'error lon')
    !ccc sta = nf_def_dim(fileid, REC_NAME, NF_UNLIMITED, dimid_rec) 
    sta = nf_def_dim(fileid, REC_NAME, nt, dimid_rec) 
    if (sta /= NF_NOERR) call err_handler(1, 'error rec')
    if (otbnd) then
      sta = nf_def_dim(fileid, BND_NAME, nb, dimid_bnd)
      if (sta /= NF_NOERR) call err_handler(1, 'error bnds')
    endif
    if (owtim1) then
      sta = nf_def_dim(fileid, REC1_NAME, nt, dimid_rec1) 
      if (sta /= NF_NOERR) call err_handler(1, 'error rec1')
    endif
    sta = nf_def_dim(fileid, ETA_NAME, np, dimid_eta)
    if (sta /= NF_NOERR) call err_handler(1, 'error eta')
    sta = nf_def_dim(fileid, ETB_NAME, np, dimid_etb)
    if (sta /= NF_NOERR) call err_handler(1, 'error etb')

    !c+++ define the coordinate dimensions
    sta = nf_def_var(fileid, LVL_NAME, NF_DOUBLE, 1, dimid_lev, varid_lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev coordinate')
    sta = nf_def_var(fileid, LAT_NAME, NF_DOUBLE, 1, dimid_lat, varid_lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat coordinate')
    sta = nf_def_var(fileid, LON_NAME, NF_DOUBLE, 1, dimid_lon, varid_lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon coordinate')
    sta = nf_def_var(fileid, REC_NAME, NF_DOUBLE, 1, dimid_rec, varid_rec)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time coordinate')
    if (otbnd) then
      sta = nf_def_var(fileid, BND_NAME, NF_DOUBLE, 1, dimid_bnd, varid_bnd)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds coordinate')
    endif
    if (owtim1) then
      sta = nf_def_var(fileid, REC1_NAME, NF_DOUBLE, 1, dimid_rec1, varid_rec1)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 coordinate')
    endif
    sta = nf_def_var(fileid, ETA_NAME, NF_DOUBLE, 1, dimid_eta, varid_eta)
    if (sta /= NF_NOERR) call err_handler(1, 'error define eta coordinate')
    sta = nf_def_var(fileid, ETB_NAME, NF_DOUBLE, 1, dimid_etb, varid_etb)
    if (sta /= NF_NOERR) call err_handler(1, 'error define etb coordinate')

    !c+++ definition of axis
    sta = nf_put_att_text(fileid, dimid_lev, AXIS, len(LVL_AXIS), LVL_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev axis')
    sta = nf_put_att_text(fileid, dimid_lat, AXIS, len(LAT_AXIS), LAT_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat axis')
    sta = nf_put_att_text(fileid, dimid_lon, AXIS, len(LON_AXIS), LON_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon axis')
    sta = nf_put_att_text(fileid, dimid_rec, AXIS, len(REC_AXIS), REC_AXIS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time axis')

    !c+++ units for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, UNITS, len(LVL_UNITS), LVL_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev units')
    sta = nf_put_att_text(fileid, dimid_lat, UNITS, len(LAT_UNITS), LAT_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat units')
    sta = nf_put_att_text(fileid, dimid_lon, UNITS, len(LON_UNITS), LON_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon units')
    sta = nf_put_att_text(fileid, dimid_rec, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define time units')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, UNITS, len(trim(BND_UNITS)), trim(BND_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds units')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 units')
    endif
    sta = nf_put_att_text(fileid, dimid_eta, UNITS, len(ETA_UNITS), ETA_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define eta units')
    sta = nf_put_att_text(fileid, dimid_etb, UNITS, len(ETB_UNITS), ETB_UNITS)
    if (sta /= NF_NOERR) call err_handler(1, 'error define etb units')

    !c+++ long names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, LNAME, len(LVL_LNAME), LVL_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev long name')
    sta = nf_put_att_text(fileid, dimid_lat, LNAME, len(LAT_LNAME), LAT_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat long name')
    sta = nf_put_att_text(fileid, dimid_lon, LNAME, len(LON_LNAME), LON_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon long name')
    sta = nf_put_att_text(fileid, dimid_rec, LNAME, len(REC_LNAME), REC_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time long name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, LNAME, len(BND_LNAME), BND_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds long name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, LNAME, len(REC1_LNAME), REC1_LNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 long name')
    endif
    sta = nf_put_att_text(fileid, dimid_eta, LNAME, len(ETA_LNAME), ETA_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define eta long name')
    sta = nf_put_att_text(fileid, dimid_etb, LNAME, len(ETB_LNAME), ETB_LNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define etb long name')

    !c+++ standard names for the coordinate variables
    sta = nf_put_att_text(fileid, dimid_lev, SNAME, len(LVL_SNAME), LVL_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev standard name')
    sta = nf_put_att_text(fileid, dimid_lat, SNAME, len(LAT_SNAME), LAT_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat standard name')
    sta = nf_put_att_text(fileid, dimid_lon, SNAME, len(LON_SNAME), LON_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon standard name')
    sta = nf_put_att_text(fileid, dimid_rec, SNAME, len(REC_SNAME), REC_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time standard name')
    if (otbnd) then
      sta = nf_put_att_text(fileid, dimid_bnd, SNAME, len(BND_SNAME), BND_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds standard name')
    endif
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, SNAME, len(REC1_SNAME), REC1_SNAME)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 standard name')
    endif
    sta = nf_put_att_text(fileid, dimid_eta, SNAME, len(ETA_SNAME), ETA_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define eta standard name')
    sta = nf_put_att_text(fileid, dimid_etb, SNAME, len(ETB_SNAME), ETB_SNAME)
    if (sta /= NF_NOERR) call err_handler(1, 'error define etb standard name')

    !c+++ CMOR for the coordinate variables
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, dimid_lev, CMOR(i), len(trim(LVL_CMOR(i))), trim(LVL_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lev '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lat, CMOR(i), len(trim(LAT_CMOR(i))), trim(LAT_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lat '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_lon, CMOR(i), len(trim(LON_CMOR(i))), trim(LON_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define lon '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_rec, CMOR(i), len(trim(REC_CMOR(i))), trim(REC_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time '//trim(hcmor))
      if (owtim1) then
        sta = nf_put_att_text(fileid, dimid_rec1, CMOR(i), len(trim(REC1_CMOR(i))), trim(REC1_CMOR(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define time1 '//trim(hcmor))
      endif
      sta = nf_put_att_text(fileid, dimid_eta, CMOR(i), len(trim(ETA_CMOR(i))), trim(ETA_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define eta '//trim(hcmor))
      sta = nf_put_att_text(fileid, dimid_etb, CMOR(i), len(ETB_CMOR(i)), ETB_CMOR(i))
      if (sta /= NF_NOERR) call err_handler(1, 'error define etb '//trim(hcmor))
    enddo !! i

    !c+++ for levels
    sta = nf_put_att_text(fileid, dimid_lev, HTAP, len(LVL_HTAP), LVL_HTAP)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev HTAP')
    sta = nf_put_att_text(fileid, dimid_lev, POSI, len(LVL_POSI), LVL_POSI)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev positive')
    sta = nf_put_att_int2(fileid, dimid_lev, GRIB, NF_INT2, 1, LVL_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_id')
    sta = nf_put_att_text(fileid, dimid_lev, GRNM, len(LVL_GRNM), LVL_GRNM)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev GRIB_name')
    sta = nf_put_att_text(fileid, dimid_lev, FMLA, len(LVL_FMLA), LVL_FMLA)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev formula')

    !c+++ for time
    sta = nf_put_att_text(fileid, dimid_rec, CALN, len(REC_CALN), REC_CALN)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time calendar')
    if (owtim1) then
      sta = nf_put_att_text(fileid, dimid_rec1, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 calendar')
    endif

    !c+++ axis ranges
    COM_DRANG(1) = minval(lev)
    COM_DRANG(2) = maxval(lev)
    sta = nf_put_att_double(fileid, dimid_lev, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lev range')
    COM_DRANG(1) = minval(lat)
    COM_DRANG(2) = maxval(lat)
    sta = nf_put_att_double(fileid, dimid_lat, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lat range')
    COM_DRANG(1) = minval(lon)
    COM_DRANG(2) = maxval(lon)
    sta = nf_put_att_double(fileid, dimid_lon, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define lon range')
    COM_DRANG(1) = time(1)
    COM_DRANG(2) = time(nt)
    sta = nf_put_att_double(fileid, dimid_rec, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define time range')
    if (otbnd) then
      COM_DRANG(1) = bnds(1)
      COM_DRANG(2) = bnds(nb)
      sta = nf_put_att_double(fileid, dimid_bnd, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define bnds range')
    endif
    if (owtim1) then
      COM_DRANG(1) = time1(1)
      COM_DRANG(2) = time1(nt)
      sta = nf_put_att_double(fileid, dimid_rec1, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time1 range')
    endif
    COM_DRANG(1) = minval(eta_fa)
    COM_DRANG(2) = maxval(eta_fa)
    sta = nf_put_att_double(fileid, dimid_eta, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define eta range')
    COM_DRANG(1) = minval(eta_fb)
    COM_DRANG(2) = maxval(eta_fb)
    sta = nf_put_att_double(fileid, dimid_etb, RANG, NF_DOUBLE, 2, COM_DRANG)
    if (sta /= NF_NOERR) call err_handler(1, 'error define etb range')

    !c+++ CCMI time data
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        !c+++ define the netCDF variables for the CCMI data
        sta = nf_def_var(fileid, CCMI_NAME(i), NF_INT, 1, dimid_rec, varid_ccm(i))
        if (sta /= NF_NOERR) call err_handler(1, 'error define coordinate '//trim(hcmor))
        !c+++ write units attributes for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), UNITS, len_trim(CCMI_UNITS(i)), trim(CCMI_UNITS(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define units '//trim(hcmor))
        !c+++ write long name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), LNAME, len_trim(CCMI_LNAME(i)), trim(CCMI_LNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define long name '//trim(hcmor))
        !c+++ write standard name for the CCMI data
        sta = nf_put_att_text(fileid, varid_ccm(i), SNAME, len_trim(CCMI_SNAME(i)), trim(CCMI_SNAME(i)))
        if (sta /= NF_NOERR) call err_handler(1, 'error define standard name '//trim(hcmor))
        !c+++ write scale factor for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), 'scale_factor', NF_INT, 1, 1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write scale factor '//trim(hcmor))
        sta = nf_put_att_int(fileid, varid_ccm(i), 'add_offset', NF_INT, 1, 0)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write offset '//trim(hcmor))
        !c+++ write missing value for the CCMI data
        sta = nf_put_att_int(fileid, varid_ccm(i), '_FillValue', NF_INT, 1, -1)
        sta = nf_put_att_int(fileid, varid_ccm(i), 'missing_value', NF_INT, 1, -1)
        if (sta /= NF_NOERR) call err_handler(sta, 'error write missing_value '//trim(hcmor))
      enddo
    endif

    !c+++ time_bnds data
    if (otbnd) then
      !c+++ set dim. id for the time_bnds data
      dimids_recb(1) = dimid_bnd
      dimids_recb(2) = dimid_rec
      !c+++ define the netCDF variables for the time_bnds data
      sta = nf_def_var(fileid, RECB_NAME, NF_DOUBLE, ndims_recb, dimids_recb, varid_recb)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds coordinate')
      !c+++ write units attributes for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, UNITS, len(trim(REC_UNITS)), trim(REC_UNITS))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds units')
      !c+++ write long name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, LNAME, len(trim(RECB_LNAME)), trim(RECB_LNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds long name')
      !c+++ write standard name for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, SNAME, len(trim(RECB_SNAME)), trim(RECB_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds standard name')
      !c+++ write calendar for the time_bnds data
      sta = nf_put_att_text(fileid, varid_recb, CALN, len(REC_CALN), REC_CALN)
      if (sta /= NF_NOERR) call err_handler(1, 'error define time_bnds calendar')
      !c+++ write min., and max. values of time_bnds data
      COM_DRANG(1) = minval(timeb)
      COM_DRANG(2) = maxval(timeb)
      sta = nf_put_att_double(fileid, varid_recb, RANG, NF_DOUBLE, 2, COM_DRANG)
      if (sta /= NF_NOERR) call err_handler(sta, 'error write bnds actual_range')
    endif

    !c+++ set dim. id for the output data
    dimids(1) = dimid_lon
    dimids(2) = dimid_lat
    dimids(3) = dimid_lev
    dimids(4) = dimid_rec
    !c+++ define the netCDF variables for the output data
    if (itype == 0) then
      sta = nf_def_var(fileid, DATA_NAME, NF_REAL, ndims, dimids, varid_data)
    else
      sta = nf_def_var(fileid, DATA_NAME, NF_INT2, ndims, dimids, varid_data)
    endif
    if (sta /= NF_NOERR) call err_handler(1, 'error define data coordinate')

    !c+++ set dim. id for the ps data
    dimids_ps(1) = dimid_lon
    dimids_ps(2) = dimid_lat
    dimids_ps(3) = dimid_rec
    !c+++ define the netCDF variables for the ps data
    if (itype == 0) then
      sta = nf_def_var(fileid, PS_NAME, NF_REAL, ndims_ps, dimids_ps, varid_ps)
    else
      sta = nf_def_var(fileid, PS_NAME, NF_INT2, ndims_ps, dimids_ps, varid_ps)
    endif
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps coordinate')

    !c+++ assign units attributes to the netCDF variable
    sta = nf_put_att_text(fileid, varid_data, UNITS, len(trim(DATA_UNITS)), trim(DATA_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data units')
    sta = nf_put_att_text(fileid, varid_ps, UNITS, len(trim(PS_UNITS)), trim(PS_UNITS))
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps units')

    !c+++ write long name
    sta = nf_put_att_text(fileid, varid_data, LNAME, len(trim(DATA_LNAME)), trim(DATA_LNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define data long name')
    sta = nf_put_att_text(fileid, varid_ps, LNAME, len(trim(PS_LNAME)), trim(PS_LNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps long name')

    !c+++ write standard name
    if (trim(DATA_SNAME) /= 'none') then
      sta = nf_put_att_text(fileid, varid_data, SNAME, len(trim(DATA_SNAME)), trim(DATA_SNAME))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data standard name')
    endif
    sta = nf_put_att_text(fileid, varid_ps, SNAME, len(trim(PS_SNAME)), trim(PS_SNAME))
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps standard name')

    !c+++ write CMOR
    do i = 1, ncmor
      write(hcmor, '(a, i1, a)') 'CMOR(', i, ')'
      sta = nf_put_att_text(fileid, varid_data, DCMOR(i), len(trim(DATA_CMOR(i))), trim(DATA_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define data '//trim(hcmor))
      sta = nf_put_att_text(fileid, varid_ps, DCMOR(i), len(trim(PS_CMOR(i))), trim(PS_CMOR(i)))
      if (sta /= NF_NOERR) call err_handler(1, 'error define ps '//trim(hcmor))
    enddo !! i

    !c+++ write grib id
    sta = nf_put_att_int2(fileid, varid_data, GRIB, NF_INT2, 1, DATA_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define data GRIB_id')
    sta = nf_put_att_int2(fileid, varid_ps, GRIB, NF_INT2, 1, PS_GRIB)
    if (sta /= NF_NOERR) call err_handler(1, 'error define ps GRIB_id')

    !c+++ write scale factor and offset of data
    sta = nf_put_att_real(fileid, varid_data, 'scale_factor', NF_REAL, 1, scale)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data scale factor')
    sta = nf_put_att_real(fileid, varid_data, 'add_offset', NF_REAL, 1, offset)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data offset')

    !c+++ write scale factor and offset of ps data
    sta = nf_put_att_real(fileid, varid_ps, 'scale_factor', NF_REAL, 1, scale)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps scale factor')
    sta = nf_put_att_real(fileid, varid_ps, 'add_offset', NF_REAL, 1, offset)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps offset')

    !c+++ write missing values of data
    if (itype == 0) then
      rdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_real(fileid, varid_data, '_FillValue', NF_REAL, 1, rdata(1,1,1,1))
      sta = nf_put_att_real(fileid, varid_data, 'missing_value', NF_REAL, 1, rdata(1,1,1,1))
    else
      sdata(1,1,1,1) = rmiss / scale - offset
      sta = nf_put_att_int2(fileid, varid_data, '_FillValue', NF_INT2, 1, sdata(1,1,1,1))
      sta = nf_put_att_int2(fileid, varid_data, 'missing_value', NF_INT2, 1, sdata(1,1,1,1))
    endif
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data missing_value')

    !c+++ write missing values of ps data
    if (itype == 0) then
      rdata_ps(1,1,1) = rmiss / scale - offset
      sta = nf_put_att_real(fileid, varid_ps, '_FillValue', NF_REAL, 1, rdata_ps(1,1,1))
      sta = nf_put_att_real(fileid, varid_ps, 'missing_value', NF_REAL, 1, rdata_ps(1,1,1))
    else
      sdata_ps(1,1,1) = rmiss / scale - offset
      sta = nf_put_att_int2(fileid, varid_ps, '_FillValue', NF_INT2, 1, sdata_ps(1,1,1))
      sta = nf_put_att_int2(fileid, varid_ps, 'missing_value', NF_INT2, 1, sdata_ps(1,1,1))
    endif
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps missing_value')

    !c+++ write min., and max. values of data
    COM_RANG(1) = minval(dout)
    COM_RANG(2) = maxval(dout)
    sta = nf_put_att_real(fileid, varid_data, RANG, NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data actual_range')
    COM_RANG(1) = rmin
    COM_RANG(2) = rmax
    sta = nf_put_att_real(fileid, varid_data, 'valid_range', NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write data valid_range')

    !c+++ write min., and max. values of ps data
    COM_RANG(1) = minval(ps)
    COM_RANG(2) = maxval(ps)
    sta = nf_put_att_real(fileid, varid_ps, RANG, NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps actual_range')
    COM_RANG(1) = rmin_ps
    COM_RANG(2) = rmax_ps
    sta = nf_put_att_real(fileid, varid_ps, 'valid_range', NF_REAL, 2, COM_RANG)
    if (sta /= NF_NOERR) call err_handler(sta, 'error write ps valid_range')

    !c+++ write global attributes
    do i = 1, nattr
      if (trim(TYPE_GATTR(i)) == 'TEXT') then !! character
        sta = nf_put_att_text(fileid, NF_GLOBAL, trim(GATTR(i)), len(trim(DATA_GATTR(i))), trim(DATA_GATTR(i)))
      else if (trim(TYPE_GATTR(i)) == 'INT') then !! integer(4)
        read(DATA_GATTR(i), *) wgi4
        sta = nf_put_att_int(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT, 1, wgi4)
      else if (trim(TYPE_GATTR(i)) == 'INT2') then !! integer(2)
        read(DATA_GATTR(i), *) wgi2
        sta = nf_put_att_int2(fileid, NF_GLOBAL, trim(GATTR(i)), NF_INT2, 1, wgi2)
      else if (trim(TYPE_GATTR(i)) == 'REAL') then !! real(4)
        read(DATA_GATTR(i), *) wgr4
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), 1, wgr4)
        sta = nf_put_att_real(fileid, NF_GLOBAL, trim(GATTR(i)), NF_REAL, 1, wgr4)
      else if (trim(TYPE_GATTR(i)) == 'DOUBLE') then !! real(8)
        read(DATA_GATTR(i), *) wgr8
        sta = nf_put_att_double(fileid, NF_GLOBAL, trim(GATTR(i)), NF_DOUBLE, 1, wgr8)
      else !! error
        sta = NF_NOERR + 1
      endif
      if (sta /= NF_NOERR) call err_handler(i, 'error define global attributes')
    enddo !! i

    !c+++ end define mode
    sta = nf_enddef(fileid)
    if (sta /= NF_NOERR) call err_handler(1, 'error enddef')

    !c+++ write the coordinate variable data
    sta = nf_put_var_double(fileid, varid_lev, lev)
    if (sta /= NF_NOERR) call err_handler(1, 'error write level data')
    sta = nf_put_var_double(fileid, varid_lat, lat)
    if (sta /= NF_NOERR) call err_handler(1, 'error write latitude data')
    sta = nf_put_var_double(fileid, varid_lon, lon)
    if (sta /= NF_NOERR) call err_handler(1, 'error write longitude data')
    sta = nf_put_var_double(fileid, varid_rec, time)
    if (sta /= NF_NOERR) call err_handler(1, 'error write time data')
    if (otbnd) then
      sta = nf_put_var_double(fileid, varid_bnd, bnds)
      if (sta /= NF_NOERR) call err_handler(1, 'error write bnds data')
    endif
    if (owtim1) then
      sta = nf_put_var_double(fileid, varid_rec1, time1)
      if (sta /= NF_NOERR) call err_handler(1, 'error write time1 data')
    endif
    sta = nf_put_var_double(fileid, varid_eta, eta_fa)
    if (sta /= NF_NOERR) call err_handler(1, 'error write eta data')
    sta = nf_put_var_double(fileid, varid_etb, eta_fb)
    if (sta /= NF_NOERR) call err_handler(1, 'error write etb data')
    !c+++ write CCMI time
    if (owtccm) then
      do i = 1, ni
        write(hcmor, '(a, i1, a)') 'CCMI(', i, ')'
        sta = nf_put_var_int(fileid, varid_ccm(i), jtime(i,1:nt))
        if (sta /= NF_NOERR) call err_handler(1, 'error write data '//trim(hcmor))
      enddo
    endif
    !c+++ write time_bnds
    if (otbnd) then
      sta = nf_put_vara_double(fileid, varid_recb, start_recb, range_recb, timeb) 
      if (sta /= NF_NOERR) call err_handler(1, 'error write output time_bnds double')
    endif
  endif

  !c+++ write output data
  start(4) = it
  if (itype == 0) then
    rdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_real(fileid, varid_data, start, range, rdata)
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data real')
  else
    sdata(1:nx,1:ny,1:np,1) = dout(1:nx,1:ny,1:np) / scale - offset
    sta = nf_put_vara_int2(fileid, varid_data, start, range, sdata) 
    if (sta /= NF_NOERR) call err_handler(1, 'error write output data short')
  endif

  !c+++ write output ps data
  start_ps(3) = it
  if (itype == 0) then
    rdata_ps(1:nx,1:ny,1) = ps(1:nx,1:ny) / scale - offset
    sta = nf_put_vara_real(fileid, varid_ps, start_ps, range_ps, rdata_ps)
    if (sta /= NF_NOERR) call err_handler(1, 'error write output ps real')
  else
    sdata_ps(1:nx,1:ny,1) = ps(1:nx,1:ny) / scale - offset
    sta = nf_put_vara_int2(fileid, varid_ps, start_ps, range_ps, sdata_ps) 
    if (sta /= NF_NOERR) call err_handler(1, 'error write output ps short')
  endif

  !c+++ close
  if (it == nt) sta = nf_close(fileid) 

  return 
end subroutine writenc_eta

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c subroutine err_handler
!c====
subroutine err_handler(ierr, err_msg)
  implicit none
  integer(kind=i4b), intent(in) :: ierr
  character(len=*), intent(in)  :: err_msg
  write(6, *) err_msg, ierr
  stop
end subroutine err_handler

!c----------------------------------------------------------------------c

!c======================================================================c

end module wnc
