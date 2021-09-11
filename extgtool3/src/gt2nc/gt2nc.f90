!c
!c  program gt2nc2
!c  [history] 
!c  2013/07/17 Yamashita: first ver.
!c  2013/11/25 Yamashita: add eta-levels
!c  2013/11/26 Yamashita: add sigma-levels
!c  2013/12/09 Yamashita: merge with CCMI_progs (use common ucaln, rwgtool)
!c  2014/02/18 Yamashita: 64-bit offset netCDF file
!c  2014/07/25 Yamashita: add global attributes
!c  2014/08/26 Yamashita: add time1 array/-cent option
!c  2014/08/29 Yamashita: add cell_measures & cell_method
!c  2014/09/02 Yamashita: add time_bnds & ccmi_date outputs
!c  2015/01/13 Yamashita: option to add time_bnds
!c  2015/02/04 Yamashita: add option miss & call repmis (for replace)
!c  2015/02/04 Yamashita: add option ncsta & nctunit
!c  2015/02/04 Yamashita: add time unit for REC_UNITS & add global attributes
!c  2015/06/26 Yamashita: add option ini & phy
!c  2015/06/26 Yamashita: add zlev to zax option
!c  2015/07/17 Yamashita: add psfc to zax option
!c  2017/07/15 Yamashita: modify I/O
!c  2017/07/15 Yamashita: use common_typedef
!c  2021/03/26 Yamashita: modify GATTR for CCMI-2022
!c  2021/05/28 Yamashita: modify tracking_id format
!c  2021/08/28 Yamashita: modify source_id format
!c  2021/08/28 Yamashita: add data type of GATTR
!c
!c  gtool3 format ==> netcdf format
!c
!c  internal: subroutine getparms
!c    external module uopts
!c             subroutines read_parms, get_parms, get_strend
!c    external module char2var
!c             subroutine c2var
!c    internal: subroutine xabort
!c  external: module common_args
!c  external: module calendar
!c            subroutine dayloop
!c  external: module rwgtool
!c            subroutines gtopen, gtrewind, gtclose, rgth, rgthd, rgt,
!c            get_hdtime2, get_etacoef, wmsg
!c                        
!c  external: module wnc
!c            subroutines writenc, writenc_z, writenc_sig, writenc_eta
!c  external: module util
!c            subroutines get_axname, getxaxis, getyaxis, getzaxis,
!c            get_date2, repmis_r4
!c  external: module error_handler
!c            subroutines ioerror
!c
!c======================================================================c
program main
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use ucaln, only: dayloop
  use rwgtool, only: gtopen, gtrewind, gtclose, rgth, rgthd, rgt, get_hdtime2, get_etacoef, wmsg
  use wnc, only: writenc, writenc_z, writenc_sig, writenc_eta
  use util, only: get_axname, getxaxis, getyaxis, getzaxis, get_date2, repmis_r4
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input data
  character(len=ncc)             :: head(ndc)         !! header
  character(len=ncc)             :: head2(ndc)        !! header of ps
  !c+++ input from gtool3 header
  integer(kind=i4b)              :: imax              !! x- axis size
  integer(kind=i4b)              :: jmax              !! y- axis size
  integer(kind=i4b)              :: kmax              !! z- axis size
  character(len=ncc)             :: haxisx            !! x- axis name
  character(len=ncc)             :: haxisy            !! y- axis name
  character(len=ncc)             :: haxisz            !! z- axis name
  real(kind=r4b)                 :: rmiss4            !! missing value
  real(kind=r8b)                 :: rmiss8            !! missing value
  character(len=ncc)             :: htunit            !! unit of time
  real(kind=r8b)                 :: tout              !! time
  real(kind=r8b)                 :: tdur              !! time step
  integer(kind=i4b)              :: jdate(6)          !! time array (year,mon,day,hour,minute,second)
  integer(kind=i4b)              :: jdate1(6)         !! str. time array (year,mon,day,hour,minute,second)
  integer(kind=i4b)              :: jdate2(6)         !! end. time array (year,mon,day,hour,minute,second)
  logical                        :: oavr              !! t: averaged data, f: instantaneous
  !c+++ input from data
  integer(kind=i4b)              :: nt                !! t- axis size
  !c+++ input from getparms
  integer(kind=i4b)              :: ista              !! start record
  integer(kind=i4b)              :: iend              !! end record
  real(kind=r4b)                 :: rmisr             !! missing. value (replace)
  real(kind=r4b)                 :: rmin              !! minimum value
  real(kind=r4b)                 :: rmax              !! maximum value
  real(kind=r4b)                 :: rmin_ps           !! minimum value of ps
  real(kind=r4b)                 :: rmax_ps           !! maximum value of ps
  character(len=nfiln)           :: ifile             !! input file name
  character(len=nfiln)           :: ips               !! input ps file name
  character(len=nfiln)           :: ofile             !! output file name
  character(len=ncc)             :: hitem             !! item (variable name)
  character(len=nfiln)           :: hlitm             !! items long_name
  character(len=nfiln)           :: hsitm             !! items standard_name
  character(len=ncc)             :: hunit             !! items unit
  character(len=ncc)             :: hfreq             !! global attributes frequency
  character(len=ncc*2)           :: hmodl             !! global attributes model_id
  character(len=ncc)             :: hproj             !! global attributes project_id
  character(len=ncc*2)           :: hdset             !! name of data set
  character(len=ncc*2)           :: hexp              !! global attributes experiment
  character(len=ncc)             :: hexi              !! global attributes experiment_id
  character(len=ncc)             :: hgrid             !! global attributes grid_label
  character(len=ncc)             :: hens              !! global attributes realization
  character(len=ncc)             :: hini              !! global attributes initialization
  character(len=ncc)             :: hphy              !! global attributes physics_version
  character(len=ncc)             :: hpexi             !! global attr. parent_experiment_id
  character(len=ncc)             :: hpexr             !! global attr. parent_experiment_rip
  character(len=ncc)             :: hmip              !! global attributes mip-era
  character(len=ncc)             :: hres              !! global attributes nominal_resolution
  character(len=nfiln*5)         :: hlics             !! global attributes licence
  character(len=ncc)             :: hrealm            !! global attributes realm
  character(len=nfiln)           :: hcont             !! global attributes contact
  character(len=nfiln)           :: hfield            !! global attributes field
  character(len=nfiln)           :: htable            !! global attributes table_id
  character(len=nfiln)           :: hsour             !! global attributes source
  character(len=nfiln)           :: hsout             !! global attributes source_type
  character(len=nfiln)           :: hsoui             !! global attributes source_id
  character(len=nfiln)           :: hinst             !! global attributes institution
  character(len=ncc)             :: hinsi             !! global attributes institute_id
  character(len=nfiln)           :: htitl             !! global attributes title
  character(len=nfiln)           :: huuid             !! global attributes tracking_id
  character(len=nfiln)           :: hcmesr            !! output data cell_measures
  character(len=nfiln)           :: hcmetd            !! output data cell_method
  integer(kind=i4b)              :: dsyy              !! start time of t-axis (default:1860)
  character(len=ncc)             :: tunit             !! time unit (days/hours)
  character(len=ncc)             :: zax               !! zaxis plev/siglv/etalv/zlev
  logical                        :: otbnd             !! t: write time_bnd, f: do nothing
  logical                        :: otcent            !! t: write date, f: write date1/date1
  logical                        :: otccmi            !! t: write CCMI time, f: do nothing

  !c+++ [input/output]
  !c+++ data
  real(kind=r8b), allocatable    :: din(:,:,:)        !! input-data
  real(kind=r8b), allocatable    :: psin(:,:)         !! input-ps
  real(kind=r4b), allocatable    :: d(:,:,:)          !! data
  real(kind=r4b), allocatable    :: ps(:,:)           !! Ps
  character(len=ncc)             :: hunitps           !! Ps unit
  !c+++ global attributes
  integer(kind=i4b), parameter   :: nattr = 35        !! number of fields
  character(len=ncc*2)           :: GATTR(nattr)      !! fields
  character(len=nfiln)           :: DATA_GATTR(nattr) !! values
  character(len=ncc)             :: TYPE_GATTR(nattr) !! data type:
                                                      !!  TEXT, INT, INT2, REAL, DOUBLE

  !c+++ [internal work]
  integer(kind=i4b)              :: it                !! loop variables
  integer(kind=i4b)              :: irec              !! record number
  !c+++ I/O unit for input/output files
  integer(kind=i4b)              :: jfile             !! for input file
  integer(kind=i4b)              :: jfile_ps          !! for input ps file
  integer(kind=i4b)              :: ios               !! end code
  !c+++ create time
  character(len=ncc*2)           :: cdate             !! creation_date
  !c+++ lon, lat, levels
  real(kind=r8b), allocatable    :: lon(:)            !! longitude (deg)
  real(kind=r8b), allocatable    :: lat(:)            !! latitude (deg)
  real(kind=r8b), allocatable    :: lev(:)            !! level (z-axis data)

  real(kind=r8b), allocatable    :: eta_fa(:)         !! for eta half lev.
  real(kind=r8b), allocatable    :: eta_fb(:)         !! for eta half lev.
  character(len=ncc)             :: haxiszp           !! p- axis name (for eta lev.)
  !c+++ time
  integer(kind=i4b), parameter   :: ntmax = 99999
  real(kind=r8b), allocatable    :: time(:)           !! time
  real(kind=r8b), allocatable    :: time1(:)          !! additional time
  real(kind=r8b), allocatable    :: timeb(:,:)        !! time boundary
  integer(kind=i4b), allocatable :: jtime(:,:)        !! ccmi timestamp
  character(len=ncc)             :: asyy              !! year (char)
  integer(kind=i4b)              :: jdays             !! days from dsyy/01/01
  character(len=nfiln)           :: REC_UNITS         !! unit for time record
  !c+++ internal switch
  logical                        :: osig = .false.    !! enable sigma-lev.
  logical                        :: oeta = .false.    !! enable eta-lev.
  logical                        :: ozlv = .false.    !! enable z-lev.
  logical                        :: opsf = .false.    !! enable p-surface lev.
  logical                        :: omis = .false.    !! enable replace 

!c
!c init
!c===
  !c+++ get parameters
  call getparms
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev' ) ozlv = .true.
  if (zax == 'psfc' ) opsf = .true.
  if (rmisr /= -1.0) omis = .true.
  if (trim(tunit) /= 'days'.and.trim(tunit) /= 'hours') tunit = 'days'
  write(asyy, '(i4.4)') dsyy
  REC_UNITS = trim(tunit)//' since '//trim(asyy)//'-1-1 00:00:0.0'

!c
!c open
!c===
  !c+++ open input file
  write(6, '(2a)') 'open input file: ', trim(ifile)
  call gtopen(trim(ifile), 'r', jfile, ios)
  if (ios /= 0) call ioerror(jfile, ios)

  !c+++ open input ps file
  if (osig.or.oeta) then
    write(6, '(2a)') 'open input ps file: ', trim(ips)
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
  endif

!c
!c axis
!c===
  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile, head, imax, jmax, kmax, rmiss8, ios)
  if (ios /= 0) call ioerror(jfile, ios)
  call gtrewind(jfile, ios)
  if (ios /= 0) call ioerror(jfile, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss8
  !c+++ x- y- z-axis names from gtool header
  call get_axname(haxisx, haxisy, haxisz, head)

  !c+++ allocate
  !c+++ for axis data
  allocate(lon(imax), lat(jmax), lev(kmax))
  allocate(time(max(ntmax,iend)), time1(max(ntmax,iend)))
  allocate(timeb(2,max(ntmax,iend)), jtime(5,max(ntmax,iend)))
  if (oeta) allocate(eta_fa(kmax), eta_fb(kmax))
  !c+++ for input/output data
  allocate(din(imax,jmax,kmax), d(imax,jmax,kmax))
  if (osig.or.oeta) allocate(psin(imax,jmax), ps(imax,jmax))
  time(:) = -1.d0
  time1(:) = -1.d0
  timeb(:,:) = -1.d0
  jtime(:,:) = -1

  !c+++ get x-axis data
  call getxaxis(imax, haxisx, lon)
  !c+++ get y-axis data
  call getyaxis(jmax, haxisy, lat)
  if (.not. oeta) then
    !c+++ get z-axis data
    call getzaxis(kmax, haxisz, lev)
  else
    !c+++ get z-axis data (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
    eta_fa(1:kmax) = eta_fa(1:kmax) * 100.d0 !! [hPa] ==> [Pa]
    if (haxisz(2:4) == 'ETA') then
      haxiszp = 'PL'//haxisz(5:len_trim(haxisz))
      write(6, *) 'haxiszp = ', haxiszp
    endif
    !c+++ get p-axis data
    call getzaxis(kmax, haxiszp, lev)
    write(6, *) 'eta_fa = ', eta_fa
    write(6, *) 'eta_fb = ', eta_fb
  endif
  write(6, *) 'lon = ', lon
  write(6, *) 'lat = ', lat
  write(6, *) 'lev = ', lev

!c
!c info
!c===
  !c+++ set unit of output data
  if (hunit == 'NULL') hunit = head(16)
  !c+++ set item of output data
  if (hitem == 'NULL') hitem = head(3)
  if (hlitm == 'NULL') hlitm = ' '
  if (hsitm == 'NULL') hsitm = ' '
  !c+++ set title of output data
  if (htitl == 'NULL') then
    htitl(1:16) = head(14)
    htitl(17:32) = head(15)
  endif
  !c+++ set model name/exp./ens.
  if (hmodl == 'NULL') hmodl = ' '
  if (hproj == 'NULL') hproj = ' '
  if (hdset == 'NULL') hdset = head(2)
  if (hexp == 'NULL') hexp = trim(hdset)
  if (hexi == 'NULL') hexi = trim(hdset)
  if (hens == 'NULL') hens = '1'
  if (hgrid  == 'NULL') hgrid = 'N/A'
  if (hini == 'NULL') hini = '1'
  if (hphy == 'NULL') hphy = '1'
  if (hpexi == 'NULL') hpexi = 'none'
  if (hpexr == 'NULL') hpexr = 'none'
  if (hmip == 'NULL') hmip = 'N/A'
  if (hres == 'NULL') hres =  'N/A'
  if (hlics == 'NULL') hlics =  'N/A'
  !c+++ set misc of output data
  if (hfreq == 'NULL') hfreq = ' '
  if (hfield == 'NULL') hfield = ' '
  if (htable == 'NULL') htable = ' '
  if (hsour == 'NULL') hsour = ' '
  if (hsout == 'NULL') hsout = ' '
  if (hinst == 'NULL') hinst = ' '
  if (hinsi == 'NULL') hinsi = ' '
  if (huuid == 'NULL') huuid = ' '
  !c+++ 
  write(6, *) 'set unit = ', trim(hunit)
  write(6, *) 'set variable name = ', trim(hitem)
  write(6, *) 'set long_name = ', trim(hlitm)
  write(6, *) 'set standard_name = ', trim(hsitm)
  write(6, *) 'set title = ', trim(htitl)
  write(6, *) 'set model_id = ', trim(hmodl)
  write(6, *) 'set project_id = ', trim(hproj)
  write(6, *) 'set experiment = ', trim(hexp)
  write(6, *) 'set experiment_id = ', trim(hexi)
  write(6, *) 'set realization = ', trim(hens)
  write(6, *) 'set grid_label = ', trim(hgrid)
  write(6, *) 'set initialization_method = ', trim(hini)
  write(6, *) 'set physics_version = ', trim(hphy)
  write(6, *) 'set parent_experiment_id = ', trim(hpexi)
  write(6, *) 'set sub_experiment = ', trim(hpexr)
  write(6, *) 'set mip-era = ', trim(hmip)
  write(6, *) 'set nominal_resolution = ', trim(hres)
  write(6, *) 'set license = ', trim(hlics)
  write(6, *) 'set realm  = ', trim(hrealm)
  write(6, *) 'set grid = ', trim(hfield)
  write(6, *) 'set table_id = ', trim(htable)
  write(6, *) 'set source = ', trim(hsour)
  write(6, *) 'set source_type = ', trim(hsout)
  write(6, *) 'set source_id = ', trim(hsoui)
  write(6, *) 'set institution = ', trim(hinst)
  write(6, *) 'set institute_id = ', trim(hinsi)
  write(6, *) 'set frequency = ', trim(hfreq)
  write(6, *) 'set tracking_id = ', trim(huuid)
  write(6, *) 'set variable_id = ', trim(hitem)
  write(6, *) 'set variant_label = ', 'r'//trim(hens)//'i'//trim(hini)//'p'//trim(hphy)//'f1'
  !c+++ get creation time
  call get_date2(cdate)

  !c+++ global attributes of NETCDF file
  GATTR(1) = 'activity_id'
  DATA_GATTR(1) = hproj
  TYPE_GATTR(1) = 'TEXT'
  GATTR(2) = 'experiment'
  DATA_GATTR(2) = hexp
  TYPE_GATTR(2) = 'TEXT'
  GATTR(3) = 'experiment_id'
  DATA_GATTR(3) = hexi
  TYPE_GATTR(3) = 'TEXT'
  GATTR(4) = 'realization_index'
  DATA_GATTR(4) = hens
  TYPE_GATTR(4) = 'INT'
  GATTR(5) = 'forcing_index'
  DATA_GATTR(5) = '1'
  TYPE_GATTR(5) = 'INT'
  GATTR(6) = 'frequency'
  DATA_GATTR(6) = hfreq
  TYPE_GATTR(6) = 'TEXT'
  GATTR(7) = 'grid_label'
  DATA_GATTR(7) = hgrid
  TYPE_GATTR(7) = 'TEXT'
  GATTR(8) = 'initialization_index'
  DATA_GATTR(8) = hini
  TYPE_GATTR(8) = 'INT'
  GATTR(9) = 'physics_index'
  DATA_GATTR(9) = hphy
  TYPE_GATTR(9) = 'INT'
  GATTR(10) = 'license'
  DATA_GATTR(10) = hlics
  TYPE_GATTR(10) = 'TEXT'
  GATTR(11) = 'model_id'
  DATA_GATTR(11) = hmodl
  TYPE_GATTR(11) = 'TEXT'
  GATTR(12) = 'realm'
  DATA_GATTR(12) = hrealm
  TYPE_GATTR(12) = 'TEXT'
  GATTR(13) = 'product'
  DATA_GATTR(13) = 'model-output'
  TYPE_GATTR(13) = 'TEXT'
  GATTR(14) = 'table_id'
  DATA_GATTR(14) = htable
  TYPE_GATTR(14) = 'TEXT'
  GATTR(15) = 'tracking_id'
  DATA_GATTR(15) = huuid
  TYPE_GATTR(15) = 'TEXT'
  GATTR(16) = 'variable_id'
  DATA_GATTR(16) = hitem
  TYPE_GATTR(16) = 'TEXT'
  GATTR(17) = 'variant_label'
  DATA_GATTR(17) = 'r'//trim(hens)//'i'//trim(hini)//'p'//trim(hphy)//'f1'
  TYPE_GATTR(17) = 'TEXT'
  GATTR(18) = 'contact'
  DATA_GATTR(18) = hcont
  TYPE_GATTR(18) = 'TEXT'
  GATTR(19) = 'grid'
  DATA_GATTR(19) = hfield
  TYPE_GATTR(19) = 'TEXT'
  GATTR(20) = 'references'
  DATA_GATTR(20) = 'These files are created according to CF1.7 see http://cfconventions.org/'
  TYPE_GATTR(20) = 'TEXT'
  GATTR(21) = 'source'
  DATA_GATTR(21) = hsour
  TYPE_GATTR(21) = 'TEXT'
  GATTR(22) = 'source_id'
  DATA_GATTR(22) = hsoui
  TYPE_GATTR(22) = 'TEXT'
  GATTR(23) = 'source_type'
  DATA_GATTR(23) = hsout
  TYPE_GATTR(23) = 'TEXT'
  GATTR(24) = 'institution'
  DATA_GATTR(24) = hinst
  TYPE_GATTR(24) = 'TEXT'
  GATTR(25) = 'institution_id'
  DATA_GATTR(25) = hinsi
  TYPE_GATTR(25) = 'TEXT'
  GATTR(26) = 'title'
  DATA_GATTR(26) = htitl
  TYPE_GATTR(26) = 'TEXT'
  GATTR(27) = 'sub_experiment'
  DATA_GATTR(27) = hpexr
  TYPE_GATTR(27) = 'TEXT'
  GATTR(28) = 'sub_experiment_id'
  DATA_GATTR(28) = hpexi
  TYPE_GATTR(28) = 'TEXT'
  GATTR(29) = 'mip_era'
  DATA_GATTR(29) = hmip
  TYPE_GATTR(29) = 'TEXT'
  GATTR(30) = 'nominal_resolution'
  DATA_GATTR(30) = hres
  TYPE_GATTR(30) = 'TEXT'
  GATTR(31) = 'Conventions'
  DATA_GATTR(31) = 'CF-1.7'
  TYPE_GATTR(31) = 'TEXT'
  GATTR(32) = 'data_specs_version'
  DATA_GATTR(32) = '01.00.00'
  TYPE_GATTR(32) = 'TEXT'
  GATTR(33) = 'history'
  DATA_GATTR(33) = 'List of the applications that have modified the original data'
  TYPE_GATTR(33) = 'TEXT'
  GATTR(34) = 'creation_date'
  DATA_GATTR(34) = trim(cdate)
  TYPE_GATTR(34) = 'TEXT'
  GATTR(35) = 'created'
  DATA_GATTR(35) = 'by gt2nc (ver. 2021-09-01 Y. Yamashita)'
  TYPE_GATTR(35) = 'TEXT'

!c
!c read GTOOL3 header
!c===
  ista = max(ista, 1)
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ read input file
    call rgth(jfile, head, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile, ios)
    if (it < ista) cycle

    nt = it - ista + 1
    !c+++ set time
    call get_hdtime2(tout, tdur, htunit, jdate, jdate1, jdate2, head, oavr)
    !ccc time(nt) = tout
    if (otcent.or.(.not. oavr)) then
      !c+++ time
      !c+++ yy/mm/dd ==> day
      call dayloop(dsyy, 1, 1, jdate(1), jdate(2), jdate(3), jdays)
      if (trim(tunit) == 'hours') then     !! time unit (hours)
        time(nt) = (jdays - 1) * 24 + jdate(4)
      else if (trim(tunit) == 'days') then !! time unit (days)
        time(nt) = (jdays - 1) + real(jdate(4)) / 24.d0
      endif
      time1(nt) = time(nt)
    else
      !c+++ time
      !c+++ yy/mm/dd ==> day
      call dayloop(dsyy, 1, 1, jdate(1), jdate(2), jdate(3), jdays)
      if (trim(tunit) == 'hours') then     !! time unit (hours)
        time(nt) = (jdays - 1) * 24 + jdate(4)
      else if (trim(tunit) == 'days') then !! time unit (days)
        time(nt) = (jdays - 1) + real(jdate(4)) / 24.d0
      endif
      !c+++ time boundary (start)
      call dayloop(dsyy, 1, 1, jdate1(1), jdate1(2), jdate1(3), jdays)
      if (trim(tunit) == 'hours') then     !! time unit (hours)
        time1(nt) = (jdays - 1) * 24 + jdate1(4)
      else if (trim(tunit) == 'days') then !! time unit (days)
        time1(nt) = (jdays - 1) + real(jdate(4)) / 24.d0
      endif
    endif
    !c+++ time boundary (start)
    call dayloop(dsyy, 1, 1, jdate1(1), jdate1(2), jdate1(3), jdays)
    if (trim(tunit) == 'hours') then     !! time unit (hours)
      timeb(1,nt) = (jdays - 1) * 24 + jdate1(4)
    else if (trim(tunit) == 'days') then !! time unit (days)
      timeb(1,nt) = (jdays - 1) + real(jdate1(4)) / 24.d0
    endif
    !c+++ time boundary (end)
    call dayloop(dsyy, 1, 1, jdate2(1), jdate2(2), jdate2(3), jdays)
    if (trim(tunit) == 'hours') then     !! time unit (hours)
      timeb(2,nt) = (jdays - 1) * 24 + jdate2(4) 
    else if (trim(tunit) == 'days') then !! time unit (days)
      timeb(2,nt) = (jdays - 1) + real(jdate2(4)) / 24.d0
    endif
    !c+++ ccmi time
    if (otccmi) jtime(1:5,nt) = jdate(1:5)

    if (it == iend) exit
  enddo !! it
  call gtrewind(jfile, ios)
  if (ios /= 0) call ioerror(jfile, ios)
  iend = nt

!c
!c read GTOOL3 & write NETCDF
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ read input file
    call rgt(jfile, imax, jmax, kmax, head, din, ios)
    d(1:imax,1:jmax,1:kmax) = din(1:imax,1:jmax,1:kmax)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile, ios)
    if (omis) then
      !c+++ replace missing values by rmisr
      call repmis_r4(imax, jmax, kmax, real(rmiss8), rmisr, d)
      rmiss4 = rmisr
    else
      rmiss4 = real(rmiss8)
    endif

    !c+++ read input ps file
    if (osig.or.oeta) then !! for sig-lev. and eta-lev.
      call rgt(jfile_ps, imax, jmax, 1, head2, psin, ios)
      ps(1:imax,1:jmax) = psin(1:imax,1:jmax)
      hunitps = head2(16)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (omis) then
        !c+++ replace missing values by rmisr
        call repmis_r4(imax, jmax, 1, real(rmiss8), rmisr, ps)
      endif
    endif
    if (it < ista) cycle

    !c+++ write NETCDF file
    irec = it - ista + 1

    if (oeta) then
      !c+++ for eta-levels
      call writenc_eta(imax, jmax, kmax, nt, 0, irec, lon, lat, lev, &
&       time(1:nt), time1(1:nt), timeb(1:2,1:nt), jtime(1:5,1:nt), d, &
&       eta_fa, eta_fb, ps, hunitps, rmin_ps, rmax_ps, &
&       ofile, 1.0, 0.0, rmiss4, rmin, rmax, REC_UNITS, &
&       hunit, hitem, hlitm, hsitm, hcmesr, hcmetd, &
&       otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
    else if (osig) then
      !c+++ for sig-levels
      call writenc_sig(imax, jmax, kmax, nt, 0, irec, lon, lat, lev, &
&       time(1:nt), time1(1:nt), timeb(1:2,1:nt), jtime(1:5,1:nt), d, &
&       ps, hunitps, rmin_ps, rmax_ps, &
&       ofile, 1.0, 0.0, rmiss4, rmin, rmax, REC_UNITS, &
&       hunit, hitem, hlitm, hsitm, hcmesr, hcmetd, &
&       otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
    else if (ozlv) then
      !c+++ for z-levels
      call writenc_z(imax, jmax, kmax, nt, 0, irec, lon, lat, lev, &
&       time(1:nt), time1(1:nt), timeb(1:2,1:nt), jtime(1:5,1:nt), d, &
&       ofile, 1.0, 0.0, rmiss4, rmin, rmax, REC_UNITS, &
&       hunit, hitem, hlitm, hsitm, hcmesr, hcmetd, &
&       otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
    else if (opsf) then
      !c+++ for p-surface levels
      call writenc(imax, jmax, kmax, nt, 0, irec, lon, lat, lev, &
&       time(1:nt), time1(1:nt), timeb(1:2,1:nt), jtime(1:5,1:nt), d, &
&       ofile, 1.0, 0.0, rmiss4, rmin, rmax, 'plev', REC_UNITS, &
&       hunit, hitem, hlitm, hsitm, hcmesr, hcmetd, &
&       otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
    else
      !c+++ for p-levels
      call writenc(imax, jmax, kmax, nt, 0, irec, lon, lat, lev, &
&       time(1:nt), time1(1:nt), timeb(1:2,1:nt), jtime(1:5,1:nt), d, &
&       ofile, 1.0, 0.0, rmiss4, rmin, rmax, 'lev', REC_UNITS, &
&       hunit, hitem, hlitm, hsitm, hcmesr, hcmetd, &
&       otbnd, nattr, GATTR, DATA_GATTR, TYPE_GATTR)
    endif
    call wmsg(head, 'WRITE')

    if (it == iend) exit
  enddo !! it
  !c+++ close input file
  call gtclose(jfile, ios)
  !c+++ deallocate
  deallocate(lon, lat, lev, time, time1, timeb, jtime, din, d)
  if (oeta) deallocate(eta_fa, eta_fb)
  if (osig.or.oeta) deallocate(psin, ps)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c
!c=====
subroutine getparms
  use uopts, only: read_parms, get_parms, get_strend
  use char2var, only: c2var
  !c+++ [internal work]
  character(len=ncc)            :: hval             !!
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output files
  call get_parms('i', 'gtool.in', ifile, ios)   !! input file name
  call get_parms('ps', 'Ps', ips, ios)          !! input ps-file name
  call get_parms('o', 'gtool.nc', ofile, ios)   !! output file name
  !c+++ 
  call get_parms('item', 'NULL', hitem, ios)    !! output file item (variable name)
  call get_parms('l', 'NULL', hlitm, ios)       !! output file item (long)
  call get_parms('s', 'NULL', hsitm, ios)       !! output file item (standard)
  call get_parms('unit', 'NULL', hunit, ios)    !! output file unit
  call get_parms('freq', 'NULL', hfreq, ios)    !! global attributes frequency
  call get_parms('model', 'NULL', hmodl, ios)   !! global attributes model_id
  call get_parms('proj', 'NULL', hproj, ios)    !! global attributes project_id
  call get_parms('dset', 'NULL', hdset, ios)    !! name of data set
  call get_parms('exp', 'NULL', hexp, ios)      !! global attributes experiment
  call get_parms('exi', 'NULL', hexi, ios)      !! global attributes experiment_id
  call get_parms('grid', 'NULL', hgrid, ios)    !! global attributes grid_label
  call get_parms('ens', 'NULL', hens, ios)      !! global attributes realization
  call get_parms('ini', 'NULL', hini, ios)      !! global attributes initialization
  call get_parms('phy', 'NULL', hphy, ios)      !! global attributes physics_version
  call get_parms('pexi', 'NULL', hpexi, ios)    !! global attributes sub_experiment_id
  call get_parms('pexr', 'NULL', hpexr, ios)    !! global attributes sub_experiment
  call get_parms('mip', 'NULL', hmip, ios)      !! global attributes mip-era
  call get_parms('res', 'NULL', hres, ios)      !! global attributes nominal_resolution
  call get_parms('license', 'NULL', hlics, ios) !! global attributes license
  call get_parms('realm', 'atmos', hrealm, ios) !! global attributes realm
  call get_parms('contact', 'NULL', hcont, ios) !! global attributes contact
  call get_parms('field', 'NULL', hfield, ios)  !! global attributes field
  call get_parms('table', 'NULL', htable, ios)  !! global attributes table_id
  call get_parms('source', 'NULL', hsour, ios)  !! global attributes source
  call get_parms('sourcet', 'NULL', hsout, ios) !! global attributes source_type
  call get_parms('sourcei', 'NULL', hsoui, ios) !! global attributes source_id
  call get_parms('inst', 'NULL', hinst, ios)    !! global attributes institution
  call get_parms('insi', 'NULL', hinsi, ios)    !! global attributes institute_id
  call get_parms('titl', 'NULL', htitl, ios)    !! global attributes title
  call get_parms('uuid', 'NULL', huuid, ios)    !! global attributes tracking_id
  call get_parms('cmesr', 'N/A', hcmesr, ios)   !! output data cell_measures
  call get_parms('cmetd', 'N/A', hcmetd, ios)   !! output data cell_method
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ 
  call get_parms('ncsta', '1860', hval, ios)    !! start time of t-axis
  call c2var(dsyy, '(i16)', hval)
  call get_parms('nctunit', 'days', tunit, ios) !! time unit (days/hours)
  call get_parms('miss', '-1.0', hval, ios)     !! missing value (replace)
  call c2var(rmisr, '(1pe15.5)', hval)
  call get_parms('min', '-1e+30', hval, ios)    !! minimum value
  call c2var(rmin, '(1pe15.5)', hval)
  call get_parms('max', '1e+30', hval, ios)     !! maximum value
  call c2var(rmax, '(1pe15.5)', hval)
  call get_parms('psmin', '-1e+30', hval, ios)  !! minimum value of ps
  call c2var(rmin_ps, '(1pe15.5)', hval)
  call get_parms('psmax', '1e+30', hval, ios)   !! maximum value of ps
  call c2var(rmax_ps, '(1pe15.5)', hval)
  !c+++ options
  call get_parms('tbnd', 'f', hval, ios)        !! t: write time_bnd, f: do nothing
  call c2var(otbnd, '(l1)', hval)
  call get_parms('cent', 't', hval, ios)        !! t: write date, f: write date1/date1
  call c2var(otcent, '(l1)', hval)
  call get_parms('ccmi', 'f', hval, ios)        !! t: write CCMI timestamp, f: do nothin
  call c2var(otccmi, '(l1)', hval)
  !c+++ axis
  call get_parms('zax', 'plev', zax, ios)
  if (ios /= 0) call xabort
  if (otccmi) otbnd = .true.

  return
end subroutine getparms

!c---------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: gt2nc'
  write(6, '(a)') '-o output-file'
  write(6, '(a)') '-i input-file'
  write(6, '(a)') '(-ps input-ps-file)'
  write(6, '(a)') '-item item_name (output variable name)'
  write(6, '(a)') '-l item_long_name -s item_standard_name'
  write(6, '(a)') ' (if -s none, the standard_name will be removed)'
  write(6, '(a)') ' '
  write(6, '(a)') '-unit unit -freq frequency'
  write(6, '(a)') ' '
  write(6, '(a)') '-model model_id -proj project_id'
  write(6, '(a)') '-dset dataset_name'
  write(6, '(a)') '-exp experiment_name -exi experiment_id'
  write(6, '(a)') '-ens ensemble_number'
  write(6, '(a)') '-grid grid_label'
  write(6, '(a)') '-ini initialization_method'
  write(6, '(a)') '-phy physics_version'
  write(6, '(a)') ' '
  write(6, '(a)') '-pexi sub_experiment_id'
  write(6, '(a)') '-pexr sub_experiment'
  write(6, '(a)') '-mip mip-era'
  write(6, '(a)') '-res nominal_resolution'
  write(6, '(a)') '-licens licens description'
  write(6, '(a)') '-realm atmos/land'
  write(6, '(a)') ' '
  write(6, '(a)') '-contact information -field description'
  write(6, '(a)') '-table CMOR_table_id'
  write(6, '(a)') '-source source'
  write(6, '(a)') '-inst institution -insi institution_id'
  write(6, '(a)') '-titl title -uuid tracking_id (UUID)'
  write(6, '(a)') '-cmesr cell_measures -cmetd cell_methods'
  write(6, '(a)') ' '
  write(6, '(a)') '-sta start-time -end end-time (gtool record number)'
  write(6, '(a)') '-cent t/f (t: write date, f: write date1/date2)'
  write(6, '(a)') '-ccmi f/t (t: write CCMI timestamp, f: do nothing)'
  write(6, '(a)') '-tbnd f/t (t: write time_bnd, f: do nothing)'
  write(6, '(a)') ' if ccmi=t, tbnd=t is forced'
  write(6, '(a)') ' '
  write(6, '(a)') '-miss missing value (replace)'
  write(6, '(a)') '-min minimum value -max maximum value'
  write(6, '(a)') '(-psmin minimum value of Ps -psmax maximum value of Ps)'
  write(6, '(a)') ' '
  write(6, '(a)') '-ncsta year (netcdf start time)'
  write(6, '(a)') '-nctunit days/hours (netcdf time unit)'
  write(6, '(a)') ' '
  write(6, '(a)') '-zax plev/siglv/etalv/zlev'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') 'plev: assume p-levels (default), and ignore -ps option'
  write(6, '(a)') 'siglv: assume sigma-levels, etalv: assume eta-levels'
  write(6, '(a)') 'zlev: assume z-levels, psfc: assume surface data with plev'
  write(6, '(a)') ' '
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program main
