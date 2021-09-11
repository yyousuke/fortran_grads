!c
!c  program gtmage
!c  [history]
!c  2013/09/25 Yamashita: first ver.
!c  2016/12/18 Yamashita: vertical mean of tropopause grids (vlay option)
!c  2017/05/15 Yamashita: modify getparms & I/O 
!c  2017/05/15 Yamashita: use common_typedef & common_const
!c  2017/05/15 Yamashita: use common_mage
!c  2017/08/04 Yamashita: use setup_cnst
!c
!c  output mean age [year] from HF vmr for p-, sig-, and eta-levels
!c  with tropopause data
!c
!c  mean_age(x,y,z,t)=(tracer_EQ_tropopause(t) - tracer(x,y,z,t))/A
!c  A=d(tracer_EQ_surface)/dt = 1.d-12 (vmr/year)
!c
!c
!c  internal: module common_mage
!c            subroutine setup_cnst
!c  internal: subroutine getparms
!c    internal: subroutine xabort
!c  internal: subroutine calc_trop_gdtr
!c    external: module util: subroutine darea_ave
!c  external: module calculate: subroutine: shift
!c  external: module dcalculate: subroutine: dshift
!c  external: module dtave: subroutine dTIMEave
!c  internal: subroutine calc_mage
!c  external: module util
!c            subroutines get_axname, getyaxis, getzaxis, set_plevs
!c  external: module rwgtool
!c            subroutines gtopen, gtclose, gtskip, gtrewind, rgt, rgt_r4, 
!c                        rgthd, wgthdd, get_etacoef
!c  external: module  error_handler
!c            subroutines ioerror, werr
!c
!c======================================================================c

module common_mage
  use common_typedef, only: r8b
  use common_const, only: cnst_h, const_setup
  !c+++ [parameter]
  real(kind=r8b), save          :: h                !! scale height [m]
  real(kind=r8b), parameter     :: A = 1d-12        !! [vmr/year]

contains

!c----------------------------------------------------------------------c
  subroutine setup_cnst
    call const_setup
    h = cnst_h
  end subroutine setup_cnst
!c----------------------------------------------------------------------c

end module common_mage

!c=====================================================================c

program gtmage
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  use common_mage, only: setup_cnst, h
  use calculate, only: shift
  use dcalculate, only: dshift
  use dtave, only: dTIMEave
  use util, only: get_axname, getyaxis, getzaxis, set_plevs
  use rwgtool, only: gtopen, gtclose, gtskip, gtrewind, rgt, rgt_r4, rgthd, wgthdd, get_etacoef
  use error_handler, only: ioerror, werr
  implicit none
  !c+++ [input]
  !c+++ input data
  real(kind=r8b), allocatable :: ps(:,:)        !! Ps[hPa]
  real(kind=r8b), allocatable :: gdtr(:,:,:)    !! [vmr]
  real(kind=r4b), allocatable :: ptp(:,:)       !! tropopause pressure [hPa]
  real(kind=r8b), allocatable :: lat(:)         !! lat[deg]
  real(kind=r8b), allocatable :: p(:)           !! p[hPa] for p- and eta- lev., sig[] for sig-lev.
  real(kind=r8b), allocatable :: eta_fa(:)      !! for eta half lev.
  real(kind=r8b), allocatable :: eta_fb(:)      !! for eta half lev.
  !c+++ input from file (rgthd & rgt)
  character(len=ncc)          :: head(ndc)      !! gtool3 header
  character(len=ncc)          :: head2(ndc)     !! gtool3 header
  !c+++ input from gtool3 header
  integer(kind=i4b)           :: imax           !! x-, y-, z-axis sizes
  integer(kind=i4b)           :: jmax           !! x-, y-, z-axis sizes
  integer(kind=i4b)           :: kmax           !! x-, y-, z-axis sizes
  character(len=ncc)          :: haxisx         !! x-axis name
  character(len=ncc)          :: haxisy         !! y-axis name
  character(len=ncc)          :: haxisz         !! z-axis name
  real(kind=r8b)              :: rmiss          !! missing value
  !c+++ input from getparms
  integer(kind=i4b)           :: ista, iend     !! start/end record
  integer(kind=i4b)           :: nymin, nymax   !! min./max. limit of eqatorial average
  integer(kind=i4b)           :: ntr_s, ntr_e   !! range of time average
  integer(kind=i4b)           :: nvlay          !! number of vertical layers
  character(len=nfiln)        :: ifile          !! input file name
  character(len=nfiln)        :: itp            !! input tropopause pressure file name
  character(len=nfiln)        :: ips            !! input ps file name
  character(len=nfiln)        :: ip             !! input pressure file name
  character(len=nfiln)        :: ofile          !! output file name
  character(len=ncc)          :: hitem          !! item
  character(len=ncc*2)        :: htitl          !! title
  character(len=ncc)          :: hunit          !! unit
  character(len=ncc)          :: hdfmt          !! data format
  character(len=ncc)          :: zax            !! zaxis plev/siglv/etalv
  logical                     :: oapnd          !! t: append, f: replace
  !c+++ [output]
  !c+++ output data
  real(kind=r4b), allocatable :: mage(:,:,:)    !! mean age [year]
  !c+++ [internal work]
  integer(kind=i4b)           :: it, k          !!
  integer(kind=i4b)           :: ios            !! end code
  integer(kind=i4b)           :: nt, it_s, it_e !!
  character(len=ncc*2)        :: htitlz         !!
  real(kind=r4b), allocatable :: pres(:,:,:)    !! [hPa]
  real(kind=r4b), allocatable :: ps4(:,:)       !! Ps[hPa]
  real(kind=r8b), allocatable :: work(:)        !!
  !c+++ from calc_trop_gdtr
  real(kind=r8b), allocatable :: gdtr_eqtp(:)   !!
  !c+++ I/O unit number
  integer(kind=i4b)           :: jfile_i        !! for input file
  integer(kind=i4b)           :: jfile_tp       !! for input tropopause pressure file
  integer(kind=i4b)           :: jfile_ps       !! for input ps file
  integer(kind=i4b)           :: jfile_p        !! for input pressure file
  integer(kind=i4b)           :: jfile_o        !! for output file
  !c+++ [internal switch]
  logical                     :: osig = .false. !! enable sigma-lev.
  logical                     :: oeta = .false. !! enable eta-lev.
  logical                     :: ozlv = .false. !! enable z-lev.
  logical                     :: opin = .false. !! enable pressure input

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms
  if (zax == 'siglv') osig = .true.
  if (zax == 'etalv') oeta = .true.
  if (zax == 'zlev' ) ozlv = .true.
  if (trim(ip) /= 'NONE' ) opin = .true.
  if (ozlv.and. .not.opin) osig = .true. !! for Ps input

  !c+++ constants
  call setup_cnst

  !c+++ open input file
  call gtopen(trim(ifile), 'r', jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open input tropopause pressure file
  write(6, *) 'open input tropopause pressure file: ', trim(itp)
  call gtopen(trim(itp), 'r', jfile_tp, ios)
  if (ios /= 0) call ioerror(jfile_tp, ios)

  !c+++ open input ps file
  if (osig.or.oeta) then
    write(6, *) 'open input ps file: ', trim(ips)
    call gtopen(trim(ips), 'r', jfile_ps, ios)
    if (ios /= 0) call ioerror(jfile_ps, ios)
  endif

  !c+++ open input pressure file
  if (opin) then
    write(6, *) 'open input pressure file: ', trim(ip)
    call gtopen(trim(ip), 'r', jfile_p, ios)
    if (ios /= 0) call ioerror(jfile_p, ios)
  endif

  !c+++ open output file
  write(6, *) 'open output file: ', trim(ofile)
  if (oapnd) then
    call gtopen(trim(ofile), 'a', jfile_o, ios)
  else
    call gtopen(trim(ofile), 'w', jfile_o, ios)
  endif
  if (ios /= 0) call ioerror(jfile_o, ios)

  !c+++ read header & set axis-sizes, missing value
  call rgthd(jfile_i, head, imax, jmax, kmax, rmiss, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)
  call gtrewind(jfile_i, ios)
  write(6, *) 'imax, jmax, kmax = ', imax, jmax, kmax
  write(6, *) 'rmiss = ', rmiss
  !c+++ x- y- z-axis names from gtool header
  call get_axname(haxisx, haxisy, haxisz, head)
  write(6, *) 'haxisx = ', haxisx
  write(6, *) 'haxisy = ', haxisy
  write(6, *) 'haxisz = ', haxisz
  !c+++ set data format of output data
  if (hdfmt == 'NULL') hdfmt = head(38)

  !c+++ allocate
  allocate(ps(imax,jmax), lat(jmax), p(kmax))
  allocate(gdtr(imax,jmax,kmax), ptp(imax,jmax), gdtr_eqtp(iend), work(iend))
  allocate(pres(imax,jmax,kmax), ps4(imax,jmax), mage(imax,jmax,kmax))
  allocate(eta_fa(kmax), eta_fb(kmax))

  !c+++ read y-axis file
  call getyaxis(jmax, haxisy, lat)

  !c+++ z-coefs.
  if (.not. oeta) then
    !c+++ read z-axis file (for p-lev & sig-lev)
    call getzaxis(kmax, haxisz, p)
    !c+++ z ==> sig (for z-lev)
    if (ozlv) then
      !c+++ z = - h log(sig), -z/h = log(sig), sig = exp(-z/h)
      do k = 1, kmax
        p(k) = exp(- p(k) / h)
      enddo !! j
    endif
  else
    !c+++ read z-axis file (for eta-lev)
    call get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  endif

!c
!c calculation of tropopause tracer mixing ratio
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      call gtskip(jfile_tp, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_tp, ios)
      if (osig.or.oeta) then
        call gtskip(jfile_ps, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_ps, ios)
      endif
      if (opin) then
        call gtskip(jfile_p, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_p, ios)
      endif
      cycle
    endif

    !c+++ read gdtr
    call rgt(jfile_i, imax, jmax, kmax, head, gdtr, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ read tropopause pressure
    call rgt_r4(jfile_tp, imax, jmax, 1, head2, ptp, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_tp, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
    !c+++ Pa ==> hPa 
    if (trim(head2(16)) == 'Pa') then
      call shift(imax, jmax, 1, rmiss, ptp, ptp, 0.01d0, 0.d0)
      write(6, '(a, f9.4, a)') 'ptp(1,1) = ', ptp(1,1), ' (hPa)'
    endif

    !c+++ read input Ps data
    if (osig.or.oeta) then
      call rgt(jfile_ps, imax, jmax, 1, head2, ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, 1, rmiss, ps, ps, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps(1,1), ' (hPa)'
      endif
      ps4(1:imax,1:jmax) = real(ps(1:imax,1:jmax))
    endif

    !c+++ set pres
    if (opin) then
      !c+++ read input pressure data
      call rgt_r4(jfile_p, imax, jmax, kmax, head2, pres, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_p, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa 
      if (trim(head2(16)) == 'Pa') then
        call shift(imax, jmax, kmax, rmiss, pres, pres, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'pres(1,1,1) = ', pres(1,1,1), ' (hPa)'
      endif
    else
      !c+++ set pres by set_plevs
      call set_plevs(imax, jmax, kmax, rmiss, p, ps4, eta_fa, eta_fb, pres, osig, oeta)
    endif

    !c+++ GDTR, ptp ==> gdtr_eqtp
    call calc_trop_gdtr(imax, jmax, kmax, nymin, nymax, nvlay, rmiss, gdtr, ptp, pres, lat, gdtr_eqtp(it))

    if (it == iend) exit
  enddo
  !c+++ rewind
  if (it < ista) then
    call gtrewind(jfile_i, ios)
    call gtrewind(jfile_tp, ios)
    if (osig.or.oeta) call gtrewind(jfile_ps, ios)
    if (opin) call gtrewind(jfile_p, ios)
  endif
  nt = it

  !c+++ average
  work(1:nt) = rmiss
  do it = max(ista, 1), nt
    it_s = max(it + ntr_s, 1)
    it_e = min(it + ntr_e, nt)
    call dTIMEave(1, 1, 1, it_e-it_s+1, rmiss, gdtr_eqtp(it_s:it_e), work(it))
  enddo !! it
  gdtr_eqtp(1:nt) = work(1:nt)

!c
!c mean age calculation
!c===
  it = 0
  do while (1 == 1)
    it = it + 1
    !c+++ skip
    if (it < ista) then
      call gtskip(jfile_i, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_i, ios)
      call gtskip(jfile_tp, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_tp, ios)
      if (osig.or.oeta) then
        call gtskip(jfile_ps, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_ps, ios)
      endif
      if (opin) then
        call gtskip(jfile_p, ios)
        if (ios == -1) exit !! reach EOF
        if (ios /= 0) call ioerror(jfile_p, ios)
      endif
      cycle
    endif

    !c+++ read gdtr
    call rgt(jfile_i, imax, jmax, kmax, head, gdtr, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_i, ios)

    !c+++ read tropopause pressure
    call rgt_r4(jfile_tp, imax, jmax, 1, head2, ptp, ios)
    if (ios == -1) exit !! reach EOF
    if (ios /= 0) call ioerror(jfile_tp, ios)
    if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
    !c+++ Pa ==> hPa 
    if (trim(head2(16)) == 'Pa') then
      call shift(imax, jmax, 1, rmiss, ptp, ptp, 0.01d0, 0.d0)
      write(6, '(a, f9.4, a)') 'ptp(1,1) = ', ptp(1,1), ' (hPa)'
    endif

    !c+++ read input Ps data
    if (osig.or.oeta) then
      call rgt(jfile_ps, imax, jmax, 1, head2, ps, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_ps, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa
      if (trim(head2(16)) == 'Pa') then
        call dshift(imax, jmax, 1, rmiss, ps, ps, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'ps(1,1) = ', ps(1,1), ' (hPa)'
      endif
      ps4(1:imax,1:jmax) = real(ps(1:imax,1:jmax))
    endif

    !c+++ set pres
    if (opin) then
      !c+++ read input pressure data
      call rgt_r4(jfile_p, imax, jmax, kmax, head2, pres, ios)
      if (ios == -1) exit !! reach EOF
      if (ios /= 0) call ioerror(jfile_p, ios)
      if (head(27) /= head2(27)) call werr(head(27), head2(27), 'Dates are not match!')
      !c+++ Pa ==> hPa 
      if (trim(head2(16)) == 'Pa') then
        call shift(imax, jmax, kmax, rmiss, pres, pres, 0.01d0, 0.d0)
        write(6, '(a, f9.4, a)') 'pres(1,1,1) = ', pres(1,1,1), ' (hPa)'
      endif
    else
      !c+++ set pres by set_plevs
      call set_plevs(imax, jmax, kmax, rmiss, p, ps4, eta_fa, eta_fb, pres, osig, oeta)
    endif

    !c+++ GDTR, ptp ==> mean_age
    call calc_mage(imax, jmax, kmax, rmiss, gdtr, ptp, pres, gdtr_eqtp(it), mage)

    !c+++ set gtool header
    head(3) = hitem
    htitlz = htitl
    head(14) = htitlz(1:16)
    head(15) = htitlz(17:32)
    head(16) = hunit

    !c+++ write mean age
    call wgthdd(jfile_o, imax, jmax, kmax, head, hdfmt, mage, ios)
    if (ios /= 0) call ioerror(jfile_o, ios)

    if (it == iend) exit
  enddo

  !c+++ close files
  call gtclose(jfile_i, ios)
  call gtclose(jfile_tp, ios)
  call gtclose(jfile_o, ios)
  if (osig.or.oeta) call gtclose(jfile_ps, ios)
  if (opin) call gtclose(jfile_p, ios)
  !c+++ deallocate
  deallocate(Ps, lat, p, gdtr, ptp, gdtr_eqtp, work, pres, ps4, mage)
  deallocate(eta_fa, eta_fb)
  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine calc_trop_gdtr
!c  calculate tropopause tracer mixing ratio (vmr)
!c=====
subroutine calc_trop_gdtr(imax, jmax, kmax, nymin, nymax, nvlay, rmiss, gdtr, ptp, pres, lat, gdtr_eqtp)
  use util, only: darea_ave
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-grid sizes
  integer(kind=i4b), intent(in) :: nymin, nymax         !! min./max. limit of average
  integer(kind=i4b), intent(in) :: nvlay                !! number of vertical layers
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r8b), intent(in)    :: gdtr(imax,jmax,kmax) !! tracer mixing ratio (vmr)
  real(kind=r4b), intent(in)    :: ptp(imax,jmax)       !! tropopause pressure (hPa)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! 3-D pressure (hPa)
  real(kind=r8b), intent(in)    :: lat(jmax)            !! lat (deg)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: gdtr_eqtp            !! EQ tropopause tracer (vmr)
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k
  integer(kind=i4b)             :: k_tp, isum
  !ccc real(kind=r8b) :: p(imax,jmax,kmax) !! (Pa)
  !c+++ for area_ave
  real(kind=r8b)                :: gdtr_tp(imax,jmax)   !! tropopause tracer (vmr)

  !ccc p(1:imax,1:jmax,1:kmax) = pres(1:imax,1:jmax,1:kmax) * 100.d0 !! hPa ==> Pa

  !c+++ tropopause & surface
  do j = 1, jmax
    do i = 1, imax
      k_tp = 0
      do k = 1, kmax
        if (pres(i,j,k) <= ptp(i,j)) then
          k_tp = k
          exit
        endif
      enddo !! k
      if (k_tp /= 0) then
        if (nvlay == 0) then
          gdtr_tp(i,j) = gdtr(i,j,k_tp)
        else if (nvlay >= 1) then
          gdtr_tp(i,j) = 0.d0
          do k = -nvlay, nvlay
            if (k_tp-nvlay <= 0.or.k_tp+nvlay >= kmax) cycle
            if (gdtr(i,j,k_tp+k) /= rmiss) then
              gdtr_tp(i,j) = gdtr_tp(i,j) + gdtr(i,j,k_tp+k)
              isum = isum + 1
            endif
          enddo !! k
          if (isum /= 0) then
            gdtr_tp(i,j) = gdtr_tp(i,j) / dble(isum)
          else
            gdtr_tp(i,j) = rmiss
          endif
        else
          gdtr_tp(i,j) = rmiss
        endif
      else
        gdtr_tp(i,j) = rmiss
      endif
    enddo !! i
  enddo !! j

  !c+++ area average
  call darea_ave(imax, jmax, 1, imax, nymin, nymax, rmiss, &
&   gdtr_tp, lat, gdtr_eqtp, .true.)

  return
end subroutine calc_trop_gdtr

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine calc_mage
!c  calculate mean age
!c=====
subroutine calc_mage(imax, jmax, kmax, rmiss, gdtr, ptp, pres, gdtr_eqtp, mage)
  use common_mage, only: A
  !c+++ [input]
  integer(kind=i4b), intent(in) :: imax, jmax, kmax     !! x-, y-, z-grid sizes
  real(kind=r8b), intent(in)    :: rmiss                !! missing value
  real(kind=r8b), intent(in)    :: gdtr(imax,jmax,kmax) !! tracer mixing ratio (vmr)
  real(kind=r4b), intent(in)    :: ptp(imax,jmax)       !! tropopause pressure (hPa)
  real(kind=r4b), intent(in)    :: pres(imax,jmax,kmax) !! 3-D pressure (hPa)
  real(kind=r8b), intent(in)    :: gdtr_eqtp            !! EQ tropopause tracer (vmr)
  !c+++ [output]
  real(kind=r4b), intent(out)   :: mage(imax,jmax,kmax) !! mean age (year)
  !c+++ [internal work]
  integer(kind=i4b) :: i, j, k
  if (gdtr_eqtp == rmiss) then
    write(6, *) 'Warn: gdtr_eqtp is missing!'
    mage(1:imax,1:jmax,1:kmax) = rmiss
    return
  endif

  !c+++  mean_age(x,y,z,t)=(tracer_EQ_tropopause(t) - tracer(x,y,z,t))/A
  !c+++  A=d(tracer_EQ_surface)/dt = 1.d-12 (vmr/year)
  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (pres(i,j,k) <= ptp(i,j).and.gdtr(i,j,k) /= rmiss) then
          mage(i,j,k) = (gdtr_eqtp - gdtr(i,j,k)) / A
        else
          mage(i,j,k) = rmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine calc_mage

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
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('tp', 'ptp', itp, ios)
  call get_parms('ps', 'Ps', ips, ios)
  call get_parms('p', 'NONE', ip, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  !c+++ 
  call get_parms('item', 'MAGE', hitem, ios)
  call get_parms('titl', 'mean age', htitl, ios)
  call get_parms('unit', 'year', hunit, ios)
  !c+++ start/end time
  call get_strend(ista, iend, ios)
  !c+++ latitude range
  call get_parms('j1', '32', hval, ios) !! min. lat. (1.4N)
  call c2var(nymin, '(i16)', hval)
  call get_parms('j2', '33', hval, ios) !! max. lat. (1.4S)
  call c2var(nymax, '(i16)', hval)
  !ccc nymin = 25           !! min. lat. (21.1N)
  !ccc nymax = 40           !! max. lat. (21.1S)
  !c+++ time range
  call get_parms('t1', '-6', hval, ios) !! min. lim. of time
  call c2var(ntr_s, '(i16)', hval)
  call get_parms('t2', '-6', hval, ios) !! max. lim. of time
  call c2var(ntr_e, '(i16)', hval)
  !c+++ number of vertical layers
  call get_parms('vlay', '0', hval, ios)
  call c2var(nvlay, '(i16)', hval)
  !c+++ data format
  call get_parms('dfmt', 'NULL', hdfmt, ios)
  !c+++ apnd
  call get_parms('apnd', 'f', hval, ios)
  call c2var(oapnd, '(l1)', hval)
  !c+++ axis
  call get_parms('zax', 'plev', zax, ios)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine xabort
!c=====
subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtmage -o output-age-file'
  write(6, '(a)') '-i input-tracer-vmr-file []'
  write(6, '(a)') '-tp input-tropopause-pressure-file [hPa or Pa]'
  write(6, '(a)') ' '
  write(6, '(a)') '-j1 min-lat -j2 max-lat (range of latitude ave. for EQ tracer)'
  write(6, '(a)') '-t1 min-limit -t2 max-limit (range of time ave. for EQ tracer)'
  write(6, '(a)') '-vlay 0 (vartical layers of EQ tracer, 0: 1 layer,  1: 3 layers)'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  write(6, '(a)') '-apnd f/t (default: f)'
  write(6, '(a)') '-dfmt UR4/UR8 (default: same as input data)'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-p input-p-file (pressure [hPa or Pa], optional)'
  write(6, '(a)') '  (default, NONE)'
  write(6, '(a)') '-ps input-ps-file (surface pressure [hPa or Pa], optional)'
  write(6, '(a)') '  (default, Ps)'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') '-zax plev/zlev/siglv/etalv'
  write(6, '(a)') 'plev: assume p-levels (default), and ignore -ps option'
  write(6, '(a)') 'zlev: assume z-levels'
  write(6, '(a)') '(either -p input-p-file or -ps input-ps-file)'
  write(6, '(a)') 'siglv: assume sigma-levels, etalv: assume eta-levels'
  write(6, '(a)') '(-ps input-ps-file) for siglv/etalv'
  write(6, '(a)') '------------------------------------------'
  write(6, '(a)') ' '
  write(6, '(a)') 'ex. p-levels: '
  write(6, '(a)') '    gtmage -i qtracer_P -tp ptp -o mean_age'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtmage
