!c
!c  module rwgtool
!c  [history]
!c  2007/05/10 Yamashita: f90 ver. of subroutine wgtool from F77 ver. (by 1994/06/04 Numaguti)
!c  2011/10/17 Yamashita: module wgtool, and rename subtroutine wgtool to wgt
!c  2011/10/18 Yamashita: add subroutine rgt, and rename module wgtool to rwgtool
!c  2013/01/10 Yamashita: use getdat and gettim in wgt
!c  2013/03/09 Yamashita: gtr, gtw with UR4/UR8
!c  2013/03/09 Yamashita: add rgthd, rgtd with UR4/UR8
!c  2013/03/10 Yamashita: add seek_iounit: seek unused I/O unit
!c  2013/03/10 Yamashita: add wmsg: write diagnostic message
!c  2013/03/30 Yamashita: modified for gtepflux.f90
!c  2013/03/31 Yamashita: add get_hdtime: get time from gtool3 header
!c  2013/07/17 Yamashita: add rgth: read gtool3 head & skip data
!c  2013/10/11 Yamashita: add gtopen: open gtool3 file
!c  2013/10/11 Yamashita: add gtclose: close gtool3 file
!c  2013/10/11 Yamashita: add gtrewind: rewind gtool3 file
!c  2014/05/28 Yamashita: add array size check of rgt
!c  2014/08/26 Yamashita: add get_hdtime: get start/end time from gtool3 head
!c  2016/11/21 Yamashita: add mch_rmchar: used for hdfmt in rgtd
!c  2016/11/24 Yamashita: add gtbackspace: backspace of gtool3 file
!c  2016/11/29 Yamashita: add lmrange_r8: max./min. limit of data in rgt
!c  2016/11/29 Yamashita: add rmeps_r8: reset missing data in rgt
!c  2016/12/06 Yamashita: modify wmsg
!c  2017/04/18 Yamashita: add lmrange_r4: max./min. limit of data in rgt_r4
!c  2017/04/18 Yamashita: add rmeps_r4: reset missing data in rgt_r4
!c  2017/04/18 Yamashita: rgt to rgt_r4, rgt
!c  2017/04/18 Yamashita: wgt to wgt_r4, wgt
!c  2017/04/18 Yamashita: add interface (rgtd)
!c  2017/04/18 Yamashita: add set_gthd: set new gtool header
!c  2017/04/27 Yamashita: add gtskip, wgthd
!c  2017/04/27 Yamashita: add wgthdd_r4, wgthdd_r8, wgtd_r4, wgtd_r8
!c  2017/04/18 Yamashita: add interfaces (wgthdd, wgtd)
!c  2017/05/02 Yamashita: use common_typedef
!c  2017/05/15 Yamashita: add get_zaxsize: get z-axis size
!c  2017/05/15 Yamashita: add get_axis: get axis points from axis information file
!c  2017/05/15 Yamashita: add get_etacoef: get eta_fa and eta_fb from eta-axis information file
!c  2018/02/15 Yamashita: add function rmc: similar to mch_rmchar
!c
!c=====================================================================c
module rwgtool
  use common_args, only: ncc, ndc, nfiln
  use common_typedef, only: i2b, i4b, r4b, r8b
  use ysys
  implicit none
  private
  public :: gtopen, gtclose, gtrewind, gtbackspace, gtskip
  public :: rgt, rgthd, rgtd, rgth, wgt, wgthd, wgthdd, wgtd
  public :: rgt_r4, rgtd_r4, rgtd_r8
  public :: wgt_r4, wgtd_r4, wgtd_r8, wgthdd_r4, wgthdd_r8
  public :: set_gthd, get_hdtime, get_hdtime2, get_hdall
  public :: get_zaxsize, get_axis, get_etacoef
  public :: seek_iounit, wmsg

!c======================================================================c

!c---------------------------------------------------------------------c
!c  interface rgthd
!c  read gtool header
!c=====
interface rgthd
  module procedure &
&   rgthd_r4, rgthd_r8
end interface rgthd

!c---------------------------------------------------------------------c
!c  interface rgtd
!c  read gtool data (data component)
!c=====
interface rgtd
  module procedure &
&   rgtd_r4, rgtd_r8
end interface rgtd

!c---------------------------------------------------------------------c
!c  interface wgthdd
!c  read gtool3 data
!c=====
interface wgthdd
  module procedure &
&   wgthdd_r4, wgthdd_r8
end interface wgthdd

!c---------------------------------------------------------------------c
!c  interface wgtd
!c  read gtool3 data
!c=====
interface wgtd
  module procedure &
&   wgtd_r4, wgtd_r8
end interface wgtd

!c---------------------------------------------------------------------c
!c  interface get_axis
!c  get axis points from axis information file
!c=====
interface get_axis
  module procedure &
&   get_axis_r4, get_axis_r8
end interface get_axis

!c======================================================================c

  !c+++ [common]
  real(kind=r8b), parameter :: vmin = -1d34 !! min. limit of data
  real(kind=r8b), parameter :: vmax = 1d34  !! max. limit of data
  real(kind=r8b), parameter :: eps = 1d-10  !! epsiron

  contains 

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine gtopen
!c  open gtool3 file
!c  mode: r: read, w: write, a: append (with native endian)
!c        rb: read big-endian, wb: write big-endian, ab: append big-endian
!c        rl: read little-endian, wl: write little-endian, al: append little-endian
!c        ra: read text-data, wa: write text-data, aa: append text-data
!c=====
subroutine gtopen(file_name, mode_i, jfile, ios)
  !c+++ [input]
  character(len=*), intent(in)   :: file_name !! file name
  character(len=*), intent(in)   :: mode_i    !! open mode
  !c+++ [output]
  integer(kind=i4b), intent(out) :: jfile     !! I/O unit
  integer(kind=i4b), intent(out) :: ios       !! end code
  !c+++ [internal work]
  logical                        :: oexist    !!
  character(len=ncc)             :: mode      !! r: read, w: write, a: append

  mode = mode_i
  !c+++ get I/O unit numbers for input file
  call seek_iounit(jfile, ios)
  if (ios /= 0) then
    write(6, '(a)') 'Error: in seek_iounit'
    return
  endif

  if (mode(1:1) == 'r') then
    !c+++ check file
    inquire(file=trim(file_name), iostat=ios, exist=oexist)
    if (.not. oexist) then
      write(6, *) 'Error: not found '//trim(file_name)
      ios = 101
      return
    endif
    !c+++ open for read
    if (mode(2:2) == 'b') then
      !c+++ big endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
&       status='old', action='read', convert='big_endian', iostat=ios)
    else if (mode(2:2) == 'l') then
      !c+++ little endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
&       status='old', action='read', convert='little_endian', iostat=ios)
    else if (mode(2:2) == 'a') then
      !c+++ ASCII
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
&       status='old', action='read', iostat=ios)
    else
      !c+++ native endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
&       status='old', action='read', iostat=ios)
    endif
  else if (mode(1:1) == 'w') then
    !c+++ open for write
    if (mode(2:2) == 'b') then
      !c+++ big endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
&       status='unknown', action='write', convert='big_endian', iostat=ios)
    else if (mode(2:2) == 'l') then
      !c+++ little endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
&       status='unknown', action='write', convert='little_endian', iostat=ios)
    else if (mode(2:2) == 'a') then
      !c+++ ASCII
      open(jfile, file=trim(file_name), form='formatted', access='sequential', &
&       status='unknown', action='write', iostat=ios)
    else
      !c+++ native endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
&       status='unknown', action='write', iostat=ios)
    endif
  else if (mode(1:1) == 'a') then
    !c+++ open for write with append
    if (mode(2:2) == 'b') then
      !c+++ big endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
  &     position='append', status='unknown', action='write', convert='big_endian', iostat=ios)
    else if (mode(2:2) == 'l') then
      !c+++ little endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
  &     position='append', status='unknown', action='write', convert='little_endian', iostat=ios)
    else if (mode(2:2) == 'a') then
      !c+++ ASCII
      open(jfile, file=trim(file_name), form='formatted', access='sequential', &
&       position='append', status='unknown', action='write', iostat=ios)
    else
      !c+++ native endian
      open(jfile, file=trim(file_name), form='unformatted', access='sequential', &
  &     position='append', status='unknown', action='write', iostat=ios)
    endif
  else
    write(6, *) 'Error: invalid input mode = ', mode
    stop 2
  endif
  if (ios == 0) write(6, '(3a, i3)') 'opend: ', trim(file_name), ', unit = ', jfile

  return
end subroutine gtopen

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine gtclose
!c  close gtool3 file
!c=====
subroutine gtclose(jfile, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile !! I/O unit
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios   !! end code
  !c+++ [internal work]
  logical                        :: opend !!

  !c+++ check
  inquire(unit=jfile, opened=opend)
  !c+++ close
  if (opend) then
    close(jfile, iostat=ios)
    write(6, '(a, i3)') 'closed: unit = ', jfile
    if (ios < 0) ios = 0
  else
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 131
  endif

  return
end subroutine gtclose

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine gtrewind
!c  rewind gtool3 file
!c=====
subroutine gtrewind(jfile, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile !! I/O unit
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios   !! end code
  !c+++ [internal work]
  logical                        :: opend !!

  !c+++ check
  inquire(unit=jfile, opened=opend)
  !c+++ rewind
  if (opend) then
    rewind(jfile, iostat=ios)
  else
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 131
  endif

  return
end subroutine gtrewind

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine gtbackspace
!c  backspace of gtool3 file
!c=====
subroutine gtbackspace(jfile, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile !! I/O unit
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios   !! end code
  !c+++ [internal work]
  logical                        :: opend !!

  !c+++ check
  inquire(unit=jfile, opened=opend)
  !c+++ backspace
  if (opend) then
    backspace(jfile, iostat=ios)
    backspace(jfile, iostat=ios)
  else
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 131
  endif

  return
end subroutine gtbackspace

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine gtskip
!c  skip 1 record of gtool3 file
!c=====
subroutine gtskip(jfile, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile !! I/O unit
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios   !! end code
  !c+++ [internal work]
  logical                        :: opend !!

  !c+++ check
  inquire(unit=jfile, opened=opend)
  !c+++ skip
  if (opend) then
    read(jfile, iostat=ios)
    if (ios /= 0) return
    read(jfile, iostat=ios)
  else
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 131
  endif

  return
end subroutine gtskip

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgt_r4
!c  read gtool3 head & data
!c
!c=====
subroutine rgt_r4(jfile, imax, jmax, kmax, head, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile             !! I/O unit
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax  !! size
  !c+++ [output]
  character(len=*), intent(out)  :: head(ndc)         !! header
  real(kind=r4b), intent(out)    :: d(imax,jmax,kmax) !! data
  integer(kind=i4b), intent(out) :: ios               !! end code
  !c+++ [internal work]
  real(kind=r4b), allocatable    :: din(:,:,:)        !! data for read
  integer(kind=i4b)              :: nx, ny, nz        !! size
  real(kind=r4b)                 :: vmiss             !! missing value

  !c+++ read header
  call rgthd_r4(jfile, head, nx, ny, nz, vmiss, ios)
  if (ios /= 0) return
  !c+++ write message
  call wmsg(head, ' READ')
  !c+++ allocate
  allocate (din(nx,ny,nz))
  d(1:imax,1:jmax,1:kmax) = vmiss
  !c+++ read data
  call rgtd_r4(jfile, nx, ny, nz, head, din, ios)
  call rmeps_r4(nx, ny, nz, vmiss, din)
  call lmrange_r4(nx, ny, nz, vmiss, din)
  d(1:min(imax,nx),1:min(jmax,ny),1:min(kmax,nz)) = din(1:min(imax,nx),1:min(jmax,ny),1:min(kmax,nz))
  deallocate (din)

  return
end subroutine rgt_r4

!c---------------------------------------------------------------------c
 
!c---------------------------------------------------------------------c
!c  subroutine rgt
!c  read gtool3 head & data
!c
!c=====
subroutine rgt(jfile, imax, jmax, kmax, head, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile             !! I/O unit
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax  !! size
  !c+++ [output]
  character(len=*), intent(out)  :: head(ndc)         !! header
  real(kind=r8b), intent(out)    :: d(imax,jmax,kmax) !! data
  integer(kind=i4b), intent(out) :: ios               !! end code
  !c+++ [internal work]
  real(kind=r8b), allocatable    :: din(:,:,:)        !! data for read
  integer(kind=i4b)              :: nx, ny, nz        !! size
  real(kind=r8b)                 :: vmiss             !! missing value

  !c+++ read header
  call rgthd_r8(jfile, head, nx, ny, nz, vmiss, ios)
  if (ios /= 0) return
  !c+++ write message
  call wmsg(head, ' READ')
  !c+++ allocate
  allocate (din(nx,ny,nz))
  d(1:imax,1:jmax,1:kmax) = vmiss
  !c+++ read data
  call rgtd_r8(jfile, nx, ny, nz, head, din, ios)
  call rmeps_r8(nx, ny, nz, vmiss, din)
  call lmrange_r8(nx, ny, nz, vmiss, din)
  d(1:min(imax,nx),1:min(jmax,ny),1:min(kmax,nz)) = din(1:min(imax,nx),1:min(jmax,ny),1:min(kmax,nz))
  deallocate (din)

  return
end subroutine rgt

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgth
!c  read gtool3 head & skip data
!c
!c=====
subroutine rgth(jfile, head, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile       !! I/O unit
  !c+++ [output]
  character(len=*), intent(out)  :: head(ndc)   !! header
  integer(kind=i4b), intent(out) :: ios         !! end code
  !c+++ [internal work]
  integer(kind=i4b)              :: nx, ny, nz  !! x-, y-, z-sizes
  real(kind=r8b)                 :: vmiss       !! missing value

  !c+++ read header
  call rgthd_r8(jfile, head, nx, ny, nz, vmiss, ios)
  if (ios /= 0) return
  !c+++ write message
  call wmsg(head, ' READ')
  !c+++ skip data
  read(jfile, iostat=ios) 

  return
end subroutine rgth

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine wgt_r4
!c  write gtool3 header & data (create new header)
!c=====
subroutine wgt_r4(jfile, imax, jmax, kmax, vmiss, d, hitem, htitl, hunit, &
& hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, haxisz, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile             !! 出力ファイル番号
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax  !! x, y, z軸のサイズ
  real(kind=r8b), intent(in)     :: vmiss             !! 欠損値
  real(kind=r4b), intent(in)     :: d(imax,jmax,kmax) !! data
  character(len=*), intent(in)   :: hitem             !! 変数名
  character(len=*), intent(in)   :: htitl             !! タイトル 
  character(len=*), intent(in)   :: hunit             !! 単位
  character(len=*), intent(in)   :: hdset             !! データセット名
  character(len=*), intent(in)   :: hdfmt             !! データフォーマット
  real(kind=r8b), intent(in)     :: time              !! 時刻
  real(kind=r8b), intent(in)     :: tdur              !! 代表時間
  character(len=*), intent(in)   :: htunit            !! 時刻単位
  integer(kind=i4b), intent(in)  :: jdate(6)          !! 日付
  character(len=*), intent(in)   :: haxisx            !! x軸名称
  character(len=*), intent(in)   :: haxisy            !! y軸名称
  character(len=*), intent(in)   :: haxisz            !! z軸名称
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios               !! end code
  !c+++ [internal work]
  character(len=ncc)             :: head(ndc)         !! head
  logical                        :: opend             !!

  !c+++ check
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error in wgt_r8: not opend, unit = ', jfile
    ios = 121
    return
  endif

  !c+++ set header
  call set_gthd(imax, jmax, kmax, vmiss, hitem, htitl, hunit, &
& hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, haxisz, head)

  !c++ write header
  write(jfile, iostat=ios) head
  if (ios /= 0) then
    write(6, '(a, i2)') 'Error in wgt_r8: write gtool3 header, unit = ', jfile
    return
  endif

  !c+++ write message
  call wmsg(head, 'WRITE')

  !c++ write output data
  call wgtd_r4(jfile, imax, jmax, kmax, head, d, ios)

  return
end subroutine wgt_r4

!c---------------------------------------------------------------------c
 
!c----------------------------------------------------------------------c
!c  subroutine wgt
!c  write gtool3 header & data (create new header)
!c=====
subroutine wgt(jfile, imax, jmax, kmax, vmiss, d, hitem, htitl, hunit, &
& hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, haxisz, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile             !! 出力ファイル番号
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax  !! x, y, z軸のサイズ
  real(kind=r8b), intent(in)     :: vmiss             !! 欠損値
  real(kind=r8b), intent(in)     :: d(imax,jmax,kmax) !! data
  character(len=*), intent(in)   :: hitem             !! 変数名
  character(len=*), intent(in)   :: htitl             !! タイトル 
  character(len=*), intent(in)   :: hunit             !! 単位
  character(len=*), intent(in)   :: hdset             !! データセット名
  character(len=*), intent(in)   :: hdfmt             !! データフォーマット
  real(kind=r8b), intent(in)     :: time              !! 時刻
  real(kind=r8b), intent(in)     :: tdur              !! 代表時間
  character(len=*), intent(in)   :: htunit            !! 時刻単位
  integer(kind=i4b), intent(in)  :: jdate(6)          !! 日付
  character(len=*), intent(in)   :: haxisx            !! x軸名称
  character(len=*), intent(in)   :: haxisy            !! y軸名称
  character(len=*), intent(in)   :: haxisz            !! z軸名称
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios               !! end code
  !c+++ [internal work]
  character(len=ncc)             :: head(ndc)         !! head
  logical                        :: opend             !!

  !c+++ check
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error in wgt_r8: not opend, unit = ', jfile
    ios = 121
    return
  endif

  !c+++ set header
  call set_gthd(imax, jmax, kmax, vmiss, hitem, htitl, hunit, &
& hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, haxisz, head)

  !c++ write header
  write(jfile, iostat=ios) head
  if (ios /= 0) then
    write(6, '(a, i2)') 'Error in wgt_r8: write gtool3 header, unit = ', jfile
    return
  endif

  !c+++ write message
  call wmsg(head, 'WRITE')

  !c++ write output data
  call wgtd_r8(jfile, imax, jmax, kmax, head, d, ios)

  return
end subroutine wgt

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wgthdd_r4
!c  write gtool3 header & data with modification (simple)
!c=====
subroutine wgthdd_r4(jfile, imax, jmax, kmax, head, hdfmt, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile              !! I/O unit
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax   !! x, y, z-sizes
  character(len=*), intent(in)   :: head(ndc)          !! header
  character(len=*), intent(in)   :: hdfmt              !! UR4/UR8
  real(kind=r4b), intent(in)     :: d(imax,jmax,kmax)  !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios                !! end code
  !c+++ [internal work]
  character(len=ncc)             :: ohead(ndc)         !! output header

  ohead(1:ndc) = head(1:ndc)
  ohead(38) = hdfmt

  !c+++ write gtool3 header
  call wgthd(jfile, ohead, ios)
  if (ios /= 0) return
  !c+++ write gtool3 data
  call wgtd_r4(jfile, imax, jmax, kmax, ohead, d, ios)

  return
end subroutine wgthdd_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wgthdd_r8
!c  write gtool3 header & data with modification (simple)
!c=====
subroutine wgthdd_r8(jfile, imax, jmax, kmax, head, hdfmt, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile             !! I/O unit
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax  !! x, y, z-sizes
  character(len=*), intent(in)   :: head(ndc)         !! header
  character(len=*), intent(in)   :: hdfmt             !! UR4/UR8
  real(kind=r8b), intent(in)     :: d(imax,jmax,kmax) !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios               !! end code
  !c+++ [internal work]
  character(len=ncc)             :: ohead(ndc)        !! output header

  ohead(1:ndc) = head(1:ndc)
  ohead(38) = hdfmt

  !c+++ write gtool3 header
  call wgthd(jfile, ohead, ios)
  if (ios /= 0) return
  !c+++ write gtool3 data
  call wgtd_r8(jfile, imax, jmax, kmax, ohead, d, ios)

  return
end subroutine wgthdd_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wgthd
!c  write gtool3 header (simple)
!c=====
subroutine wgthd(jfile, head, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile      !! I/O unit
  character(len=*), intent(in)   :: head(ndc)  !! header
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios        !! end code
  !c+++ [internal work]
  integer(kind=i2b)              :: jdates(7)  !! input from getdat, gettim
  integer(kind=i4b)              :: jdaten(6)  !!
  character(len=ncc)             :: huser      !!
  logical                        :: opend      !!
  character(len=ncc)             :: ohead(ndc) !! output header

  ohead(1:ndc) = head(1:ndc)

  !c+++ check
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error in wgthd: not opend, unit = ', jfile
    ios = 121
    return
  endif

  !c+++ set modify time & user
  call getdat(jdates(1), jdates(2), jdates(3))
  call gettim(jdates(4), jdates(5), jdates(6), jdates(7))
  jdaten(1:6) = jdates(1:6)

  call getenv('USER', huser)

  !c+++ CDATE
  !ccc write(ohead(60), '(i4.4, 2i2.2, 1x, 3i2.2)') jdaten(1:6)
  !ccc ohead(61) = huser
  !c+++ MDATE
  write(ohead(62), '(i4.4, 2i2.2, 1x, 3i2.2)') jdaten(1:6)
  ohead(63) = huser

  !c+++ write
  write(jfile, iostat=ios) ohead
  !c+++ write message
  if (ios == 0) call wmsg(head, 'WRITE')

  return
end subroutine wgthd

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wgtd_r4
!c  read gtool3 data
!c=====
subroutine wgtd_r4(jfile, imax, jmax, kmax, head, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile             !! I/O unit
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax  !! x, y, z- sizes
  character(len=*), intent(in)   :: head(ndc)         !! header
  real(kind=r4b), intent(in)     :: d(imax,jmax,kmax) !! 
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios               !! end code
  !c+++ [internal work]
  real(kind=r4b), allocatable    :: d4(:,:,:)         !!
  real(kind=r8b), allocatable    :: d8(:,:,:)         !!
  logical                        :: opend             !!

  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error in wgtd_r4: not opend, unit = ', jfile
    ios = 122
    return
  endif

  !c++ write output data
  if (head(38) == 'UR4') then
    allocate(d4(imax,jmax,kmax))
    d4(1:imax,1:jmax,1:kmax) = d(1:imax,1:jmax,1:kmax)
    write(jfile, iostat=ios) d4(1:imax,1:jmax,1:kmax)
    deallocate(d4)
  else if (head(38) == 'UR8') then
    allocate(d8(imax,jmax,kmax))
    d8(1:imax,1:jmax,1:kmax) = d(1:imax,1:jmax,1:kmax)
    write(jfile, iostat=ios) d8(1:imax,1:jmax,1:kmax)
    deallocate(d8)
  else
    write(6, *) 'Error in wgtd_r4:', head(38), 'is not supported yet.'
    stop 2
  endif

  if (ios /= 0) then
    write(6, '(a, i2)') 'Error in wgtd_r4: write gtool3 data, unit = ', jfile
  endif

  return
end subroutine wgtd_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wgtd_r8
!c  read gtool3 data
!c=====
subroutine wgtd_r8(jfile, imax, jmax, kmax, head, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile             !! I/O unit
  integer(kind=i4b), intent(in)  :: imax, jmax, kmax  !! x, y, z- sizes
  character(len=*), intent(in)   :: head(ndc)         !! header
  real(kind=r8b), intent(in)     :: d(imax,jmax,kmax) !!
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ios               !! end code
  !c+++ [internal work]
  real(kind=r4b), allocatable    :: d4(:,:,:)         !!
  real(kind=r8b), allocatable    :: d8(:,:,:)         !!
  logical                        :: opend             !!

  !c+++ check
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error in wgtd_r8: not opend, unit = ', jfile
    ios = 122
    return
  endif

  !c++ write output data
  if (head(38) == 'UR4') then
    allocate(d4(imax,jmax,kmax))
    d4(1:imax,1:jmax,1:kmax) = d(1:imax,1:jmax,1:kmax)
    write(jfile, iostat=ios) d4(1:imax,1:jmax,1:kmax)
    deallocate(d4)
  else if (head(38) == 'UR8') then
    allocate(d8(imax,jmax,kmax))
    d8(1:imax,1:jmax,1:kmax) = d(1:imax,1:jmax,1:kmax)
    write(jfile, iostat=ios) d8(1:imax,1:jmax,1:kmax)
    deallocate(d8)
  else
    write(6, *) 'Error in wgtd_r8:', head(38), 'is not supported yet.'
    stop 2
  endif

  if (ios /= 0) then
    write(6, '(a, i2)') 'Error in wgtd_r8: write gtool3 data, unit = ', jfile
  endif

  return
end subroutine wgtd_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgthd_r4
!c  read gtool3 header
!c=====
subroutine rgthd_r4(jfile, head, imax, jmax, kmax, vmiss, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile     !! I/O unit
  !c+++ [output]
  character(len=*), intent(out)  :: head(ndc) !! header
  integer(kind=i4b), intent(out) :: imax      !! x-size
  integer(kind=i4b), intent(out) :: jmax      !! y-size
  integer(kind=i4b), intent(out) :: kmax      !! z-size
  real(kind=r4b), intent(out)    :: vmiss     !! missing value
  integer(kind=i4b), intent(out) :: ios       !! end code
  !c+++ [internal work]
  logical                        :: opend     !!

  !c+++ inquire file status
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 111
    return
  endif

  !c+++ read
  read(jfile, iostat=ios) head
  if (ios == 0) then
    !c+++ get size
    read(head(31), *) imax
    read(head(34), *) jmax
    read(head(37), *) kmax
    read(head(39), *) vmiss
  else
    if (ios == -1) then
      write(6, '(a, i3)') 'Reach EOF: in rgthd'
    else
      write(6, '(a, i3)') 'Error: in rgthd'
    endif
  endif

  return
end subroutine rgthd_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgthd_r8
!c  read gtool3 header
!c=====
subroutine rgthd_r8(jfile, head, imax, jmax, kmax, vmiss, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile     !! I/O unit
  !c+++ [output]
  character(len=*), intent(out)  :: head(ndc) !! header
  integer(kind=i4b), intent(out) :: imax      !! x-size
  integer(kind=i4b), intent(out) :: jmax      !! y-size
  integer(kind=i4b), intent(out) :: kmax      !! z-size
  real(kind=r8b), intent(out)    :: vmiss     !! missing value
  integer(kind=i4b), intent(out) :: ios       !! end code
  !c+++ [internal work]
  logical                        :: opend     !!

  !c+++ inquire file status
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 111
    return
  endif

  !c+++ read
  read(jfile, iostat=ios) head
  if (ios == 0) then
    !c+++ get size
    read(head(31), *) imax
    read(head(34), *) jmax
    read(head(37), *) kmax
    read(head(39), *) vmiss
  else
    if (ios == -1) then
      write(6, '(a, i3)') 'Reach EOF: in rgthd'
    else
      write(6, '(a, i3)') 'Error: in rgthd'
    endif
  endif

  return
end subroutine rgthd_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgtd_r4
!c  read gtool3 data
!c=====
subroutine rgtd_r4(jfile, imax, jmax, kmax, head, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile              !! I/O unit
  integer(kind=i4b), intent(in)  :: imax               !! x-size
  integer(kind=i4b), intent(in)  :: jmax               !! y-size
  integer(kind=i4b), intent(in)  :: kmax               !! z-size
  character(len=*), intent(in)   :: head(ndc)          !! header
  !c+++ [output]
  real(kind=r4b), intent(out)    :: d(imax,jmax,kmax)  !! data
  integer(kind=i4b), intent(out) :: ios                !! end code
  !c+++ [internal work]
  real(kind=r4b), allocatable    :: d4(:,:,:)          !!
  real(kind=r8b), allocatable    :: d8(:,:,:)          !!
  character(len=ncc)             :: hdfmt              !! UR4/UR8
  logical                        :: opend              !!

  !c+++ inquire file status
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 112
    return
  endif

  !c++ read gtool3 data
  hdfmt=trim(head(38))
  call mch_rmchar(hdfmt, ' ')
  if (trim(hdfmt) == 'UR4') then
    allocate(d4(imax,jmax,kmax))
    read(jfile, iostat=ios) d4(1:imax,1:jmax,1:kmax)
    d(1:imax,1:jmax,1:kmax) = d4(1:imax,1:jmax,1:kmax)
    deallocate(d4)
  else if (trim(hdfmt) == 'UR8') then
    allocate(d8(imax,jmax,kmax))
    read(jfile, iostat=ios) d8(1:imax,1:jmax,1:kmax)
    d(1:imax,1:jmax,1:kmax) = d8(1:imax,1:jmax,1:kmax)
    deallocate(d8)
  else
    write(6, *) 'Error:', hdfmt, ' is not supported yet.'
    stop 2
  endif

  return
end subroutine rgtd_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rgtd_r8
!c  read gtool3 data
!c=====
subroutine rgtd_r8(jfile, imax, jmax, kmax, head, d, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: jfile              !! I/O unit
  integer(kind=i4b), intent(in)  :: imax               !! x-size
  integer(kind=i4b), intent(in)  :: jmax               !! y-size
  integer(kind=i4b), intent(in)  :: kmax               !! z-size
  character(len=*), intent(in)   :: head(ndc)          !! header
  !c+++ [output]
  real(kind=r8b), intent(out)    :: d(imax,jmax,kmax)  !! data
  integer(kind=i4b), intent(out) :: ios                !! end code
  !c+++ [internal work]
  real(kind=r4b), allocatable    :: d4(:,:,:)          !! input data (UR4)
  real(kind=r8b), allocatable    :: d8(:,:,:)          !! input data (UR8)
  character(len=ncc)             :: hdfmt              !! UR4/UR8
  logical                        :: opend              !!

  !c+++ inquire file status
  inquire(unit=jfile, opened=opend)
  if (.not. opend) then
    write(6, '(a, i3)') 'Error: not opend, unit = ', jfile
    ios = 112
    return
  endif

  !c++ read gtool3 data
  hdfmt=trim(head(38))
  call mch_rmchar(hdfmt, ' ')
  if (trim(hdfmt) == 'UR4') then
    allocate(d4(imax,jmax,kmax))
    read(jfile, iostat=ios) d4(1:imax,1:jmax,1:kmax)
    d(1:imax,1:jmax,1:kmax) = d4(1:imax,1:jmax,1:kmax)
    deallocate(d4)
  else if (trim(hdfmt) == 'UR8') then
    allocate(d8(imax,jmax,kmax))
    read(jfile, iostat=ios) d8(1:imax,1:jmax,1:kmax)
    d(1:imax,1:jmax,1:kmax) = d8(1:imax,1:jmax,1:kmax)
    deallocate(d8)
  else
    write(6, *) 'Error:', hdfmt, ' is not supported yet.'
    stop 2
  endif

  return
end subroutine rgtd_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine mch_rmchar
!c  remove character from input word
!c=====
subroutine mch_rmchar(word, c)
  !c+++ [input/output]
  character(len=*), intent(inout) :: word      !! data
  !c+++ [input]
  character(len=1), intent(in)    :: c         !! remove char.
  !c+++ [internal work]
  integer(kind=i4b)               :: i, j      !!
  character(len=nfiln)            :: a, b      !!
  a = word
  b = ""
  j = 1
  do i = 1, len_trim(a)
    if (a(i:i) /= c) then
      b(j:j) = a(i:i)
      j = j + 1
    endif
  enddo
  word = b
  return
end subroutine mch_rmchar

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  function rmc
!c  similar to mch_rmchar
!c=====
function rmc(str, c)
  !c+++ [input]
  character(len=*), intent(inout) :: str       !! data
  character(len=1), intent(in)    :: c         !! remove char.
  !c+++ [internal work]
  integer(kind=i4b)               :: i, j      !!
  !c+++ [function]
  character(len(str))             :: rmc       !!
  rmc = ""
  j = 1
  do i = 1, len_trim(str)
    if (str(i:i) /= c) then
      rmc(j:j) = str(i:i)
      j = j + 1
    endif
  enddo
end function rmc

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine lmrange_r4
!c  max./min. limit of data
!c=====
subroutine lmrange_r4(nx, ny, nz, vmiss, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz  !! x-, y-, z- grid sizes
  real(kind=r4b), intent(in)    :: vmiss       !! missing value
  !c+++ [output]
  real(kind=r4b), intent(inout) :: d(nx,ny,nz) !! data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k     !!

  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        if (d(i,j,k) <= vmin.or.d(i,j,k) >= vmax) then
          d(i,j,k) = vmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine lmrange_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine lmrange_r8
!c  max./min. limit of data
!c=====
subroutine lmrange_r8(nx, ny, nz, vmiss, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz  !! x-, y-, z- grid sizes
  real(kind=r8b), intent(in)    :: vmiss       !! missing value
  !c+++ [output]
  real(kind=r8b), intent(inout) :: d(nx,ny,nz) !! data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k     !!

  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        if (d(i,j,k) <= vmin.or.d(i,j,k) >= vmax) then
          d(i,j,k) = vmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine lmrange_r8

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rmeps_r4
!c  reset missing data values within eps
!c=====
subroutine rmeps_r4(nx, ny, nz, vmiss, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz  !! x-, y-, z- grid sizes
  real(kind=r4b), intent(in)    :: vmiss       !! missing value
  !c+++ [output]
  real(kind=r4b), intent(inout) :: d(nx,ny,nz) !! data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k     !!

  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        if (abs(d(i,j,k) - vmiss) < eps) then
          d(i,j,k) = vmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine rmeps_r4

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine rmeps_r8
!c  reset missing data values within eps
!c=====
subroutine rmeps_r8(nx, ny, nz, vmiss, d)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz  !! x-, y-, z- grid sizes
  real(kind=r8b), intent(in)    :: vmiss       !! missing value
  !c+++ [output]
  real(kind=r8b), intent(inout) :: d(nx,ny,nz) !! data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k     !!

  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        if (abs(d(i,j,k) - vmiss) < eps) then
          d(i,j,k) = vmiss
        endif
      enddo !! i
    enddo !! j
  enddo !! k

  return
end subroutine rmeps_r8

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine set_gthd
!c  set gtool3 header (create new header)
!c=====
subroutine set_gthd(imax, jmax, kmax, vmiss, hitem, htitl, hunit, &
& hdset, hdfmt, time, tdur, htunit, jdate, haxisx, haxisy, haxisz, head)
  !c+++ [input]
  integer(kind=i4b), intent(in)   :: imax, jmax, kmax  !! x, y, z軸のサイズ
  real(kind=r8b), intent(in)      :: vmiss             !! 欠損値
  character(len=*), intent(in)    :: hitem             !! 変数名
  character(len=*), intent(in)    :: htitl             !! タイトル 
  character(len=*), intent(in)    :: hunit             !! 単位
  character(len=*), intent(in)    :: hdset             !! データセット名
  character(len=*), intent(in)    :: hdfmt             !! データフォーマット
  real(kind=r8b), intent(in)      :: time              !! 時刻
  real(kind=r8b), intent(in)      :: tdur              !! 代表時間
  character(len=*), intent(in)    :: htunit            !! 時刻単位
  integer(kind=i4b), intent(in)   :: jdate(6)          !! 日付
  character(len=*), intent(in)    :: haxisx            !! x軸名称
  character(len=*), intent(in)    :: haxisy            !! y軸名称
  character(len=*), intent(in)    :: haxisz            !! z軸名称
  !c+++ [output]
  character(len=ncc), intent(out) :: head(ndc)         !! header
  !c+++ [internal work]
  integer(kind=i4b)               :: i                 !!
  integer(kind=i2b)               :: jdates(7)         !! input from getdat, gettim
  integer(kind=i4b)               :: jdaten(6)         !!
  character(len=ncc*2)            :: htitlz            !!
  character(len=ncc)              :: huser             !!

  !ccc call idate(jdates)
  !ccc call itime(jdates(4))
  !ccc jdaten(1) = jdates(3)
  !ccc jdaten(2) = jdates(2)
  !ccc jdaten(3) = jdates(1)
  !ccc jdaten(4:6) = jdates(4:6)
  call getdat(jdates(1), jdates(2), jdates(3))
  call gettim(jdates(4), jdates(5), jdates(6), jdates(7))
  jdaten(1:6) = jdates(1:6)

  call getenv('USER', huser)

  head(1:ndc) = ' '

  write(head(1), '(i16)') 9010      
  head(2) = hdset
  head(3) = hitem
  !write(head(12), '(i16)') 1   
  !write(head(13), '(i16)') 1
  head(12) = '1   '
  head(13) = '1   '
  htitlz   = htitl
  head(14) = htitlz(1:16)
  head(15) = htitlz(17:32)
  head(16) = hunit
  write(head(25), '(i16)') int(time)
  write(head(28), '(i16)') int(tdur)
  head(26) = htunit
  write(head(27), '(i4.4, 2i2.2, 1x, 3i2.2)') jdate(1:6)
  head(29) = haxisx
  write(head(30), '(i16)') 1         
  write(head(31), '(i16)') imax
  head(32) = haxisy
  write(head(33), '(i16)') 1         
  write(head(34), '(i16)') jmax
  head(35) = haxisz
  write(head(36), '(i16)') 1         
  write(head(37), '(i16)') kmax
  head(38) = hdfmt
  do i = 39, 43
    write(head(i), '(e16.7)') vmiss
  enddo
  write(head(44), '(i16)') 1
  write(head(46), '(i16)') 0
  write(head(47), '(e16.7)') 0.      
  write(head(48), '(i16)') 0
  write(head(60), '(i4.4, 2i2.2, 1x, 3i2.2)') jdaten(1:6)
  head(61) = huser
  write(head(62), '(i4.4, 2i2.2, 1x, 3i2.2)') jdaten(1:6)
  head(63) = huser
  write(head(64), '(i16)') imax*jmax*kmax

  return
end subroutine set_gthd

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_hdtime
!c  read gtool3 header and get time with related unit
!c=====
subroutine get_hdtime(time, tdur, htunit, jdate, head)
  !c+++ [input]
  character(len=*), intent(in)   :: head(ndc)  !! header
  !c+++ [output]
  real(kind=r8b), intent(out)    :: time      !! 時刻
  real(kind=r8b), intent(out)    :: tdur      !! 代表時間
  character(len=*), intent(out)  :: htunit    !! 時刻単位
  integer(kind=i4b), intent(out) :: jdate (6) !! 日付
  !c+++ [internal work]

  htunit = head(26)
  read(head(25), *) time
  read(head(28), *) tdur

  read(head(27), '(i4.4, i2.2, i2.2, 1x, i2.2, i2.2, i2.2)') jdate(1:6)

  return
end subroutine get_hdtime

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_hdtime2
!c  read gtool3 header and get time with related unit + start/end time
!c=====
subroutine get_hdtime2(time, tdur, htunit, jdate, jdate1, jdate2, head, oavr)
  !c+++ [input]
  character(len=*), intent(in)   :: head(ndc)  !! header
  !c+++ [output]
  real(kind=r8b), intent(out)    :: time       !! 時刻
  real(kind=r8b), intent(out)    :: tdur       !! 代表時間
  character(len=*), intent(out)  :: htunit     !! 時刻単位
  integer(kind=i4b), intent(out) :: jdate (6)  !! 日付
  integer(kind=i4b), intent(out) :: jdate1 (6) !! 日付（開始）
  integer(kind=i4b), intent(out) :: jdate2 (6) !! 日付（終了）
  logical, intent(out)           :: oavr       !! 平均, t: 平均値, f: 瞬間値
  !c+++ [internal work]
  integer(kind=i4b)              :: i          !!

  htunit = head(26)
  read(head(25), *) time
  read(head(28), *) tdur

  jdate(1:6) = 0
  jdate1(1:6) = 0
  jdate2(1:6) = 0
  read(head(27), '(i4.4, i2.2, i2.2, 1x, i2.2, i2.2, i2.2)') jdate(1:6)
  read(head(48), '(i4.4, i2.2, i2.2, 1x, i2.2, i2.2, i2.2)') jdate1(1:6)
  read(head(49), '(i4.4, i2.2, i2.2, 1x, i2.2, i2.2, i2.2)') jdate2(1:6)

  oavr = .false.
  do i = 1, 6
    if (jdate(i) /= jdate2(i)) oavr = .true.
    if (jdate1(i) == 0.or.jdate2(i) == 0) then
      jdate1(i) = jdate(i)
      jdate2(i) = jdate(i)
      oavr = .true.
    endif
  enddo !! i

  return
end subroutine get_hdtime2

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_hdall
!c  read gtool3 header
!c=====
subroutine get_hdall(head, imax, jmax, kmax, vmiss, hitem, htitl, hunit, &
& hdset, time, tdur, htunit, jdate, haxisx, haxisy, haxisz)
  !c+++ [input]
  character(len=*), intent(in)   :: head(ndc)        !! header
  !c+++ [output]
  integer(kind=i4b), intent(out) :: imax, jmax, kmax !! x, y, z軸のサイズ
  real(kind=r8b), intent(out)    :: vmiss            !! 欠損値
  character(len=*), intent(out)  :: hitem            !! 変数名
  character(len=*), intent(out)  :: htitl            !! タイトル 
  character(len=*), intent(out)  :: hunit            !! 単位
  character(len=*), intent(out)  :: hdset            !! データセット名
  real(kind=r8b), intent(out)    :: time             !! 時刻
  real(kind=r8b), intent(out)    :: tdur             !! 代表時間
  character(len=*), intent(out)  :: htunit           !! 時刻単位
  integer(kind=i4b), intent(out) :: jdate(6)         !! 日付
  character(len=*), intent(out)  :: haxisx           !! x軸名称
  character(len=*), intent(out)  :: haxisy           !! y軸名称
  character(len=*), intent(out)  :: haxisz           !! z軸名称
  !c+++ [internal work]
  character(len=ncc*2)           :: htitlz           !!

  hdset = head(2)
  hitem = head(3)

  htitlz(1:16) = head(14)
  htitlz(17:32) = head(15)
  htitl = htitlz

  hunit = head(16)
  htunit = head(26)
  read(head(25), *) time
  read(head(28), *) tdur

  read(head(27), '(i4.4, i2.2, i2.2, 1x, i2.2, i2.2, i2.2)') jdate(1:6)

  haxisx = head(29)
  haxisy = head(32)
  haxisz = head(35)

  read(head(31), *) imax
  read(head(34), *) jmax
  read(head(37), *) kmax
  read(head(39), *) vmiss

  return
end subroutine get_hdall

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_axis_r4
!c  get axis points from axis information file
!c=====
subroutine get_axis_r4(nmax, haxname, dout)
  use error_handler, only: ioerror
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nmax       !! axis size
  character(len=*), intent(in)   :: haxname    !! axis name
  !c+++ [output]
  real(kind=r4b), intent(out)    :: dout(nmax) !! axis points
  !c+++ [internal work]
  integer(kind=i4b)              :: ios        !! end code
  integer(kind=i4b)              :: jfile      !! I/O unit number
  character(len=ncc)             :: head(ndc)  !!
  character(len=nfiln)           :: gtax       !! GTAXDIR
  character(len=nfiln)           :: filename   !!
  real(kind=r8b)                 :: d(nmax)    !! data for read

  !c+++ get parameter
  call getenv('GTAXDIR', gtax)
  write(6, *) 'GTAXDIR=', trim(gtax)

  !c+++ open axis information file
  filename = trim(gtax)//'/GTAXLOC.'//trim(haxname)
  call gtopen(trim(filename), 'r', jfile, ios)
  if (ios /= 0) call ioerror(jfile, ios)

  !c+++ read axis information file
  call rgt(jfile, nmax, 1, 1, head, d, ios)
  if (ios /= 0) call ioerror(jfile, ios)
  dout(1:nmax) = d(1:nmax)
  call gtclose(jfile, ios)
  if (ios /= 0) call ioerror(jfile, ios)

  return
end subroutine get_axis_r4

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_axis_r8
!c  get axis points from axis information file
!c=====
subroutine get_axis_r8(nmax, haxname, dout)
  use error_handler, only: ioerror
  !c+++ [input]
  integer(kind=i4b), intent(in)  :: nmax       !! axis size
  character(len=*), intent(in)   :: haxname    !! axis name
  !c+++ [output]
  real(kind=r8b), intent(out)    :: dout(nmax) !! axis points
  !c+++ [internal work]
  integer(kind=i4b)              :: ios        !! end code
  integer(kind=i4b)              :: jfile      !! I/O unit number
  character(len=ncc)             :: head(ndc)  !!
  character(len=nfiln)           :: gtax       !! GTAXDIR
  character(len=nfiln)           :: filename   !!
  real(kind=r8b)                 :: d(nmax)    !! data for read

  !c+++ get parameter
  call getenv('GTAXDIR', gtax)
  write(6, *) 'GTAXDIR=', trim(gtax)

  !c+++ open axis information file
  filename = trim(gtax)//'/GTAXLOC.'//trim(haxname)
  call gtopen(trim(filename), 'r', jfile, ios)
  if (ios /= 0) call ioerror(jfile, ios)

  !c+++ read axis information file
  call rgt(jfile, nmax, 1, 1, head, d, ios)
  if (ios /= 0) call ioerror(jfile, ios)
  dout(1:nmax) = d(1:nmax)
  call gtclose(jfile, ios)
  if (ios /= 0) call ioerror(jfile, ios)

  return
end subroutine get_axis_r8

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_zaxsize
!c  get z-axis size from axis information file
!c=====
subroutine get_zaxsize(haxisz, kmax)
  use error_handler, only: ioerror
  !c+++ [input]
  character(len=*), intent(in)   :: haxisz     !! z-axis name
  !c+++ [output]
  integer(kind=i4b), intent(out) :: kmax       !! z-axis size
  !c+++ [internal work]
  integer(kind=i4b)              :: ios        !! end code
  integer(kind=i4b)              :: jfile      !! I/O unit number
  character(len=ncc)             :: head(ndc)  !! gtool header
  character(len=nfiln)           :: gtax       !! GTAXDIR
  character(len=nfiln)           :: filename

  !c+++ get parameter
  call getenv('GTAXDIR', gtax)
  write(6, *) 'GTAXDIR=', trim(gtax)

  !c+++ open z-axis file
  filename = trim(gtax)//'/GTAXLOC.'//trim(haxisz)
  call gtopen(trim(filename), 'r', jfile, ios)
  if (ios /= 0) then
    write(6, *) 'Error: open '//trim(filename)
    call ioerror(jfile, ios)
    return
  endif

  !c+++ read z-axis file
  call rgth(jfile, head, ios)
  if (ios /= 0) call ioerror(jfile, ios)

  !c+++ read z-axis size from header
  read(head(31), *) kmax

  return
end subroutine get_zaxsize

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine get_etacoef
!c  get eta_fa and eta_fb from eta-axis information file
!c=====
subroutine get_etacoef(kmax, haxisz, eta_fa, eta_fb)
  use error_handler, only: werrc
  !c+++ [input]
  integer(kind=i4b), intent(in) :: kmax         !! z-axis size
  character(len=*), intent(in)  :: haxisz       !! z-axis name
  !c+++ [output]
  real(kind=r8b), intent(out)   :: eta_fa(kmax) !! for eta half lev.
  real(kind=r8b), intent(out)   :: eta_fb(kmax) !! for eta half lev.
  !c+++ [internal work]
  integer(kind=i4b)             :: ios          !! end code
  integer(kind=i4b)             :: jfile        !! I/O unit number
  character(len=ncc)            :: head(ndc)    !! gtool header
  character(len=nfiln)          :: gtax         !! GTAXDIR
  character(len=nfiln)          :: filename     !!
  !c+++ for eta half lev.
  real(kind=r8b)                :: eta(kmax)    !! eta lev.
  !c+++ data for read
  real(kind=r8b)                :: d(kmax)      !!

  !c+++ get parameter
  call getenv('GTAXDIR', gtax)
  !ccc write(6, *) 'GTAXDIR=', trim(gtax)

  !c+++ open eta axis file
  filename = trim(gtax)//'/GTAXLOC.'//trim(haxisz)
  call gtopen(trim(filename), 'r', jfile, ios)
  if (ios /= 0) call werrc(ios, 'Error: open '//trim(filename))

  !c+++ read eta axis file
  call rgt(jfile, kmax, 1, 1, head, eta, ios)
  if (ios /= 0) call werrc(ios, 'file read error')
  call rgt(jfile, kmax, 1, 1, head, d, ios)
  if (ios /= 0) call werrc(ios, 'file read error')
  eta_fa(1:kmax) = d(1:kmax)
  call rgt(jfile, kmax, 1, 1, head, d, ios)
  if (ios /= 0) call werrc(ios, 'file read error')
  eta_fb(1:kmax) = d(1:kmax)
  close(jfile)

  return
end subroutine get_etacoef

!c----------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine seek_iounit
!c  seek unused I/O unit
!c=====
subroutine seek_iounit(jfile, ios)
  !c+++ [internal parameter]
  integer(kind=i4b), parameter   :: jfile_min = 11  !! min. of I/O unit number
  integer(kind=i4b), parameter   :: jfile_max = 100 !! max. of I/O unit number
  !c+++ [output]
  integer(kind=i4b), intent(out) :: jfile           !! I/O unit number
  integer(kind=i4b), intent(out) :: ios             !! end code
  !c+++ [internal work]
  logical :: opend

  ios = 1
  jfile = jfile_min
  do while (1 == 1)
    if (jfile > jfile_max) then
      write(6, '(a)') 'I/O unit full'
      ios = 1
      return
    endif
    !c+++ check unit status
    inquire(unit=jfile, opened=opend)
    if (.not. opend) then
      ios = 0
      return
    endif
    jfile = jfile + 1
  enddo

  return
end subroutine seek_iounit

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine wmsg
!c=====
subroutine wmsg(head, msg)
  !c+++ [input]
  character(len=*), intent(in)   :: head(ndc) !! header
  character(len=*), intent(in)   :: msg       !! message
  !c+++ [internal work]
  integer(kind=i4b)              :: i         !!
  integer(kind=i4b)              :: jdim(3)   !! x, y, z軸のサイズ
  character(len=10)              :: hdim(3)   !!

  !c+++ get size
  read(head(31), *) jdim(1)
  read(head(34), *) jdim(2)
  read(head(37), *) jdim(3)

  do i = 1, 3
    if (jdim(i) < 10) then
      write(hdim(i), '(i1)') jdim(i)
    else if (jdim(i) < 100) then
      write(hdim(i), '(i2)') jdim(i)
    else if (jdim(i) < 1000) then
      write(hdim(i), '(i3)') jdim(i)
    else if (jdim(i) < 10000) then
      write(hdim(i), '(i4)') jdim(i)
    else
      write(hdim(i), '(i5)') jdim(i)
    endif
  enddo
  !c+++ write message
  write(6, '(3a, 1x, 9a, 1x, 5a)') trim(msg), ': ', trim(head(3)), &
&   '[', trim(head(27)), '] (', trim(head(29)), ',', trim(head(32)), &

&   ',', trim(head(35)), ')', trim(hdim(1)), 'x', trim(hdim(2)), 'x', trim(hdim(3))
  return
end subroutine wmsg

!c---------------------------------------------------------------------c

!c======================================================================c

end module rwgtool
