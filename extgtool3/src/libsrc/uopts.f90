!c  module util
!c  [history]
!c  2013/01/30 Yamashita: first ver. with subroutines getparms, tcent
!c  2013/03/10 Yamashita: add read_parms, get_parms
!c  2013/03/10 Yamashita: add get_strend: output start/end time
!c  2013/03/10 Yamashita: move tcent to ucaln.f90
!c  2013/09/27 Yamashita: add get_common
!c  2014/08/05 Yamashita: modify get_parms
!c  2017/05/01 Yamashita: use common_typedef
!c  2017/08/04 Yamashita: add get_comname
!c  2018/07/19 Yamashita: modify read_parms
!c  2021/08/22 Yamashita: add read_opt
!c
!c  utilities for parameter input
!c
!c  public:
!c  subroutine read_parms: read parameters
!c  subroutine get_comname: get command name
!c  subroutine get_parms: get saved parms
!c  subroutine get_strend: output start/end time
!c  subroutine read_opt: read options
!c  subroutine get_common: get common parameters
!c    internal:
!c    subroutine xabort_common
!c=====================================================================c
module uopts
  use common_args, only: ncc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  implicit none
  private
  public :: read_parms, get_comname, get_parms, get_strend, read_opt, get_common

  !c+++ [common]
  !c+++ options for store
  character(len=nfiln), private              :: comname   !! command name
  character(len=nfiln), allocatable, private :: opts(:,:) !! (:,1): name, (:,2): val
  !c+++ number of stored options
  integer(kind=i4b), private                 :: num_opts

  contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine read_parms
!c  read parameters
!c=====
subroutine read_parms(ierr)
  !c+++ [output]
  integer(kind=i4b), intent(out) :: ierr  !! end code
  !c+++ [internal work]
  !c+++ for read
  character(len=nfiln)           :: opt   !!
  !c+++ for store (move to util_common)
  !ccc character(len=16), allocatable :: opts(:,:) !! (:,1): name, (:,2): val
  !c+++ function
  integer(kind=i4b)              :: iargc !! max. number of argument
  !c+++ loop variables
  integer(kind=i4b)              :: i     !! argument number
  !c+++ other variables
  integer(kind=i4b)              :: nopt  !! option number
  integer(kind=i4b)              :: ncol  !! column number
  logical                        :: ostr  !! store option value

  ierr = 0
  allocate(opts(iargc(),2))
  comname = ' '
  opts(:,:) = ' '
  !c+++ read arguments {like C, main(int argc, char **argv)}
  call getarg(0, comname) !! command name
  ostr = .false.
  nopt = 0
  do i = 1, iargc()
    call getarg(i, opt)
    ncol = scan(opt, '=')
    !c+++ store option names & values
    if (trim(opt) == '-h'.or.trim(opt) == '--help') then
      !c+++ in case of [-h, --help]
      ierr = 2
      return
    else if (opt(1:1) == '-') then
      nopt = nopt + 1
      if (ncol /= 0) then
        !c+++ in case of [-name=val]
        opts(nopt,1) = opt(2:ncol-1)
        opts(nopt,2) = opt(ncol+1:len_trim(opt))
        ostr = .false.
      else
        !c+++ in case of [-name val], or [-name]
        opts(nopt,1) = opt(2:len_trim(opt))
        opts(nopt,2) = 't' ! for [-name]: true
        ostr = .true. !! ignore osta in next '-' input, or 'name=val' input
      endif
    else if (opt(1:1) == '#') then
      nopt = nopt + 1
      !c+++ in case of [#name]: false
      opts(nopt,1) = opt(2:len_trim(opt))
      opts(nopt,2) = 'f'
      ostr = .true. !! ignore osta in next '-' input, or 'name=val' input
    else if (ncol /= 0) then
      nopt = nopt + 1
      !c+++ in case of [name=val]
      opts(nopt,1) = opt(1:ncol-1)
      opts(nopt,2) = opt(ncol+1:len_trim(opt))
      ostr = .false.
    else if (ostr) then
      !c+++ store option value (-name val)
      opts(nopt,2) = trim(opt)
      ostr = .false.
    else
      write(6, '(2a)') 'Unexpected option, value = ', trim(opt)
      ierr = 1
      return
    endif
  enddo !! i
  !c+++ number of saved options (in util_common)
  num_opts = nopt

  return
end subroutine read_parms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine get_comname
!c  get saved command name
!c=====
subroutine get_comname(hval, ierr)
  !c+++ [output]
  character(len=*), intent(out)    :: hval  !! option value
  integer(kind=i4b), intent(out)   :: ierr  !! end code
  hval = ' '
  if (comname /= ' ') then
    hval = trim(comname)
    ierr = 0
  else
    ierr = 1
  endif

  return
end subroutine get_comname

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine get_parms
!c  get saved parms
!c=====
subroutine get_parms(hname, hdef, hval, ierr)
  !c+++ [input]
  character(len=*), intent(in)     :: hname !! option name
  character(len=*), intent(in)     :: hdef  !! default option value
  !c+++ [output]
  character(len=*), intent(out)    :: hval  !! option value
  integer(kind=i4b), intent(inout) :: ierr  !! end code
  !c+++ [internal work]
  integer(kind=i4b)                :: iflag !!
  !c+++ loop variables
  integer(kind=i4b)                :: i     !! argument number
  !c+++ [internal save]
  logical, save                    :: ofirst = .true.

  !c+++ once
  if (ofirst) then
    ofirst = .false.
    ierr = 0
  endif
  !c+++ set option value
  hval = ' '
  iflag = 1
  do i = 1, num_opts
    if (hname == opts(i,1)) then
      hval = trim(opts(i,2))
      iflag = 0
      exit
    endif
  enddo !! i
  if (iflag == 0) then
    !c+++ write option value
    write(6, '((4a))') 'get_parms: ', trim(hname), ' = ', trim(hval) 
  else
    !c+++ set/write default option value
    hval = trim(hdef)
    write(6, '((5a))') 'get_parms: ', trim(hname), ' = ', trim(hval), ' (default)'
    iflag = 0
  endif

  ierr = max(ierr, iflag)

  return
end subroutine get_parms

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine get_strend
!c  output start/end time
!c=====
subroutine get_strend(ista, iend, ierr)
  !c+++ [output]
  integer(kind=i4b), intent(out)   :: ista !! start time
  integer(kind=i4b), intent(out)   :: iend !! end time
  integer(kind=i4b), intent(inout) :: ierr !! end code
  !c+++ [internal work]
  character(len=ncc) :: hval !! option value

  !c+++ get start time
  call get_parms('sta', '-9999', hval, ierr)
  read(hval, '(i16)') ista
  !c+++ get end time
  call get_parms('end', '-9999', hval, ierr)
  read(hval, '(i16)') iend

  return
end subroutine get_strend

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine read_opt
!c  read options separated by ,
!c=====
subroutine read_opt(nv, hdump, hout)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nv        !! number of options
  character(len=*), intent(in)  :: hdump     !! for header information
  !c+++ [output]
  character(len=*), intent(out) :: hout(nv)  !! for header information
  !c+++ [internal work]
  integer(kind=i4b)             :: ivar      !! loop variable
  !c+++ other variables
  integer(kind=i4b)             :: ncol      !! number of character in column
  integer(kind=i4b)             :: ncs       !! start number of column

  ncs = 1
  do ivar = 1, nv
    ncol = scan(hdump(ncs:len_trim(hdump)), ',')
    if (ncol == 0) then
      hout(ivar) = hdump(ncs:len_trim(hdump))
      exit
    endif
    hout(ivar) = hdump(ncs:ncs+ncol-2)
    ncs = ncs + ncol
  enddo !! ivar

  return
end subroutine read_opt

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine get_common
!c  get common parameters
!c=====
subroutine get_common(prog_name, ifile, ofile, hitem, htitl, hunit, ista, iend)
  !c+++ [input]
  character(len=*), intent(in)   :: prog_name         !! program name
  !c+++ [output]
  !c+++ input from get_parms
  character(len=*), intent(out)  :: ifile            !! input file
  character(len=*), intent(out)  :: ofile            !! output file
  character(len=*), intent(out)  :: hitem            !! new item of output data
  character(len=*), intent(out)  :: htitl            !! new title of output data
  character(len=*), intent(out)  :: hunit            !! new unit of output data
  !c+++ input from get_strend
  integer(kind=i4b), intent(out) :: ista, iend       !! start/end time
  !c+++ [internal work]
  integer(kind=i4b)              :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort_common(prog_name)
  call get_parms('i', 'gtool.in', ifile, ios)
  call get_parms('o', 'gtool.out', ofile, ios)
  call get_parms('item', 'NULL', hitem, ios)
  call get_parms('titl', 'NULL', htitl, ios)
  call get_parms('unit', 'NULL', hunit, ios)
  call get_strend(ista, iend, ios)
  if (ios /= 0) call xabort_common(prog_name)

  return
end subroutine get_common

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine xabort_common
!c=====

subroutine xabort_common(prog_name)
  character(len=*), intent(in) :: prog_name         !! program name
  write(6, '(a)') 'Usage: '
  write(6, '(2a)') trim(prog_name), ' -i input-file -o output-file'
  write(6, '(a)') '-sta start-time -end end-time'
  write(6, '(a)') '-item hitem -titl title -unit unit'
  stop 2
end subroutine xabort_common

!c----------------------------------------------------------------------c

!c=====================================================================c

end module uopts
