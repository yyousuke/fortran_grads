!c
!c  program histchk
!c  [history]
!c  2013/03/06 Yamashita: first ver.
!c  2017/05/10 Yamashita: modify hiskchk
!c  check nmhisd of namelist input file & print output file names
!c
!c  external: module common_args
!c  external: module common_typedef
!c  internal: module common_hist
!c  internal: subroutine ohist
!c    internal: subroutine handle_error
!c  internal: subroutine  rhist
!c  internal: subroutine chist
!c  internal: subroutine histcn
!c  internal: subroutine histld
!c
!c======================================================================c
module common_hist
  use common_args, only: ncc, nfiln
  use common_typedef, only: i4b
  integer(kind=i4b), parameter :: iunit = 21        !! input unit number
  integer(kind=i4b), parameter :: max_items = 1000  !! max. of items
  character(len=nfiln)         :: hitems(max_items) !! items
  data hitems /max_items * ' '/
end module common_hist

!c======================================================================c

program histchk
  use common_hist
  use common_typedef, only: i4b, r8b
  implicit none
  !c+++ [write]
  character(len=ncc) :: hitem !! item
  !c+++ [work]
  integer(kind=i4b)  :: id    !! loop variable
  integer(kind=i4b)  :: nitem !! number of saved items

  !c+++ open output file
  open(51, file='items.txt', status='unknown')

  !c+++ open SYSIN file
  call ohist('SYSIN')
  !c+++ read SYSIN file & register items
  call rhist
  !c+++ close SYSIN file
  call chist

  !c+++ count saved items
  call histcn(nitem)
  do id = 1, nitem
    !c+++ load items
    call histld(id, hitem)
    !c+++ write items
    write(51, '(i4, 1x, a)') id, trim(hitem)
  enddo !! nv

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine ohist
!c  open SYSIN file
!c=====
subroutine ohist(file_name)
  use common_hist, only: iunit
  !c+++ [input]
  character(len=*), intent(in) :: file_name  !!
  !c+++ [internal work]
  integer(kind=i4b)            :: ios        !! end code

  !c+++ open SYSIN file
  open(iunit, file=trim(file_name), status='old', iostat=ios) 
  if (ios /= 0) call handle_error(iunit, ios)

  return
end subroutine ohist

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine chist
!c  close SYSIN file
!c=====
subroutine chist
  use common_hist, only: iunit
  close(iunit) 
  return
end subroutine chist

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine rhist
!c  read SYSIN file
!c=====
subroutine rhist
  use common_hist, only: ncc, nfiln, iunit
  !c+++ [internal work]
  integer(kind=i4b)    :: ios        !! end code

  !c+++ nmhist
  character(len=ncc)   :: item       !! name of variables
  character(len=nfiln) :: file       !! file name
  integer(kind=i4b)    :: xsel       !! selected type of X
  integer(kind=i4b)    :: ysel       !! selected type of Y
  integer(kind=i4b)    :: zsel       !! selected type of Z
  real(kind=r8b)       :: tintv      !! output interval
  real(kind=r8b)       :: tavrg      !! accumulation interval
  character(len=ncc)   :: tunit      !! time unit
  integer(kind=i4b)    :: start(6)   !! start time
  integer(kind=i4b)    :: end(6)     !! finish time
  character(len=ncc)   :: dfmt       !! output format
  real(kind=r8b)       :: fact       !! factor
  logical              :: pout       !! output at p-level
  logical              :: plog       !! log-linear interpolation
  logical              :: psmlt      !! multiply Ps
  integer(kind=i4b)    :: diur       !! interval of diurnal out
  real(kind=r8b)       :: tmax       !! max.
  real(kind=r8b)       :: tmin       !! min.
  real(kind=r8b)       :: tvmax      !! max.
  real(kind=r8b)       :: tvmin      !! min.
  logical              :: square     !! square output
  logical              :: paraf      !! parallel output
  integer              :: maz        !!

  namelist /nmhist/ item  , file  , &
&                   xsel  , ysel  , zsel  , &
&                   tintv , tavrg , tunit , start , end   , &
&                   dfmt  , fact  , pout  , plog  , psmlt , &
&                   diur  , tmax  , tmin  , tvmax , tvmin , square, &
&                   paraf , maz
  !c+++ read
  !ccc do while (.not. eof(iunit))
  do while (1 == 1)
    read(iunit, nmhist, iostat=ios) !!  read nmhist
    if (ios /= 0) exit
    !c+++ register item
    call histrg(file)
  enddo

  return
end subroutine rhist

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine histrg
!c  register items in nmhist
!c=====
subroutine histrg(hitem)
  use common_hist
  !c+++ [input]
  character(len=*), intent(in) :: hitem  !! item
  !c+++ [internal work]
  integer(kind=i4b)            :: i      !! loop variable
  integer(kind=i4b)            :: nitems !! number of items
  nitems = 0
  i = 0
  do while(1 == 1)
    i = i + 1
    if (i > max_items) then
      write(6, *) 'Error: maximum item limit exceeded'
      stop 2
    endif

    nitems = nitems + 1
    !c+++ seek registered items
    if (hitems(i) == hitem) exit
    !c+++ register new item
    if (hitems(i) == ' ') then
      write(6, *) 'register new item: ', trim(hitem)
      hitems(i) = hitem
      exit
    endif
  enddo !! i

  return
end subroutine histrg

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine histld
!c load items
!c=====
subroutine histld(id, hitem)
  use common_hist
  !c+++ [input]
  integer(kind=i4b), intent(in) :: id    !! history number
  !c+++ [output]
  character(len=*), intent(out) :: hitem !! item

  if (id > max_items) then
    write(6, *) 'Error: input number exceeds maximum item limit'
    stop 2
  endif
  hitem = hitems(id)

  return
end subroutine histld

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine histcn
!c  count saved items
!c=====
subroutine histcn(nitem)
  use common_hist
  !c+++ [output]
  integer(kind=i4b), intent(out) :: nitem  !! number of saved items
  !c+++ [internal work]
  integer(kind=i4b)              :: i      !! loop variable
  integer(kind=i4b)              :: nitems !! number of items

  nitems = 0
  i = 0
  do while(1 == 1)
    i = i + 1
    if (i > max_items) then
      write(6, *) 'Error: maximum item limit exceeded'
      stop 2
    endif

    nitems = nitems + 1
    !c+++ register new item
    if (hitems(i) == ' ') then
      nitem = nitems - 1
      exit
    endif
  enddo !! i

  return
end subroutine histcn

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine handle_error
!c  error handler
!c=====
subroutine handle_error(iunit, ios)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: iunit !! unit number
  integer(kind=i4b), intent(in) :: ios   !! end code

  if (ios == -1) then
    write(6, *) 'Error: reach EOF, unit = ', iunit
  else if (ios == -2) then
    write(6, *) 'Error: data ended, unit = ', iunit
  else
    write(6, *) 'Error, unit = ', iunit, 'code = ', ios
  endif
  stop 2
end subroutine handle_error

!c----------------------------------------------------------------------c

!c=====================================================================c

end program histchk
