!c
!c  program gtitem
!c  [history]
!c  2013/10/16 Yamashita: first ver.
!c  2017/05/10 Yamashita: modify I/O error
!c  2017/05/10 Yamashita: use common_typedef
!c  
!c  read SYSIN and show item
!c
!c=====================================================================c

module common_gtitem
  use common_args, only: ncc, nfiln
  use common_typedef, only: i4b, r4b, r8b
  integer(kind=i4b), parameter :: max_items = 1000 !! max. of items
  integer(kind=i4b), parameter :: max_vars  = 2    !! max. of vars.
  character(len=nfiln)  :: hitems(max_items,max_vars)
  integer(kind=i4b) :: jfile_i !! I/O unit for input file
end module common_gtitem

!c=====================================================================c

program gtitem
  use common_gtitem
  use rw, only: fopen2, fclose
  use rwgtool, only: seek_iounit
  use error_handler, only: ioerror
  implicit none
  !c+++ [input]
  !c+++ input from getparms
  character(len=nfiln) :: ifile     !! input/output file name
  character(len=nfiln) :: ofile     !! input/output file name
  !c+++ input from file
  character(len=ncc)   :: hitem     !! item
  !c+++ [output]
  !c+++ input from itemld
  character(len=ncc)   :: item_name !! item
  character(len=nfiln) :: file_name !! file name
  !c+++ [work]
  integer(kind=i4b)    :: id        !!
  integer(kind=i4b)    :: nitem     !! number of saved items
  integer(kind=i4b)    :: ios       !! end code
  integer(kind=i4b)    :: jfile_o   !! I/O unit for output file

!c
!c prepare
!c===
  !c+++ read parameters
  call getparms

  !c+++ open input file (SYSIN)
  call osysin(ifile)

  !c+++ get I/O unit numbers for output file
  call seek_iounit(jfile_o, ios)
  if (ios /= 0) call ioerror(jfile_o, ios)
  !c+++ open output file
  call fopen2(jfile_o, ofile, 'unknown', 2, 0, ios)
  if (ios /= 0) call ioerror(jfile_o, ios)

!c
!c main
!c===
  !c+++ read SYSIN file & register items
  call rsysin
  !c+++ close SYSIN file
  call csysin

  !c+++ count saved items
  call itemcn(nitem)
  do id = 1, nitem
    !c+++ load items
    call itemld(id, item_name, file_name)
    if (trim(hitem) == 'LIST') then
      !c+++ write list of items
      write(jfile_o, '(i4, 2(1x, a))') id, item_name, trim(file_name)
    else if (trim(hitem) == trim(item_name)) then
      !c+++ write file name
      write(jfile_o, '(a)') trim(file_name)
    endif
  enddo !! nv
  call fclose(jfile_o)

  stop

contains

!c=====================================================================c

!c----------------------------------------------------------------------c
!c  subroutine osysin
!c  open SYSIN file
!c=====
subroutine osysin(file_name)
  use rwgtool, only: seek_iounit
  use error_handler, only: ioerror
  !c+++ [input]
  character(len=*), intent(in) :: file_name !! SYSIN file name
  !c+++ [internal work]
  integer(kind=i4b)            :: ios       !! error code

  !c+++ get I/O unit numbers for input file
  call seek_iounit(jfile_i, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  !c+++ open SYSIN file
  call fopen2(jfile_i, file_name, 'old', 2, 0, ios)
  if (ios /= 0) call ioerror(jfile_i, ios)

  return
end subroutine osysin

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine csysin
!c  close SYSIN file
!c=====
subroutine csysin
  use rw, only: fclose
  call fclose(jfile_i) 
  return
end subroutine csysin

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine rsysin
!c  read SYSIN file
!c=====
subroutine rsysin
  !c+++ [internal work]
  integer(kind=i4b)    :: ios      !!
  !c+++ nmdata
  character(len=ncc)   :: item     !! name of variables
  character(len=ncc)   :: itemd    !! name of variables
  character(len=nfiln) :: file     !! file name
  real(kind=r8b)       :: defalt   !! default value
  integer(kind=i4b)    :: xsel     !! selected type of X
  integer(kind=i4b)    :: ysel     !! selected type of Y
  integer(kind=i4b)    :: zsel     !! selected type of Z
  integer(kind=i4b)    :: start(6) !! start time
  integer(kind=i4b)    :: end(6)   !! finish time
  character(len=ncc)   :: dfmt     !! output format
  character(len=ncc)   :: iclas    !! output format
  real(kind=r8b)       :: fact     !! factor
  logical              :: pin      !! output at p-level
  integer(kind=i4b)    :: diur     !! interval of diurnal out
  logical              :: ointrp   !! interpolate ?
  logical              :: climat   !! climatology ?
  integer(kind=i4b)    :: intrpt   !! interpolation type

  namelist /nmdata/ item  , itemd , file  , defalt, &
&                   xsel  , ysel  , zsel  , &
&                   start , end   , &
&                   dfmt  , iclas , fact  , &
&                   pin   , diur  , ointrp, intrpt, climat

  !c+++ read
  !ccc do while (.not. eof(jfile_i))
  do while (1 == 1)
    read(jfile_i, nmdata, iostat=ios) !!  read nmhist
    if (ios /= 0) exit
    !c+++ register item
    call itemrg(item, file)
  enddo

  return
end subroutine rsysin

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine itemrg
!c  register items
!c=====
subroutine itemrg(hitem, hfile)
  !c+++ [input]
  character(len=*), intent(in) :: hitem     !! item
  character(len=*), intent(in) :: hfile     !! file name
  !c+++ [internal work]
  integer(kind=i4b)            :: i, nitems !!
  !c+++ [internal save]
  logical, save                :: ofirst = .true.

  !c+++ one time
  if (ofirst) then
    ofirst = .false.
    hitems(:,:) = ' '
  endif

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
    if (trim(hitems(i,1)) == trim(hitem)) exit
    !c+++ register new item
    if (hitems(i,1) == ' ') then
      write(6, '(4a)') 'register new item: ', trim(hitem), ' = ', trim(hfile)
      hitems(i,1) = trim(hitem)
      hitems(i,2) = trim(hfile)
      exit
    endif
  enddo !! i

  return
end subroutine itemrg

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine itemld
!c load items
!c=====
subroutine itemld(id, hitem, hfile)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: id    !! history number
  !c+++ [output]
  character(len=*), intent(out) :: hitem !! item
  character(len=*), intent(out) :: hfile !! file name

  if (id > max_items) then
    write(6, *) 'Error: input number exceeds maximum item limit'
    stop 2
  endif
  hitem = hitems(id, 1)
  hfile = hitems(id, 2)

  return
end subroutine itemld

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine itemcn
!c  count saved items
!c=====
subroutine itemcn(nitem)
  !c+++ [output]
  integer(kind=i4b), intent(out) :: nitem     !! number of saved items
  !c+++ [internal work]
  integer(kind=i4b)              :: i, nitems !!

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
    if (hitems(i,1) == ' ') then
      nitem = nitems - 1
      exit
    endif
  enddo !! i

  return
end subroutine itemcn

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine getparms
!c
!c=====
subroutine getparms
  use uopts, only: read_parms, get_parms
  use char2var, only: c2var
  !c+++ [internal work]
  integer(kind=i4b)             :: ios              !! end code

  !c+++ input parameters
  call read_parms(ios)
  if (ios /= 0) call xabort
  !c+++ input/output file
  call get_parms('i', 'SYSIN', ifile, ios)
  call get_parms('o', 'gtitem.txt', ofile, ios)
  !c+++ new item of output data
  call get_parms('item', 'LIST', hitem, ios)
  if (ios /= 0) call xabort

  return
end subroutine getparms

!c----------------------------------------------------------------------c

subroutine xabort
  write(6, '(a)') 'Usage: '
  write(6, '(a)') 'gtitem -i input-file -o output-file'
  write(6, '(a)') '-item putput_item_name (or LIST)'
  stop 2
end subroutine xabort

!c----------------------------------------------------------------------c

!c=====================================================================c

end program gtitem
