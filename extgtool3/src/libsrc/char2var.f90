!c
!c  module char2var
!c  [history]
!c  2015/06/01 Yamashita: first ver.
!c
!c  call c2var(var, cfmt, c)
!c  convert c to var with cfmt
!c=====================================================================c
module char2var
  use common_typedef, only: byte, short, long, llong, &
&                    float, double, ldble
  implicit none        
  private             

  !c+++ call c2var(var, cfmt, c)
  interface c2var
    module procedure &
&     c2var_byte, c2var_short, c2var_long, c2var_llong, &
&     c2var_float, c2var_double, c2var_ldble, c2var_bool
  end interface

  public :: c2var

  contains              

!c=====================================================================c

!c---------------------------------------------------------------------c
!c  subroutine c2var_byte
!c  convert character to byte
!c=====
subroutine c2var_byte(var, cfmt, c)
  implicit none 

  !c+++ [output]
  integer(byte), intent(out)  :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_byte

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine c2var_short
!c  convert character to short
!c=====
subroutine c2var_short(var, cfmt, c)
  implicit none 

  !c+++ [output]
  integer(short), intent(out) :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_short

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine c2var_long
!c  convert character to long
!c=====
subroutine c2var_long(var, cfmt, c)
  implicit none 

  !c+++ [output]
  integer(long), intent(out)  :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_long

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine c2var_llong
!c  convert character to llong
!c=====
subroutine c2var_llong(var, cfmt, c)
  implicit none 

  !c+++ [output]
  integer(llong), intent(out) :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_llong

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine c2var_float
!c  convert character to float
!c=====
subroutine c2var_float(var, cfmt, c)
  implicit none 

  !c+++ [output]
  real(float), intent(out)    :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_float

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine c2var_double
!c  convert character to double
!c=====
subroutine c2var_double(var, cfmt, c)
  implicit none 

  !c+++ [output]
  real(double), intent(out)   :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_double

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine c2var_ldble
!c  convert character to ldble
!c=====
subroutine c2var_ldble(var, cfmt, c)
  implicit none 

  !c+++ [output]
  real(ldble), intent(out)    :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_ldble

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine c2var_bool
!c  convert character to bool
!c=====
subroutine c2var_bool(var, cfmt, c)
  implicit none 

  !c+++ [output]
  logical, intent(out)        :: var   !"
  !c+++ [input]
  character(len=*)            :: c     !" input char
  character(len=*)            :: cfmt  !" format

  read(c, cfmt) var

end subroutine c2var_bool

!c---------------------------------------------------------------------c

!c=====================================================================c

end module char2var
