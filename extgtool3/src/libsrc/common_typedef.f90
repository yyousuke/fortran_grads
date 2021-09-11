!c
!c  module common_typedef
!c  [history]
!c  2015/06/01 Yamashita: first ver.
!c  2017/05/01 Yamashita: typedef_common to common_typedef
!c
!c======================================================================c
module common_typedef
  implicit none 
  private 

  integer(kind=4), parameter, public :: & 
    !c+++ Byte = selected_int_kind(有効桁数)
&   byte   = selected_int_kind(2),   & !" 1 Byte 
&   short  = selected_int_kind(4),   & !" 2 Byte
&   long   = selected_int_kind(9),   & !" 4 Byte
&   llong  = selected_int_kind(18),  & !" 8 Byte
    !c+++ Byte = selected_real_kind(有効桁数)
&   float  = selected_real_kind(6),  & !" 4 Byte
&   double = selected_real_kind(15), & !" 8 Byte
&   ldble  = selected_real_kind(33)    !" 16 Byte

  !c+++ alias
  integer(kind=4), parameter, public :: & 
& i1b = byte, &
& i2b = short, &
& i4b = long, &
& i8b = llong, &
& r4b = float, &
& r8b = double, &
& r16 = ldble, &
& c8b = double

end module common_typedef
