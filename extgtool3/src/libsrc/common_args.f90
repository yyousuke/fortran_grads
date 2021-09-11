!c
!c  module common_args
!c  [history]
!c  2017/04/28 Yamashita: first ver.: ioerror: error handler (2013/03/09 ver.)
!c  2017/05/02 Yamashita: use common_typedef
!c  2017/07/14 Yamashita: ndc
!c
!c=====================================================================c
module common_args
  use common_typedef, only: i4b
  integer(kind=i4b), public, parameter :: ncc = 16    !! number of character in gtool3 header
  integer(kind=i4b), public, parameter :: ndc = 64    !! number of gtool3 header array
  integer(kind=i4b), public, parameter :: nfiln = 200 !! number of file name length
end module common_args
