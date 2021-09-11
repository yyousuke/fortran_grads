!c
!c  module fourier
!c  [history]
!c  2013/03/09 Yamashita: first ver.
!c  2017/06/05 Yamashita: use common_typedef
!c  fourier trasform
!c
!c  public:
!c  subroutine fc_fwd: forward
!c  subroutine fc_bwd: backward
!c======================================================================c
module fourier
  use common_typedef, only: i4b, r8b, c8b
  implicit none
  private
  public :: fc_fwd, fc_bwd

contains

!c======================================================================c

!c---------------------------------------------------------------------c
!c  subroutine fc_fwd
!c  forward transform
!c=====
subroutine fc_fwd(N, T, W, undef, isn, ien)
  implicit none
  !c+++ [input]
  integer(kind=i4b), intent(in)    :: N         !! total wavenumber
  integer(kind=i4b), intent(in)    :: isn, ien  !! wavenumber range
  real(kind=r8b), intent(in)       :: T(N), undef
  !c+++ [output]
  complex(kind=c8b), intent(inout) :: W(0:N/2)
  !c+++ [internal work]
  integer(kind=i4b)                :: k, m, num
  real(kind=r8b)                   :: X, pi
  complex(kind=c8b)                :: IMG = (0.0,1.0)
  pi = 4.d0 * datan(1.d0)

  W(0:N/2) = (0.0,0.0)
  do k = isn, ien !! caution: ONLY wavenumber isn to ien is calculated
    num = 0
    do m = 1, N
      if (T(m) /= undef) then
        X = 2.d0 * pi * (m - 1) / dble(N)
        W(k) = W(k) + T(m) * cdexp(-IMG * k * X)
        num = num + 1
      endif
    enddo !! m
    if (num == 0) then
      W(k) = 0.d0
    else
      W(k) = W(k) / dble(num)
    endif
  enddo !! k

  return 
end subroutine fc_fwd

!c---------------------------------------------------------------------c

!c---------------------------------------------------------------------c
!c  subroutine fc_bwd
!c
!c=====
subroutine fc_bwd(N, W, U, isn, ien)
  implicit none
  !c+++ [input]
  integer(kind=i4b), intent(in) :: N         !! total wavenumber
  integer(kind=i4b), intent(in) :: isn, ien  !! wavenumber range
  complex(kind=c8b), intent(in) :: W(0:N/2)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: U(N)
  !c+++ [internal work]
  integer(kind=i4b)             :: k, m
  real(kind=r8b)                :: X, pi
  complex(kind=c8b)             :: IMG = (0.0,1.0)
  pi = 4.d0 * datan(1.d0)

  if (isn == 0) then
    U(1:N) = dreal(W(0))
  else
    do m = 1, N
      U(m) = 0.d0
      do k = isn, ien !! caution: ONLY wavenumber isn to ien is calculated
        X = 2.d0 * pi * (m - 1) / dble(N)
        U(m) = U(m) + 2.d0 * dreal(W(k) * cdexp(IMG * k * X))
      enddo !! k
    enddo !! m
  endif

  return 
end subroutine fc_bwd

!c---------------------------------------------------------------------c

!c======================================================================c

end module fourier
