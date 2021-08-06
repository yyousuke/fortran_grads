!c
!c  subroutine convend
!c  [history]
!c  2010/11/19 Yamashita
!c
!c=====
subroutine convend(outbuf, inbuf, bufsize)
  implicit none
  integer(4), intent(in) :: bufsize
  character(1), intent(in) :: inbuf(bufsize)
  character(1), intent(out) :: outbuf(bufsize)
  !c+++ work
  integer(4) :: i

  do i = 1, bufsize
    outbuf(i) = inbuf(bufsize-i+1)
  enddo

  return
end subroutine convend
