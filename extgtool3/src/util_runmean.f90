!c
!c  module util_runmean
!c  [history]
!c  2016/11/22 Yamashita: first ver. 
!c  2016/12/08 Yamashita: bug fix of runmean_boxcar
!c  2017/07/28 Yamashita: use common_typedef
!c
!c  public: runmean_main: main routine of running mean
!c    internal: readopt
!c    internal: runmean_fourier: fourier transform
!c      internal: detrend
!c        internal: avesd
!c        internal: covariance
!c        internal: reg
!c      internal: frtf forward transform
!c      internal: frtb backward transform
!c      internal: nnf non-negative filter
!c
!c======================================================================c
module util_runmean
  use common_typedef, only: i4b, r4b, r8b, c8b
  implicit none
  private
  public :: runmean_main

contains

!c======================================================================c

!c----------------------------------------------------------------------c
!c  subroutine runmean_fourier
!c  fourier transform
!c=====
subroutine runmean_main(nx, ny, nz, N, rmiss, dio, tlen, tavr, mval, mtype)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz        !! z-, y-, z-sizes
  integer(kind=i4b), intent(in) :: N                 !! t-size 
  real(kind=r4b), intent(in)    :: rmiss             !! missing values
  real(kind=r4b), intent(in)    :: tlen              !! time length (s)
  real(kind=r4b), intent(in)    :: tavr              !! time ave. of fourier (s)
  real(kind=r4b), intent(in)    :: mval              !! minimum-value (potype=mvf)
  character(len=*), intent(in)  :: mtype             !! type of running mean
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: dio(nx,ny,nz,N)   !! input/outnput data
  !c+++ [internal work]
  character(len=16)             :: hopts(4)          !! options
  character(len=3)              :: rmtype            !! type of running mean
  character(len=3)              :: wftype            !! type of window function
  character(len=3)              :: prtype            !! type of pre processing
  character(len=3)              :: potype            !! type of post processing

  !c+++ default values
  rmtype = 'frt'
  wftype = 'ham'
  prtype = ' '
  potype = ' '
  !c+++ convert options
  call readopt(4, '_', mtype, hopts)
  !c+++ type of running mean
  if (hopts(1) /= ' ') rmtype = trim(hopts(1))
  !c+++ type of window function
  if (hopts(2) /= ' ') wftype = trim(hopts(2))
  !c+++ type of pre
  if (hopts(3) /= ' ') prtype = trim(hopts(3))
  !c+++ type of post
  if (hopts(4) /= ' ') potype = trim(hopts(4))
  write(6, '(2a)') 'runmean_main: type of running mean = ', rmtype
  write(6, '(2a)') 'runmean_main: type of window function = ', wftype
  write(6, '(2a)') 'runmean_main: type of pre processing =  ', prtype
  write(6, '(2a)') 'runmean_main: type of post processing =  ', potype

  !c+++ running mean
  if (rmtype == 'frt') then
    call runmean_fourier(nx, ny, nz, N, rmiss, dio, tlen, tavr, mval, wftype, prtype, potype)
  else if (rmtype == 'box') then
    call runmean_boxcar(nx, ny, nz, N, rmiss, dio, tlen, tavr, mval, potype)
  endif

  return
end subroutine runmean_main
                                                                       
!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine runmean_boxcar
!c  boxcar filter
!c=====
subroutine runmean_boxcar(nx, ny, nz, N, rmiss, dio, tlen, tavr, mval, potype)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz        !! z-, y-, z-sizes
  integer(kind=i4b), intent(in) :: N                 !! t-size 
  real(kind=r4b), intent(in)    :: rmiss             !! missing values
  real(kind=r4b), intent(in)    :: tlen              !! time length (s)
  real(kind=r4b), intent(in)    :: tavr              !! time ave. of fourier (s)
  real(kind=r4b), intent(in)    :: mval              !! minimum-value (potype=mvf)
  character(len=*), intent(in)  :: potype            !! type of post processing
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: dio(nx,ny,nz,N)   !! input/outnput data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k
  integer(kind=i4b)             :: ij, id
  integer(kind=i4b)             :: ijkdim            !! array size
  !c+++ for boxcar
  integer(kind=i4b)             :: nrec              !! number of record for average
  real(kind=r8b), allocatable   :: U(:)              !! data
  !c+++ data
  real(kind=r4b), allocatable   :: d(:,:)            !! data for calc.
  logical, allocatable          :: omain(:)          !! mask
!c
!c prepare
!c===
  ijkdim = nx * ny * nz
  !c+++ allocate
  allocate (U(N))
  allocate(d(ijkdim,N), omain(ijkdim))
  nrec = int(tavr / dble(tlen) * dble(N))
  write(6, '(a, 1pe15.7, a)') 'runmean_boxcar: time length = ', tlen, ' (s)'
  write(6, '(a, 1pe15.7, a)') 'runmean_boxcar: time average = ', tavr, ' (s)'
  write(6, '(a, i7)') 'runmean_boxcar: record length = ', N
  write(6, '(a, i7)') 'runmean_boxcar: mean record length = ', nrec

  !c+++ set input data
  d(1:ijkdim,1:N) = 0.d0
  do id = 1, N
    ij = 0
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          ij = ij + 1
          if (dio(i,j,k,id) /= rmiss) then
            d(ij,id) = dio(i,j,k,id)
          else
            d(ij,id) = rmiss
          endif
        enddo !! i
      enddo !! j
    enddo !!k
  enddo !! id

  !c+++ check
  omain(1:ijkdim) = .true.
  do ij = 1, ijkdim
    do id = 1, N
      if (d(ij,id) == rmiss) then
        omain(ij) = .false.
        exit
      endif 
    enddo !! id
  enddo !! ij

!c
!c main
!c===
  do ij = 1, ijkdim
    if (omain(ij)) then
      if (ijkdim >= 1000.and.mod(ij, 1000) == 0) then
        write(6, '(2(a, i7), a)') 'runmean_boxcar: (IJ =', ij, '/', ijkdim, ')'
      endif
      !c+++ set input data
      U(1:N) = d(ij,1:N)
  
      !c+++ boxcar filter
      call boxcar(N, nrec, U)

      !c+++ non-negative filter
      if (trim(potype) == 'nnf') then
        call nnf(N, U)
      else if (trim(potype) == 'mvf') then
        call minvf(N, mval, U)
      endif

      !c+++ set output data
      d(ij,1:N) = U(1:N)
    else
      d(ij,1:N) = rmiss
    endif ! omain
  enddo !! ij

!c
!c final
!c===
  !c+++ set output data
  do id = 1, N
    ij = 0
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          ij = ij + 1
          if (d(ij,id) /= rmiss) then
            dio(i,j,k,id) = d(ij,id)
          else
            dio(i,j,k,id) = rmiss
          endif
        enddo !! i
      enddo !! j
    enddo !!k
  enddo !! id
  !c+++ deallocate
  deallocate (U, d, omain)

  return
end subroutine runmean_boxcar
                                                                       
!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine runmean_fourier
!c  fourier transform
!c=====
subroutine runmean_fourier(nx, ny, nz, N, rmiss, dio, tlen, tavr, mval, wftype, prtype, potype)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nx, ny, nz        !! z-, y-, z-sizes
  integer(kind=i4b), intent(in) :: N                 !! t-size 
  real(kind=r4b), intent(in)    :: rmiss             !! missing values
  real(kind=r4b), intent(in)    :: tlen              !! time length (s)
  real(kind=r4b), intent(in)    :: tavr              !! time ave. of fourier (s)
  real(kind=r4b), intent(in)    :: mval              !! minimum-value (potype=mvf)
  character(len=*), intent(in)  :: wftype            !! type of window function
  character(len=*), intent(in)  :: prtype            !! type of pre processing
  character(len=*), intent(in)  :: potype            !! type of post processing
  !c+++ [modify]
  real(kind=r4b), intent(inout) :: dio(nx,ny,nz,N)   !! input/outnput data
  !c+++ [internal work]
  integer(kind=i4b)             :: i, j, k           !!
  integer(kind=i4b)             :: ij, id            !!
  integer(kind=i4b)             :: ijkdim            !! array size
  integer(kind=i4b)             :: iws, iwe          !! start/end wavenumber
  integer(kind=i4b)             :: isn, ien          !! min./max. lim. of wavenumber
  !c+++ coef
  real(kind=r8b), allocatable   :: T(:)              !! data before fourier
  real(kind=r8b), allocatable   :: U(:)              !! data after fourier
  complex(8), allocatable       :: W(:)              !! Fourier coef.
  !c+++ data
  real(kind=r4b), allocatable   :: d(:,:)            !! data for calc.
  logical, allocatable          :: omain(:)          !! mask
  !c+++ 
  real(kind=r8b), allocatable   :: ww(:)             !! window function
  real(kind=r8b), allocatable   :: d1(:)             !! time index
  real(kind=r8b), allocatable   :: trend(:)          !! trend + avr
  real(kind=r8b)                :: pi
  !c+++ [internal switch]
  logical                       :: ortrend = .false. !! remove trend
  logical                       :: orave   = .false. !! remove average

!c
!c prepare
!c===
  ijkdim = nx * ny * nz
  pi = 4.d0 * datan(1.d0)

  if (trim(prtype) == 'rta'.or.trim(prtype) == 'rat') then
     ortrend = .true.
     orave   = .true.
  else if (trim(prtype) == 'rav') then
     orave   = .true.
  else if (trim(prtype) == 'rtr') then
     ortrend = .true.
  endif
  !ccc write(6, '(2a)') 'runmean_fourier: type of window function = ', wftype
  !ccc write(6, '(2a)') 'runmean_fourier: type of pre processing =  ', prtype
  !ccc write(6, '(2a)') 'runmean_fourier: type of post processing =  ', potype

  !c+++ wave number range
  iws = 0
  iwe = int(tlen / tavr)
  isn = 0
  ien = iwe
  write(6, '(a, 1pe15.7, a)') 'runmean_fourier: time length = ', tlen, ' (s)'
  write(6, '(a, 1pe15.7, a)') 'runmean_fourier: time average = ', tavr, ' (s)'
  write(6, '(a, i7)') 'runmean_fourier: record length = ', N
  write(6, '(a, i7)') 'runmean_fourier: wavenumber limit = ', iwe
  !c+++ allocate
  allocate (T(N), U(N), W(0:N/2))
  allocate(d(ijkdim,N), omain(ijkdim))
  allocate (ww(N), d1(N), trend(N))

  !c+++ window function
  if (wftype == 'ham') then
    !c+++ Hamming Window
    do id = 1, N
      ww(id) = 0.54d0 - 0.46d0 * cos(2.d0 * pi * (id - 1) / dble(N - 1))
    enddo !! id
  else if (wftype == 'han') then
    !c+++ hann Window
    do id = 1, N
      ww(id) = 0.5d0 - 0.5d0 * cos(2.d0 * pi * (id - 1) / dble(N - 1))
    enddo !! id
  else if (wftype == 'rec') then
    ww(1:N) = 1.d0 ! rectangular window
  else
    ww(1:N) = 1.d0 ! rectangular window
  endif
  !c+++ time index
  do id = 1, N
    d1(id) = dble(id - 1) / dble(N - 1) * dble(tlen)
  enddo !! id

  !c+++ set input data
  d(1:ijkdim,1:N) = 0.d0
  do id = 1, N
    ij = 0
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          ij = ij + 1
          if (dio(i,j,k,id) /= rmiss) then
            d(ij,id) = dio(i,j,k,id)
          else
            d(ij,id) = rmiss
          endif
        enddo !! i
      enddo !! j
    enddo !!k
  enddo !! id

  !c+++ check
  omain(1:ijkdim) = .true.
  do ij = 1, ijkdim
    do id = 1, N
      if (d(ij,id) == rmiss) then
        omain(ij) = .false.
        exit
      endif 
    enddo !! id
  enddo !! ij

!c
!c main
!c===
  !c+++ fourier
  do ij = 1, ijkdim
    if (omain(ij)) then
      if (ijkdim >= 100.and.mod(ij, 100) == 0) then
        write(6, '(2(a, i7), a)') 'runmean_fourier: (IJ =', ij, '/', ijkdim, ')'
      endif
      !c+++ set input data with filter
      T(1:N) = d(ij,1:N)
      if (orave.or.ortrend) then
        call detrend(N, d1, T(1:N), T(1:N), trend, ortrend, orave) !! remove trend + avr
      else
        trend(1:N) = 0.d0
      endif
      T(1:N) = T(1:N) * ww(1:N) !! window function
  
      !c+++ FORWARD TRANSFORM T(i,j) => W(k,j)
      call frtf(N, T, W, ien)

      !c+++  BACKWARD TRANSFORM W(k,i) => U(it,i)
      call frtb(N, W, U, iws, iwe)

      !c+++ non-negative filter
      if (trim(potype) == 'nnf') then
        call nnf(N, U)
      else if (trim(potype) == 'mvf') then
        call minvf(N, mval, U)
      endif
      !c+++ set output data with filter
      d(ij,1:N) = U(1:N) / ww(1:N) + trend(1:N)
    else
      d(ij,1:N) = rmiss
    endif ! omain
  enddo !! ij
  
!c
!c final
!c===
  !c+++ set output data
  do id = 1, N
    ij = 0
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          ij = ij + 1
          if (d(ij,id) /= rmiss) then
            dio(i,j,k,id) = d(ij,id)
          else
            dio(i,j,k,id) = rmiss
          endif
        enddo !! i
      enddo !! j
    enddo !!k
  enddo !! id
  !c+++ deallocate
  deallocate (T, U, W, d, omain, ww, d1, trend)

  return
end subroutine runmean_fourier
                                                                       
!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine readopt
!c=====

subroutine readopt(nvar, c, hdump, hout)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nvar       !! number of options
  character(len=1), intent(in)  :: c          !! separator
  character(len=*), intent(in)  :: hdump      !! input option data
  !c+++ [output]
  character(len=*), intent(out) :: hout(nvar) !! divided options
  !c+++ [internal work]
  !c+++ loop variable
  integer(kind=i4b)             :: ivar       !!
  !c+++ other variables
  integer(kind=i4b)             :: ncs, ncol  !! column number

  hout(1:nvar) = ' '
  ncs = 1
  do ivar = 1, nvar
    ncol = scan(hdump(ncs:len_trim(hdump)), c)
    if (ncol == 0) then
      hout(ivar) = hdump(ncs:len_trim(hdump))
      exit
    endif
    hout(ivar) = hdump(ncs:ncs+ncol-2)
    ncs = ncs + ncol
  enddo !! ivar

  return
end subroutine readopt

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine frtf
!c  forward transform
!c=====
subroutine frtf(N, T, W, ien)
  !c+++ [input]
  integer(kind=i4b), intent(in)    :: N, ien          !!
  real(kind=r8b), intent(in)       :: T(N)            !!
  !c+++ [output]
  complex(kind=c8b), intent(inout) :: W(0:N/2)        !!
  !c+++ [internal work]
  integer(kind=i4b)                :: k, m            !!
  real(kind=r8b)                   :: X, pi           !!
  complex(kind=c8b)                :: IMG = (0.0,1.0) !!
  pi = 4.d0 * datan(1.d0)

  do k = 0, ien !! caution: ONLY wavenumber 0 to ien is calculated
    W(k) = (0.0,0.0)
    do m = 1, N
      X = 2.d0 * pi * (m - 1) / dble(N)
      W(k) = W(k) + T(m) * cdexp(-IMG * k * X)
    enddo !! m
    W(k) = W(k) / dble(N)
  enddo !! k

  return 
end subroutine frtf

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine frtb
!c  backward transform
!c=====
subroutine frtb(N, W, U, iws, iwe)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: N, iws, iwe     !!
  complex(kind=c8b), intent(in) :: W(0:N/2)        !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: U(N)            !!
  !c+++ [internal work]
  integer(kind=i4b)             :: k, m            !!
  integer(kind=i4b)             :: iws2            !!
  real(kind=r8b)                :: X, pi           !!
  complex(kind=c8b)             :: IMG = (0.0,1.0) !!
  pi = 4.d0 * datan(1.d0)

  if (iws == 0) then
    U(1:N) = dreal(W(0))
    iws2 = iws + 1
  else
    U(m) = 0.d0
    iws2 = iws
  endif
  do m = 1, N
    do k = iws2, iwe
      X = 2.d0 * pi * (m - 1) / dble(N)
      U(m) = U(m) + 2.d0 * dreal(W(k) * cdexp(IMG * k * X))
    enddo !! k
  enddo !! m

  return 
end subroutine frtb

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine nnf
!c  non-negative filter
!c=====
subroutine nnf(N, U)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: N        !! t-size 
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: U(N)     !!
  !c+++ [internal work]
  integer(kind=i4b)             :: m        !!
  real(kind=r8b)                :: dU(N)    !!
  real(kind=r8b)                :: plus(N)  !! plus: 1, minus: 0
  real(kind=r8b)                :: Up       !! plus
  real(kind=r8b)                :: Um       !! minus
  real(kind=r8b)                :: ratio    !! 0-1

  !ccc dU(1:N) = min(0.d0, U(1:N))
  !ccc U(1:N) = max(0.d0, U(1:N))
  do m = 1, N
    plus(m) = 0.5d0 + sign(0.5d0, U(m))
  enddo !! m

  Up = 0.d0
  Um = 0.d0
  do m = 1, N
    Up = Up + U(m) * plus(m)
    Um = Um - U(m) * (1.d0 - plus(m))
  enddo !! m
  ratio = Um / max(Up, 1d-10) &
&       * (0.5d0 + sign(0.5d0, Up - Um)) &
&       * (0.5d0 + sign(0.5d0, Up - 1d-10))

  dU(1:N) = 0.d0
  do m = 1, N
    dU(m) = - U(m) * (1.d0 - plus(m) + ratio * plus(m))
  enddo !! m

  !c+++ new values
  do m = 1, N
    U(m) = U(m) + dU(m)
  enddo !! m

  return 
end subroutine nnf

!c---------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine minvf
!c  minimum-value filter
!c=====
subroutine minvf(N, mval, U)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: N        !! t-size 
  real(kind=r4b), intent(in)    :: mval     !! minimum value
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: U(N)     !!
  !c+++ [internal work]
  integer(kind=i4b)             :: m        !!
  real(kind=r8b)                :: plus(N)  !! plus: 1, minus: 0

  !c+++ filt
  do m = 1, N
    plus(m) = 0.5d0 + sign(0.5d0, (U(m) - mval))
  enddo !! m
  !c+++ new values
  do m = 1, N
    U(m) = U(m) * plus(m)
  enddo !! m

  return 
end subroutine minvf

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine boxcar
!c  boxcar filter
!c=====
subroutine boxcar(N, nrec, U)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: N        !! t-size 
  integer(kind=i4b), intent(in) :: nrec     !! number of record for average
  !c+++ [modify]
  real(kind=r8b), intent(inout) :: U(N)     !!
  !c+++ [internal work]
  integer(kind=i4b)             :: it, m    !!
  integer(kind=i4b)             :: irs, ire !! record number
  real(kind=r8b)                :: w(N)     !!

  w(1:N) = 0.d0
  do it = 1, N
    irs = max(it - nrec / 2, 1)
    ire = min(it + nrec / 2, N)
    do m = irs, ire
      w(it) = w(it) + U(m)
    enddo !! m
    w(it) = w(it) / dble(ire - irs + 1)
  enddo !! it

  !c+++ set output values
  U(1:N) = w(1:N)

  return 
end subroutine boxcar

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine detrend
!c  remove trend and aevrage components
!c=====
subroutine detrend(nt, tim, din, dout, trend, ortrend, orave)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nt
  real(kind=r8b), intent(in)    :: tim(nt)    !! input time index 
  real(kind=r8b), intent(in)    :: din(nt)    !! input data
  logical, intent(in)           :: ortrend    !! remove trend
  logical, intent(in)           :: orave      !! remove average
  !c+++ [output]
  real(kind=r8b), intent(out)   :: dout(nt)   !! data
  real(kind=r8b), intent(out)   :: trend(nt)  !! ave + trend
  !c+++ [internal work]
  real(kind=r8b)                :: avr1, avr2 !! avr(X), avr(Y)
  real(kind=r8b)                :: sd1, sd2   !! sd(X), sd(Y)
  real(kind=r8b)                :: cov        !! cov(X,Y)
  real(kind=r8b)                :: r, a, b    !! correlation & regression coefficient
  real(kind=r8b)                :: d1(nt)     !! time index
  real(kind=r8b)                :: d2(nt)     !! data
  real(kind=r8b)                :: w(nt)      !!
  real(kind=r8b)                :: tr(nt)     !!

  !c+++ set input data
  d1(1:nt) = tim(1:nt)
  d2(1:nt) = din(1:nt)

  !c+++ avr(X), sd(X)
  call avesd(nt, d1, avr1, sd1)
  !c+++ avr(Y), sd(Y)
  call avesd(nt, d2, avr2, sd2)
  !c+++ cov(X, Y)
  call covariance(nt, d1, d2, avr1, avr2, cov)
  !c+++ correlation coefficient & regression coefficient
  call reg(avr1, avr2, sd1, sd2, cov, r, a, b)

  !c+++ remove trend & average component
  if (ortrend .and. orave) then
    tr(1:nt) = a * (d1(1:nt) - avr1) + avr2
  else if (ortrend) then
    tr(1:nt) = a * (d1(1:nt) - avr1)
  else if (orave) then
    tr(1:nt) = avr2
  else
    tr(1:nt) = 0.0
  endif
  w(1:nt) = d2(1:nt) - tr(1:nt)
  !ccc w(1:nt) = d2(1:nt) - a * (d1(1:nt) - d1(1))
  
  !c+++ set output data
  dout(1:nt) = w(1:nt)
  trend(1:nt) = tr(1:nt)

  return
end subroutine detrend

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine avesd
!c=====
subroutine avesd(nt, d, avr, sd)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nt      !!
  real(kind=r8b), intent(in)    :: d(nt)   !!
  !c+++ [output]
  real(kind=r8b), intent(out)   :: avr, sd !! avr(d), sd(d)
  !c+++ [work]
  integer(kind=i4b)             :: it      !!

!c
!c average & standard deviation
!c===
  avr = 0.d0
  sd = 0.d0
  do it = 1, nt
    avr = avr + d(it)
    sd = sd + d(it)**2
  enddo !! i
  !c+++ avr(X), avr(Y)
  avr = avr / dble(nt)
  !c+++ sd(X), sd(Y)
  sd = dsqrt(sd / dble(nt) - avr**2)

  return
end subroutine avesd

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine covariance
!c=====
subroutine covariance(nt, d1, d2, avr1, avr2, cov)
  !c+++ [input]
  integer(kind=i4b), intent(in) :: nt             !!
  real(kind=r8b), intent(in)    :: d1(nt), d2(nt) !! X, Y
  real(kind=r8b), intent(in)    :: avr1, avr2     !! avr(X), avr(Y)
  !c+++ [output]
  real(kind=r8b), intent(out)   :: cov            !! cov(X, Y)
  !c+++ [internal work]
  integer(kind=i4b)             :: it             !!

!c
!c covariance
!c===
  !c+++ sum (X * Y) 
  cov = 0.d0
  do it = 1, nt
    cov = cov + d1(it) * d2(it)
  enddo !! it
  !c+++ cov(X,Y)
  cov = cov / dble(nt) - avr1 * avr2

  return
end subroutine covariance

!c----------------------------------------------------------------------c

!c----------------------------------------------------------------------c
!c  subroutine reg
!c=====
subroutine reg(avr1, avr2, sd1, sd2, cov, r, a, b)
  !c+++ [input]
  real(kind=r8b), intent(in)  :: avr1, avr2  !! avr(X), avr(Y)
  real(kind=r8b), intent(in)  :: sd1, sd2    !! sd(X), sd(Y)
  real(kind=r8b), intent(in)  :: cov         !! cov(X,Y)
  !c+++ [output]
  real(kind=r8b), intent(out) :: r, a, b     !! correlation & regression coefficient

!c
!c correlation coefficient & regression coefficient
!c===
  !c+++ a = cov(X,Y) / var(X)
  a = cov / sd1**2
  !c+++ b = avr(Y) - a * avr(X)
  b = avr2 - a * avr1
  !c+++ r = cov(X,Y) / (sd(X) * sd(Y))
  r = cov / (sd1 * sd2)

  return
end subroutine reg

!c----------------------------------------------------------------------c

!c======================================================================c

end module util_runmean
