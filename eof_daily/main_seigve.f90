!c
!c  program seigve
!c  2003/08/05 Yamashita: first ver.
!c  2011/06/01 Yamashita: real(8), f77==>f90
!c  2020/10/28 Yamashita: merge subroutines and var-cov.f90
!c  2021/06/08 Yamashita: use common_args
!c  calculate eigen value and eigen vector
!c
!c=====================================================================
program main_seigve
  use common_args, only: NP=>npc, nv, nc
! integer(4)              :: NP   !! number of output PCs
! integer(4)              :: NV   !! number of grids
! integer(4)              :: NC   !! number of time
  integer(4)              :: NA   !! number of spatial grids
  integer(4)              :: NDIM !! maximum dimensition in calc.
  real(8), allocatable    :: A(:,:), E(:), VE(:,:), AV(:,:)
  real(8), allocatable    :: WORK(:,:)
  integer(4), allocatable :: LEX(:)
  integer(4)              :: neig, nls, nvec
  real(8)                 :: eps

  !c+++ input from <stdin>
! read(5, *) NP, NV, NC
  NA = NV
  NDIM = int(NV * 0.01 + 1)
  NDIM = NDIM * 100
  allocate( A(NDIM,NV), E(NV), VE(NDIM,NV), AV(NA,NA) )
  allocate( WORK(NDIM,8), LEX(NV) )

  write(6, *) 'calling subroutine maceps ...'
  call maceps(eps) 
  write(6, *) 'calling subroutine read_egdata ...'
  call read_egdata(NDIM, NV, NA, NC, A, AV)
!ccc  write(6,*) 'CHECK data '
!ccc  do j=1,NV
!ccc    do i=1,NV
!ccc      if ((i.eq.j).or.(i-1.eq.j)) then
!ccc        write(6,*) i,j,A(i,j)
!ccc      end if
!ccc    end do
!ccc  end do
  neig  = NV
  nls = 1
  nvec = NV

  write(6, *) 'calling subroutine eigen ...'
  call eigen(NDIM, NV, A, neig, nls, E, nvec, VE, eps, WORK, LEX) 
  write(6, *) 'calling subroutine write_result ...'
  call write_result(NDIM, NV, neig, E, nvec, VE, NP) 

  deallocate( A, E, VE, AV, WORK, LEX )
  stop

contains

!c=====================================================================

!c---------------------------------------------------------------------
!c
!c  subroutine write_result
!c  2003/08/05 Yamashita: first ver.
!c  2011/06/01 Yamashita: real(8), f77==>f90
!c
!c  print eigenvalues and eigenvectors 
!c
subroutine write_result(NDIM, NV, neig, E, nvec, VE, NP)
  integer(4) :: NDIM, NV, neig, nvec, NP, KS, KL
  real(8)    :: E(neig), VE(NDIM,nvec)
  integer(4) :: inum, j, k

  inum = NV / 20
  if (inum < 20) inum = 20

  write(6, 2001) E(1:NP)
  do j = 1, neig
    write(51, *) j, E(j) !! write eigen value
  enddo
2001 format(/' ---EIGVEC--- '/ 'eigen values EIG(i)' /4(1x,1pe13.6))
  write(6, *) ' eigen vectors VEC(i)'
  do k = 1, NP
    do j = 1, NV
      write(61, *) k, VE(j,k)  !! write eigen vector
    enddo
  enddo

  KS = 1
  KL = 4
  do while (1 == 1)
    do j = 1, NV, inum
      write(6, '(4(1x,1pe13.6))') VE(j,KS:KL)
    enddo
    KS = KL + 1
    KL = KS + 3
    if (KS > NP) return
    if (KL > NP) KL = NP
    write(*, '(1x)') 
  enddo

  return
end subroutine write_result

!c---------------------------------------------------------------------

!c---------------------------------------------------------------------
!c
!c  get machine epsiron in double precision
!c  2003/08/05 Yamashita: first ver.
!c  2011/06/01 Yamashita: real(8), f77==>f90
!c
!c=====
subroutine maceps(epsmac)
  real(8) :: epsmac
  epsmac = 1.d0
1 continue 
  if ((1.d0+epsmac) <= 1.d0) then
    epsmac = 2.d0 * epsmac
    return
  endif
  epsmac = epsmac * 0.5d0
  go to 1
end subroutine maceps

!c---------------------------------------------------------------------

!c---------------------------------------------------------------------
!c
!c  subroutine read_egdata
!c  2003/08/05 Yamashita: first ver.
!c  2011/06/01 Yamashita: real(8), f77==>f90
!c  2020/10/28 Yamashita: merge var-cov.f90 into main_seigve.f90
!c
!c  read input data of eigen
!c
!c=====
subroutine read_egdata(NDIM, NV, NA, nc, A, AV)
  implicit none
  integer(4)           :: NDIM, NV, NA, nc  
  real(8)              :: A(NDIM,NV)
  real(8)              :: AV(NA,NA)
  integer(4)           :: i, j, ic, iv, jv, ip, jp
  integer(4)           :: nlen
  real(8)              :: wi
  real(8), allocatable :: ave(:), SD(:), X(:)
  real(8), allocatable :: cov(:,:), WX(:)
  nlen = NA * 8
  !nlen = NA * NA * 8
  allocate ( ave(NA), SD(NA), X(NA) )
  allocate ( cov(NA,NA), WX(NA) )

  !c+++ input data
  open(10, file='fort.10', status='old', form='unformatted', access='direct', recl=nlen)
  !c+++ initialize
  write(6, *) 'initialize (in BSTAT)'
  read(10, rec=1) X
  do iv = 1, nv !! iv: grid number
    WX(iv) = X(iv) !! X(1,iv)=reference value of anomaly data
    ave(iv) = 0.0
    do jv = 1, nv
      cov(iv,jv) = 0.0
    enddo !! jv
  enddo !! iv

  !c+++ pre-computation for making matrix
  write(6, *) 'pre-computation for making matrix (in BSTAT)'
  do ic = 2, nc !! time series loop
    if ( mod(ic, 50) == 0 ) then
      write(6, '(a,1x,i7,a,i7)') 'IC =', ic, '/', nc
    endif
    read(10, rec=ic) X
    do iv = 1, nv !! grid number loop
      wi = X(iv) - WX(iv)
      ave(iv) = ave(iv) + wi
      do jv = iv, nv
        cov(iv,jv) = cov(iv,jv) + wi * ( X(jv) - WX(jv) )
      enddo !! jv
    enddo !! iv
  enddo !! ic

  do iv = 1, nv
    ave(iv) = ave(iv) / DBLE(nc) + WX(iv)
  enddo !! iv

  !c+++ make variance-covariance matrix
  write(6, *) 'make variance-covariance matrix (in BSTAT)'
  do ip = 1, nv
    wi = WX(ip) - ave(ip)
    do jp = ip, nv
      cov(ip,jp) = cov(ip,jp) / DBLE(nc) - wi * ( WX(jp) - ave(jp) )
      !c+++ copied to lower triangle side of variance-covariance matrix
      cov(jp,ip) = cov(ip,jp)
    enddo !! jp
  enddo !! ip

  write(6,*) 'make triangle matrix'
  AV(:,:) = cov(:,:)
  do i = 1, NV
    do j = 1, i
      A(i,j) = AV(i,j)
    enddo
  enddo
 
  close(10)
  deallocate ( ave, SD, X, cov, WX )

  return
end subroutine read_egdata

!c---------------------------------------------------------------------

!c---------------------------------------------------------------------
!c
!c  subroutine eigen
!c  2003/08/05 Yamashita: first ver.
!c  2011/06/01 Yamashita: real(8), f77==>f90
!c                                                               
!c  solve eigen value problem of N*N (N.gt.2) symmetric matrix A 
!c     A*VE = E*VE                                               
!c    by Householder,bisection and inverse iteration method      
!c                                                               
!c   input NDIM ;declared row dimension of A in calling problem  
!c           NV ;order of matrix A                               
!c         neig ;number of eigenvalues to be computed            
!c          nls ;If nls.ge.0 then neig largest                  
!c               If nls.ge.0 then neig largest                 
!c         nvec ;number of eigenvectors to be computed         
!c               If nvec is more then neig, nvec is set equal to neig  
!c               If nvec=0 then skip computation of eigenvectors      
!c          eps ;relative error tolerance for eigenvalues             
!c                                                                   
!c  output    E,(k=1,neig) ;eigenvalues                             
!c           VE,(k=1,nvec) ;eigenvectors                           
!c                                                                
!c  workarrays  WORK(NDIM,i),i=1,8 ;                              
!c        I  WORK    I  WORK   I  WORK   I  WORK                  
!c        1  AL      2  BE     3  CO     4  W:B2:D1               
!c        5  P:BL    6  Q:BU   7  BV     8  CM                   
!c              LEX ;used in INVITR                              
!c                                                              
!c=====
subroutine eigen(NDIM, NV, A, neig, nls, E, nvec, VE, eps, WORK, LEX)
  integer(4) :: NDIM, NV, nvec, neig, nls
  real(8)    :: A(NDIM,NV), E(neig), VE(NDIM,nvec) 
  real(8)    :: WORK(NDIM,8), eps
  integer(4) :: LEX(NV)

  if (NV <= 2) return

  write(6,*) 'calling subroutine house ...'
  call house(NDIM, NV, A, WORK(1,1), WORK(1,2), WORK(1,3), WORK(1,4), WORK(1,5), WORK(1,6))
  write(6, *) 'calling subroutine bisec ...'
  call bisec(NV, neig, nls, E, eps, WORK(1,1), WORK(1,2), WORK(1,4))

  write(6, *) 'calling subroutine INVITR ...'
  call invitr(NDIM, NV, A, neig, E, nvec, VE, WORK(1,1), WORK(1,2), &
&            WORK(1,3), WORK(1,4), WORK(1,5), WORK(1,6), &
&            WORK(1,7), WORK(1,8), LEX)

  return
end subroutine eigen

!c---------------------------------------------------------------------

!c---------------------------------------------------------------------
!c                                                                    
!c triangularization of symmetric matrix A(N,N) by Householder's method 
!c 2003/08/05 Yamashita: first ver.
!c 2011/06/01 Yamashita: real(8), f77==>f90
!c                                                                     
!c input A ;matrix to be tridiagonarized                               
!c        Element in lower left triangular part ((A(i,j),i=1,NV),j=1,i)
!c        most be given                                             
!c                                                                  
!c output AL(i) ;i-th diagonal element of tridiagonarized matrix     
!c        BE(i) ;i-th subdiagonal element of tridiagonarized matrix  
!c        Location of AL and BE is as follows                        
!c                                                                    
!c        ...BE(i-1) AL(i)......................                     
!c        ...........BE(i) AL(i+1)..............                     
!c        .................BE(i+1) AL(i+2)......                     
!c                                                                   
!c     A(j,k) ;W(j) at the k-th step which will be used in INVITR    
!c      CO(k) ;normalization factor of W which will be used in INVITR 
!c                                                                     
!c=====
subroutine house(NDIM, NV, A, AL, BE, CO, W, P, Q) 
  integer(4) :: NDIM, NV
  real(8)    :: A(NDIM,NV)
  real(8)    :: AL(NV), BE(NV), CO(NV)
  real(8)    :: W(NV), P(NV), Q(NV)
  real(8)    :: T, S
  integer(4) :: i, j, k
      
  if (NV <= 2) return

!c+++ start Householder transformation
  write(6, *) 'hauseholder transformation start'
  do k = 1, NV-2
    if (MOD(k,50) == 0) then
      write(6, '(a, 1x, i7, a, i7)') 'STEP :', k, ' / ', NV-2
    endif
    T = 0.d0
    do i = k+1, NV
      T = T + A(i,k)**2
    enddo !! i

    S = SQRT(T)
    if (A(k+1,k) < 0.0) then
      S = -S
    endif
    AL(k) = A(k,k)
    BE(k) = -S
    if (T == 0.0) then
      write(6, *) 'skip STEP',k
      cycle
    endif

    CO(k) = 1.d0 / (T + A(k+1,k) * S)
    W(k+1) = A(k+1,k) + S
    do i = k+2, NV
      W(i) = A(i,k)
    enddo !! i
    A(k+1,k) = W(k+1)

    do i = k+1, NV
      T = 0.d0
      do j = k+1, i
        T = T + A(i,j) * W(j)
      enddo !! j
      do j = i+1, NV
        T = T + A(j,i) * W(j)
      enddo !! j
      P(i) = CO(k) * T
    enddo !! i 

    T = 0.d0
    do i = k+1, NV
      T = T + P(i) * W(i)
    enddo !! i

    S = 0.5d0 * CO(k) * T
    do i = k+1, NV
      Q(i) = P(i) - S * W(i)
    enddo !! i

    do j = k+1, NV
      do i = j, NV
        A(i,j) = A(i,j) - W(i) * Q(j) - Q(i) * W(j) 
      enddo !! i
    enddo !! j
  enddo !! k

  AL(NV-1) = A(NV-1,NV-1)
  Al(NV) = A(NV,NV)
  BE(NV-1) = A(NV,NV-1)

  return
end subroutine house

!c---------------------------------------------------------------------

!c---------------------------------------------------------------------
!c subroutine bisec
!c 2003/08/05 Yamashita: first ver.
!c 2011/06/01 Yamashita: real(8), f77==>f90
!c                                                               
!c compute eigenvalues of matrix A(NV,NV) by bisection method    
!c                                                                  
!c  before calling this subroutine matrix A must be transformed into
!c  tridiagonal form by calling HOUSE                             
!c                                                                 
!c  input AL(I),BE(I) ;See coment in HOUSE                       
!c                                                               
!c  output E(I),I=1,neig ;eigen values computed                  
!c                                                              
!c  work  B2(1)=0.0 B2(I) = BE(I-1),I=2,3,,,NV                 
!c=====
subroutine bisec(NV, neig, nls, E, eps, AL, BE, B2)
  integer(4) :: NV, neig, nls
  real(8)    :: E(neig)
  real(8)    :: AL(NV), BE(NV), B2(NV), eps
  real(8)    :: range0, range1, epsa, small, A, B, C, G
  integer(4) :: i, k, nneg, ipass
  !c+++ set initial
  BE(NV) = 0.d0
  range0 = ABS(AL(1)) + ABS(BE(1))
  do i = 2, NV
    range1 = ABS(BE(i-1)) + ABS(AL(i)) + ABS(BE(i))
    if (range1 > range0) then
      range0 = range1
    endif
  enddo !! i
  if (nls < 0) then
    range0 = - range0
  endif 

  B2(1) = 0.d0
  do i=2,NV
    B2(i) = BE(i-1)**2
  enddo !! i

  epsa  = range0 * eps
  small = epsa * eps

  do i = 1, neig
    E(i) = - range0
  enddo !! i
  B = range0

!ccc start bisection method 
  write(6, *) 'bisection method start'
  do k = 1, neig
    A = E(k)

10  continue
    C = 0.5d0 * (A + B) 
    if (ABS(B-A) > epsa.and.C /= A.and.C /= B) then
       nneg = 0
       G = 1.d0
       ipass = 0

       do i = 1, NV
         if (ipass == 0) then
           G = C - AL(i) - B2(i) / G
         elseif (ipass == 1) then
           ipass = 2
         else
           G = C - AL(i)
           ipass = 0
         endif

         if (ipass == 0) then
           if (G <= 0.0) then
             nneg = nneg + 1
           endif
           if (ABS(G) <= ABS(B2(i) * small)) then
             ipass = 1
           endif
         endif
       enddo !! i

       if (nls < 0) then 
         nneg = NV - nneg 
       endif

       if (nneg < k) then
         B = C 
       else
         A = C
         do i = k, MIN(nneg, neig)
           E(i) = C
         enddo !! i
       endif
       
       go to 10
    endif
  enddo !! k

  return
end subroutine bisec

!c---------------------------------------------------------------------

!c---------------------------------------------------------------------
!c
!c  subroutine invitr
!c  2003/08/05 Yamashita: first ver.
!c  2011/06/01 Yamashita: real(8), f77==>f90
!c                                                                   
!c  compute eigen vectors of matrix A(NV,NV)                        
!c      by inverse iteration method                                 
!c                                                                  
!c  Before calling this subroutine matrix A must be transformed into
!c  tridiagonal form by calling house (Householder's method) and the
!c  eigen values E(k)i,k=1,nvec must be obtained by calling bisec  
!c  (bisection).                                                  
!c                                                               
!c  input  AL(i) ;i-th diagonal element of tridiagonal matrix    
!c         BE(i) ;i-th subdiagonal element                       
!c         CO(i) ;computed in house                               
!c          E(i),k=1,neig ; eigen values computed by calling bisec   
!c        A(j,k) ;W(j) for inverse transform at the k-th step computed 
!c                in house                         
!c                                                     
!c output  (VE(i,k),i=1,NV) ;k-th eigenvector          
!c                                                     
!c   work  DI(i) ;i-th diagonal E(k) - AL(i)           
!c         BL(i) ;i-th lower subdiagonal               
!c         BU(i) ;i-th upper subdiagonal              
!c         BV(i) ;i-th 2-nd upper subdiagonal         
!c         CM(i) ;i-th multiplier of GAUSS elimination
!c                                                   
!c         location in tridiagonal matrix           
!c               ...........                            
!c       BL(i-1) DI(i)  BU(i)    BV(i)       0        0  
!c          0    BL(i)  DI(i+1)  BU(i+1)  BV(i+1)     0   
!c          0      0    BL(i+1)  DI(i+2)  BU(i+2)  BV(i+2) 
!c               ...........                                
!c        LEX(i) ;integer work array                         
!c             1 ;i-th and i+1-th row exchanged               
!c             0 ;not exchanged                                
!c                                                              
!c=====
subroutine invitr(NDIM, NV, A, neig, E, nvec, VE, AL, BE, CO, DI, BL, BU, BV, CM, LEX)
  integer(4) :: NDIM, NV, neig, nvec
  real(8)    :: A(NDIM,NV), E(neig), VE(NDIM,nvec)
  real(8)    :: AL(NV), BE(NV), CO(NV), epsmac
  real(8)    :: DI(NV), BL(NV), BU(NV), BV(NV), CM(NV)
  integer(4) :: LEX(NV)
  integer(4) :: i, j, k, km, l, lend, irand
  real(8)    :: eps, small, crand, S, T

  if (nvec <= 0) return
  if (nvec > neig) nvec = neig

  call maceps(epsmac)
  eps = epsmac * MAX(ABS(E(1)), ABS(E(nvec)))
  small = epsmac * eps
  crand = epsmac / 1664501

  !c+++ initial setting for random number
  irand = 1

  !c+++  inverse iteration  
  write(6, *) 'inverse iteration start'
  do k = 1, nvec
    if (MOD(k,50) == 0) then
      write(6, '(a, 1x, i7, a, i7)') 'STEP :', k, ' / ', nvec
    endif
    do i = 1, NV
      DI(i) = E(k) -AL(i)
      BL(i) = -BE(i)
      BU(i) = -BE(i)
    enddo !! i
 
    !c+++  LU decomposition by Gauss elimination
    do i = 1, NV-1
      if (ABS(DI(i)) >= ABS(BL(i))) then
        !c+++ -----  row not exchanged ----- 
        LEX(i) = 0
        if (ABS(DI(i)) < small) then
          DI(i) = small
        endif
        CM(i+1) = BL(i) / DI(i)
        DI(i+1) = DI(i+1) - CM(i+1) * BU(i)
        BV(i) = 0.d0
      else
        !c+++ -----  row exchanged ----- 
        LEX(i) = 1
        CM(i+1) = DI(i) / BL(i)
        DI(i) = BL(i) 
        S = BU(i)
        BU(i) = DI(i+1)
        BV(i) = BU(i+1)
        DI(i+1) = S - CM(i+1) * BU(i)
        BU(i+1) =   - CM(i+1) * BV(i)
      endif
    enddo !! i

    if (ABS(DI(NV)) < small) then
      DI(NV) = small 
    endif

    !c+++  genarate initial guess of eigen vector using random numbers 
    do i = 1, NV
      irand = MOD(1229 * irand + 351750, 1664501)
      VE(i,k) = REAL(irand) * crand
    enddo !! i

    if (k == 1) then
      km = k
    else if (ABS(E(k) - E(km)) > eps) then
      !c+++ ----- non-degenerate eigenvalues -----
      km = k
    else
!c      ----- degenerate eigenvalues -----
!c      km-th through k-th eigenvalues are degenerate  
!c      i.e. degeneracy = k - km + 1
!c        orthogonalized vectors
      do i = km, k-1
        T = 0.d0
        do j = 1, NV
          T = T + VE(j,i) * VE(j,k)
        enddo !! j
        S = T
        do j = 1, NV
          VE(j,k) = VE(j,k) - S * VE(j,i)
        enddo !! j
      enddo !! i
    endif

    !c+++ start inverse iteration
    lend = k - km + 2
    if (lend > 5) then
      lend = 5
    endif

    do l = 1, lend
      if (l /= 1.or.k /= km) then
        !c+++ ----- forward substitution -----
        do i = 1, NV-1
          if (LEX(i) == 0) then
            VE(i+1,k) = VE(i+1,k) - CM(i+1) * VE(i,k)
          else
            S = VE(i,k)
            VE(i,k) = VE(i+1,k)
            VE(i+1,k) = S - CM(i+1) * VE(i,k)
          endif
        enddo !! i
      endif
      !c+++ ----- backward substitution -----
      do i = NV, 1, -1
        S = VE(i,k) 
        if (i <= NV-1) then
          S = S - BU(i) * VE(i+1,k)
        endif
        if (i <= NV-2) then
          S = S - BV(i) * VE(i+2,k)
        endif
        VE(i,k) = S / DI(i)
      enddo !! i
      !c+++ ----- normalize vector to avoid overflow -----
      T = 0.d0
      do j = 1, NV
        T = T + VE(j,k)**2
      enddo !! j
      S = 0.d0
      if (T /= 0.0) then
        S = SQRT(1.d0 / T) 
      endif
      do j = 1, NV
        VE(j,k) = VE(j,k) * S
      enddo !! j
    enddo !! l
  enddo ! k 
!c 
!c transformation of eigenvectors to original space
!c
  write(6, *) 'transformation to original space'
  do k = 1, nvec
    if (MOD(k, 50) == 0) then
      write(6, '(a, 1x, i7, a, i7)') 'STEP :', k, ' / ', nvec
    endif
    do i = NV-2, 1, -1
      T = 0.d0
      do j = i+1, NV
        T = T + A(j,i) * VE(j,k)
      enddo !! j
      S = T * CO(i)
      do j = i+1, NV
        VE(j,k) = VE(j,k) - S * A(j,i)
      enddo !! j
    enddo !! i
  enddo !! k
!c
!c orthogonalize eigen vectors if degenarate
!c
  write(6,*) 'orthogonalize eigen vector'
  km = 1 
  do k = 2, nvec
    if (ABS(E(k) - E(km)) >= eps) then
      !c+++ ----- non-degenerate eigenvalues-----
      km = k
    else
      !c+++ ----- km-th through k-th eigenvalues are degenarate -----
      do i = km, k-1
        T = 0.d0
        do j = 1, NV
          T = T + VE(j,i) * VE(j,k)
        enddo !! j
        S = T
        do j=1,NV
          VE(j,k) = VE(j,k) - S * VE(j,i)
        enddo !! j
      enddo !! i
      !c+++ ----- normalize eigen vector -----
      T = 0.d0
      do j = 1, NV
        T = T + VE(j,k)**2
      enddo !! j
      S = SQRT(1.d0 / T)
      do j = 1, NV
        VE(j,k) = VE(j,k) * S
      enddo !! j
    endif
  enddo !! k

  return
end subroutine invitr

!c---------------------------------------------------------------------

end program main_seigve
