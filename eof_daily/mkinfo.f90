!c
!c  program mkinfo
!c  [history]
!c  2003/08/05 Yamashita: 04e-ev2score.csh
!c  2021/06/07 Yamashita: mkinfo.f90 from 04e-ev2score.csh
!c 
!c  input: eigenvalue.dat
!c  output: info-PCA.dat
!c
!c======================================================================c
program mkinfo
  use common_args,  only: nv, npc
  implicit none
  !c+++ [input]
  integer(4) :: jpc       !! PC number 
  real(8)    :: E(nv)     !! eigenvalue data
  !c+++ [output]
  real(8)    :: prop(nv)  !! prop.
  real(8)    :: cprop(nv) !! cumlative prop.
  !c+++ [work]
  integer(4) :: ipc       !! loop variable
  real(8)    :: cpn       !! cprop
  !c+++ open & read input data
  open(10, file='fort.10', status='old') 
  do ipc = 1, nv
    read(10, *) jpc, E(ipc)
  enddo !! ipc
  close(10)
  !c+++ open output data
  open(51, file='fort.51', status='unknown') 

  cprop(1) = E(1)
  do ipc = 2, nv
    if (E(ipc) > 0) then
      cprop(ipc) = cprop(ipc - 1) + E(ipc)
    else
      cprop(ipc) = cprop(ipc - 1)
    endif
  enddo !! ipc
  cpn = cprop(nv)

  !c+++ write
  do ipc = 1, nv
    prop(ipc) = abs(E(ipc)) / cpn * 100.0
    cprop(ipc) = cprop(ipc) / cpn * 100.0
    write(51, '(i5, 2x, e15.6, 2x, 2(f5.1,2x))') ipc, E(ipc), prop(ipc), cprop(ipc)
  enddo !! ipc
  close(51)
  write(6, *) 'PC  e-value    -- prop --   cumprop'
  do ipc = 1, npc 
    write(6, *) ipc, E(ipc), prop(ipc), cprop(ipc) 
  enddo !! ipc

  stop
end program mkinfo
