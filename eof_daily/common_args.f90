module common_args
  integer(4), parameter :: nx = 288 !! number of x-grids
  integer(4), parameter :: ny = 145 !! number of y-grids
  integer(4), parameter :: nz = 1 !! number of z-grids
  integer(4), parameter :: nmm = 91 !! number of records (or month)

  integer(4), parameter :: nv = 1617 !! number of spatial grids
  integer(4), parameter :: nc = 80 !! number of time grids

  !c+++ start/end time
  integer(4), parameter :: nsyy = 2019 !! start year
  integer(4), parameter :: nsmm = 12 !! start month
  integer(4), parameter :: neyy = 2020 !! end year
  integer(4), parameter :: nemm = 2 !! end month

  !c+++ vertical weight
  integer(4), parameter :: weight = 0

  !c+++ info-PCA
  integer(4), parameter :: npc = 5 !! number of PCA for output

  !c+++ read climatorogy 
  logical, parameter    :: opt_clim = .false.
end module common_args
