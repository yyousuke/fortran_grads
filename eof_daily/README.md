# eof_daily

## prepare

### (0) settings

modify setpath.sh

- **compiler**: (FC: compiler path, OPT: compiler options)

- **original data**: (DATADIR_JRA55: path to JRA55 data)

### (1) grid points

- **x-grid poins** are stored in xdef
    
(xdef.csh is used for JRA-55 288 grid points)

    % ./xdef.csh > xdef

- **y-grid poins** are stored in ydef
    
 (ydef.csh is used for JRA-55 145 grid points)

    % ./ydef.csh > ydef

- **z-grid poins** are stored in zdef (manually)

- **option of 01mkdata-x.csh**
    
    set mkgrid = 1: xdef, ydef, and zdef files are used

    set mkgrid = 0: only zdef file is used, and x-, y-grid points are calculated linearly

### (2) setpath.csh

mofify following compiler and options

    set FC = gfortran
    
    set OPT = "-O -fconvert=big-endian -frecord-marker=4"

### (3) input data

    % cd 00readgrads/

    modify 001link.csh (dsyy, nsyy, nsmm, neyy, nemm)

    % ./main.csh

### (4) modify 01mkdata-x.csh

- **data range**: 
    dsyy is start year of data (must be same as 00readgrads)

    analysis period is nsyy/nsmm to neyy/nemm

- **analysis period**

    set flag = 0 is used for whole month analysis during nsyy/nsmm-neyy/nemm
    
    set flag = 1 is used for limited nsmm-nemm analysis during nsyy-neyy

- **analysis region**
    
    longitude range: stx - enx
    
    latitude range: sty - eny
    
    set level1 = 1 and level2 = 1 for surface data
    
    for pressure  level data, level1 is mimimum and level2 is maximum

    set weight = 1 for pressure level data (weighted by the square root of the pressure interval)

    set weight = 0 for surface data

- **npc**: number of principal component for output

    number of opt_neg in 03post.csh must be the same as npc

- **grid information for original data**

    nx, ny, nz are the number of x-, y-, z-grid points 

    nmm is the number of months per year (For daily mean data, this value can be replaced by days par year)

- **variable name**

    modify set var = slp  for Sea Level Pressure data

## run

- **preprocess**

    % ./01mkdata-x.csh

- **main**

    % ./02e-ev.csh

- **post process**

    % ./03post.csh 
    
    (modify opt_neg to change sings for EOFs)
    