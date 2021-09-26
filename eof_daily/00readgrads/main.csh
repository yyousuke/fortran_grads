#!/bin/csh

#foreach var ( slp )
foreach var ( uwnd vwnd )
 ./001link.csh ${var}
 ./002clim_mon.csh
 ./003zonal_mon.csh
end
