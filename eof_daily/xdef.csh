#!/bin/csh
set s = 0
while (${s} <= 288)
  echo "${s}*1.25" | bc
  @ s ++
end

