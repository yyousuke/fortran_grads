#!/bin/csh
set s = 0
while (${s} <= 144)
  echo "90.0 - ${s}*1.25" | bc
  @ s ++
end

