set term pngcairo
set output

set xdata time
set timefmt '%s'

plot '-' using 1:2, '-' using 1:3
