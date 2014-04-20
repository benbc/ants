set terminal pngcairo size 1280,480
set output

set key above

set border 3 # (1, 2, 4, 8) = (bottom, left, top, right)

set xlabel 'Time'
set xtics nomirror
set xdata time
set timefmt "%s"

set ylabel 'Queue'
set ytics nomirror

plot '-' using 1:2 title 'Queue' axis x1y1 with lines
