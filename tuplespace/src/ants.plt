set terminal pngcairo size 1280,480
set output

set key above

set border 3 # (1, 2, 4, 8) = (bottom, left, top, right)

set xlabel 'Time'
set xtics nomirror
set xdata time
set timefmt "%Y-%m-%dT%H:%M:%S"
set format x "%M:%.2S"

set ylabel 'Queue'
set ytics nomirror

plot '-' using 1:2 title 'Queue' axis x1y1 with lines
