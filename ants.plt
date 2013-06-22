set term pngcairo
set output

set xlabel 'Seconds'

set ylabel 'Workers'
set y2label 'Queue'
set ytics nomirror
set y2tics

plot '-' using 1:2 title 'Workers', \
     '-' using 1:3 title 'Queue' axis x1y2
