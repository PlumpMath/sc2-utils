#!/bin/sh
#
# Usage: zerg-graphs-gnuplot.sh DATA_FILE_1 DATA_FILE_2

DAT1="${1}"
DAT2="${2}"

gnuplot -persist <<EOF

set term png size 800,600 font size 8
#set term png size 400,300 font size 8
set output "tmp.png"
# http://en.wikibooks.org/wiki/Gnuplot
#set term svg fsize 8
#set output "tmp.svg"

set grid
set lmargin 8
set rmargin 10
set xlabel "seconds"
set ylabel "drones"
set y2label "minerals"
set xtics 60
set y2tics

set xrange [0:480]
set yrange [-5:90]
set y2range [-250:4500]

plot "< paste ${DAT1} ${DAT2}" using 1:(column(3)-column(6)) \
                               notitle w l lc rgb "magenta", \
     '${DAT2}' using 1:2 notitle smooth bezier lc rgb "#ff7f7f" axes x1y2, \
     '${DAT1}' using 1:2 notitle smooth bezier lc rgb "#7f7fff" axes x1y2, \
     '${DAT2}' using 1:3 t "${DAT2}" w l lc rgb "red", \
     '${DAT1}' using 1:3 t "${DAT1}" w l lc rgb "blue"

EOF
