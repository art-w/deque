#!/usr/bin/gnuplot -persist

set title "appending a list with itself"
set xlabel "length (number of elements)"
set ylabel "time (ms)"
set key left top

plot "bench.tsv" using 1:2 title "List" with lines, \
     "bench.tsv" using 1:4 title "Steque" with lines, \
     "bench.tsv" using 1:5 title "Deck" with lines, \
     "bench.tsv" using 1:6 title "Deckrev" with lines

set terminal pngcairo
set output "/tmp/append.png"
replot
