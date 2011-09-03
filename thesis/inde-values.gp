set terminal epslatex color solid
set output "inde-values.eps"
set data style lines
set title "increasing/decreasing method 4 sample plot"
set xlabel "time step"
set ylabel "value"
set xrange [0:3000]
plot "inde-values.data"
