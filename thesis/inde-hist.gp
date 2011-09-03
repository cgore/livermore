set terminal epslatex color solid 
set output "inde-hist.eps"
set data style lines
set title "increasing/decreasing method 4 sample performance"
set xlabel "time step"
set ylabel "accuracy"
set xrange [0:3000]
plot "inde-hist.data"
