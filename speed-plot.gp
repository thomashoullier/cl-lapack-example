# Gnuplot script. Plot the result of the speed assessment of square matrices
# multiplications.
set terminal png size 600,400
set output 'speed-plot.png'
set datafile separator " "
set key left top
set grid
set xlabel "Side of square matrices multiplied"
set ylabel "Real Time (s)"
set logscale y

plot 'speed.dat' u 1:2 w linespoints title "SBCL",\
     '' u 1:3 w linespoints title "SBCL specialized",\
     '' u 1:4 w linespoints title "LAPACK dgemm()"
