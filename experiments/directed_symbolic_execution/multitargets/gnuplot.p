#
# (echo 'benchmark = "pr"'; cat plot.p) | gnuplot > plot.eps
#
set term postscript eps color
set   autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels

set size 2, 1
set log x 2
set xtic auto                          # set xtics automatically
set ytic auto                          # set ytics automatically
set title benchmark
set xlabel "Time (secs)"
set ylabel "Coverage (lines)"
set key top left
set pointsize 0.7

plot \
   sprintf("plots/%s/InterSDSE/time.dat", benchmark)             using 2:1 title 'InterSDSE'         with linespoints lt 1 lc 1 lw 3 pt 7 , \
   sprintf("plots/%s/InterSDSE(RoundRobin)/time.dat", benchmark) using 2:1 title 'InterSDSE(RR)'     with linespoints lt 2 lc 1 lw 3 pt 7 , \
   sprintf("plots/%s/InterSDSE(Probabilistic)/time.dat", benchmark) using 2:1 title 'InterSDSE(Pr)'  with linespoints lt 4 lc 1 lw 3 pt 7 , \
   sprintf("plots/%s/CCBSE(RandomPath)/time.dat", benchmark)     using 2:1 title 'CCBSE(RandomPath)' with linespoints lt 2 lc 2 lw 3 pt 7 , \
   sprintf("plots/%s/OtterKLEE/time.dat", benchmark)             using 2:1 title 'OtterKLEE'         with linespoints lt 1 lc 3 lw 3 pt 7 , \
   sprintf("plots/%s/Mix(OtterKLEE)/time.dat", benchmark)        using 2:1 title 'Mix(OtterKLEE)'    with linespoints lt 2 lc 3 lw 3 pt 7 , \
   sprintf("plots/%s/OtterSAGE/time.dat", benchmark)             using 2:1 title 'OtterSAGE'         with linespoints lt 1 lc 4 lw 3 pt 7 , \
   sprintf("plots/%s/Mix(OtterSAGE)/time.dat", benchmark)        using 2:1 title 'Mix(OtterSAGE)'    with linespoints lt 2 lc 4 lw 3 pt 7 , \
   sprintf("plots/%s/RandomPath/time.dat", benchmark)            using 2:1 title 'RandomPath'        with linespoints lt 1 lc 5 lw 3 pt 7 , \
   sprintf("plots/%s/Mix(RandomPath)/time.dat", benchmark)       using 2:1 title 'Mix(RandomPath)'   with linespoints lt 2 lc 5 lw 3 pt 7
