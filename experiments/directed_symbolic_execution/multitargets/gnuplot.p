set term postscript eps color
set   autoscale                        # scale axes automatically
unset log                              # remove any log-scaling
unset label                            # remove any previous labels

set size 1.5, 1.5
set log x 2
set xtic auto                          # set xtics automatically
set ytic auto                          # set ytics automatically
#set title benchmark
set xlabel "Time (secs)"
set ylabel "Coverage (normalized)"
#set key below
set key box bottom right
set pointsize 0.9

