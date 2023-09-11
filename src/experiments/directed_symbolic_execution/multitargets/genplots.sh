#!/bin/sh
for b in AGGREGATED #mkdir mkfifo mknod paste ptx pr seq md5sum tac
do
    (echo $(printf 'benchmark = "%s"' $b); cat gnuplot.p; python gnuplot_gen.py) | gnuplot | epstopdf --filter > $b.pdf
done

