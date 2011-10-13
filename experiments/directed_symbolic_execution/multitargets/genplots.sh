#!/bin/sh
for b in mkdir mkfifo mknod paste ptx pr seq md5sum tac
do
    (echo $(printf 'benchmark = "%s"' $b); cat gnuplot.p) | gnuplot > $b.eps
    epstopdf $b.eps
    rm $b.eps
done

