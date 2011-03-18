#!/usr/bin/env python
import sys, time
from datetime import datetime

last_line = ''
time_acc = 0.0 
print "[Start] %s" % str(datetime.now())
while True:
    line = sys.stdin.readline()
    t_now = time.time()
    if last_line != '':
        time_elapsed = t_now - t_last
        time_acc += time_elapsed
        print "%6.2f\t%6.2f\t" % (time_elapsed, time_acc), last_line,
        sys.stdout.flush()
    if line == '': break
    t_last = t_now
    last_line = line
print "[End] %s" % str(datetime.now())

