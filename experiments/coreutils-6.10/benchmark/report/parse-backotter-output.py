#!/usr/bin/env python
import sys, re

line_target_re = re.compile(r'.*Line target: (\S*.c:\d+) in function (.*)')
target_reached_re = re.compile(r'\s*(\d+\.\d+)\s*(\d+\.\d+)\s*(\S+)\s*\[.*\]\s*(\S*.c:\d+) : TargetReached.*')

n_line_targets = 0

covered = dict()
def add_covered(line, time):
    if line not in covered:
        covered[line] = time
    else:
        covered[line] = min(covered[line], time)
    return

for line in open(sys.argv[1]):
    results = line_target_re.match(line)
    if results:
        #print results.group(1)
        n_line_targets += 1
        continue
    results = target_reached_re.match(line)
    if results:
        #print results.group(2), results.group(3), results.group(4)
        time = float(results.group(2))
        origin_function = results.group(3)
        line = results.group(4)
        if origin_function == "__otter_main_driver":
            add_covered(line, time)

s = ""
for k,v in covered.items():
    s += "%.2f " % v

print "%d\t%d\t%s" % (n_line_targets, len(covered), s)
