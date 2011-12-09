#!/usr/bin/env python
# Input: a csv of the form
# program(total):line, strategy, median-time

import sys, csv, re, os
from collections import defaultdict

def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise

path = sys.argv[1]

aggregated = "AGGREGATED"

csv_reader = csv.reader(sys.stdin, delimiter=',', quotechar='"')

table = defaultdict(lambda: defaultdict(list))

num_targets_map = {
    "mkdir"  : 8,
    "mkfifo" : 11,
    "mknod"  : 23,
    "paste"  : 78,
    "seq"    : 16,
    "ptx"    : 517,
    "md5sum" : 65,
    "tac"    : 51,
    "pr"     : 92,
}

for row in csv_reader:
    results = re.compile(r'(.*):(\d+)').match(row[0])
    if results != None:
        program = results.group(1)
        num_targets = num_targets_map[program]
        #linenum = float(results.group(2))
        strategy = row[1]
        if row[2] != 'inf':
            median = float(row[2])
            table[program][strategy].append((median,1.0))
            table[aggregated][strategy].append((median,1.0/num_targets))

for program in table.keys():
    for strategy in table[program].keys():
        p = os.path.join(path, program, strategy)
        mkdir_p(p)
        p = os.path.join(p, 'time.dat')
        file = open(p, 'w')
        cov = 0.0
        for t,v in sorted(table[program][strategy]):
            cov += v
            print >>file, "\t%.4f\t%.2f" % (cov, t)
        file.close()

