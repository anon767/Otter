#!/usr/bin/env python
# TODO: factor out code to genmedians.py

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

csv_reader = csv.reader(sys.stdin, delimiter=',', quotechar='"')

table = defaultdict(lambda: defaultdict(list))

for row in csv_reader:
    results = re.compile(r'(.*):(\d+)').match(row[0])
    if results != None:
        program = results.group(1)
        #linenum = float(results.group(2))
        strategy = row[1]
        if row[2] != 'inf':
            median = float(row[2])
            table[program][strategy].append(median)

for program in table.keys():
    for strategy in table[program].keys():
        p = os.path.join(path, program, strategy)
        mkdir_p(p)
        p = os.path.join(p, 'time.dat')
        file = open(p, 'w')
        cov = 0
        for t in sorted(table[program][strategy]):
            cov += 1
            print >>file, "\t%d\t%.2f" % (cov, t)
        file.close()

