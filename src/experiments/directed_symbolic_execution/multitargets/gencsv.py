#!/usr/bin/env python
import sys, os, csv, re
from collections import defaultdict

path = sys.argv[1]

table = defaultdict(list)

def get_linenum(line):
    results = re.compile(r".*:(.*)").match(line)
    if results != None:
        return int(results.group(1))
    else:
        return -1

def infty(): return 'NA'

# using last timestamp for two targets on the same line
for program in sorted(os.listdir(path)):
    for strategy in sorted(os.listdir(os.path.join(path, program))):
        for seed in sorted(os.listdir(os.path.join(path, program, strategy))):
            p = os.path.join(path, program, strategy, seed, 'entry')
            if os.path.exists(p):
                file = open(p)
                targets = set()
                removed = defaultdict(infty)
                for line in file:
                    results = re.compile(r"^target: (.*)").match(line)
                    if results!=None: targets.add(results.group(1))
                    results = re.compile(r"^remove: (\S*) (.*)").match(line)
                    if results!=None: removed[results.group(2)] = float(results.group(1))
                file.close()
                table[(program, strategy)].append({'targets':targets,'removed':removed})

csv_writer = csv.writer(sys.stdout, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

#csv_writer.writerow(['Test','Strategy'] + ["d%d"%x for x in range(1,num_seeds+1)])

for (program, strategy), data in sorted(table.items()):
    csv_writer.writerow([program+':(#targets)', strategy] + sorted([len(x['targets']) for x in data]))
    csv_writer.writerow([program+':(#removed)', strategy] + sorted([len(x['removed']) for x in data]))
    csv_writer.writerow([program+':(max)', strategy] + sorted([infty() if len(x['removed'])==0 else max(x['removed'].values()) for x in data]))
    for target in data[0]['targets']:
        list = sorted([x['removed'][target] for x in data])
        if list[0] != infty():
            csv_writer.writerow(['%s:%d' % (program, get_linenum(target)), strategy] + list)

