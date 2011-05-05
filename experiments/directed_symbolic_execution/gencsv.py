#!/usr/bin/env python
import sys, os, csv
from collections import defaultdict

num_seeds = int(sys.argv[1])
path = sys.argv[2]

table = defaultdict(list)

for program in sorted(os.listdir(path)):
    for strategy in sorted(os.listdir(os.path.join(path, program))):
        for seed in sorted(os.listdir(os.path.join(path, program, strategy))):
            p = os.path.join(path, program, strategy, seed, 'entry')
            if os.path.exists(p):
                file = open(p)
                time = file.readline().rstrip()
                file.close()
                try: time = float(time)
                except ValueError: pass
                table[(program, strategy)].append(time)

csv_writer = csv.writer(sys.stdout, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
csv_writer.writerow(['Test','Strategy'] + ["d%d"%x for x in range(1,num_seeds+1)])

for (program, strategy), times in sorted(table.items()):
    csv_writer.writerow([program, strategy] + sorted(times))

