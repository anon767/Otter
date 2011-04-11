#!/usr/bin/env python
# Find the first occurence of "TargetReached" in Otter's output piped to timelines
# TODO: make gentests.py generate this script, with @seeds@ and @dest@ substituted, and run in the end of at.sh
import sys, re, os, csv
from collections import defaultdict
from benchmarks import *

def dosub(s):
    for argv in sys.argv[1:]:
        [key, value] = argv.split('=')
        s = s.replace('@%s@'%key, value)
    return s

num_seeds = int(dosub('@seeds@'))

table = defaultdict(list)
for seed in range(1, num_seeds+1):
    test_files = []
    for strategy in strategies.items():
        for option in options.items():
            for benchmark in benchmarks.values():
                for program in benchmark['programs'].items():
                    test_out = dosub('@dest@/results/%d/%s-%s-%s.log' % (seed, program[0], strategy[0], option[0]))
                    file = open(test_out)
                    time = "NA"
                    for line in file.readlines():
                        results = re.compile(r"^\s*\d+\.\d+\s*(\d+\.\d+).*`TargetReached.*").match(line)
                        if results:
                            time = float(results.group(1))
                            break
                    table[(program[0], strategy[0])].append(time)
                    file.close()

csv_writer = csv.writer(sys.stdout, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
csv_writer.writerow(['Test','Strategy'] + ["d%d"%x for x in range(1,num_seeds+1)])

for (program, strategy), times in sorted(table.items()):
    csv_writer.writerow([program, strategy] + sorted(times))

