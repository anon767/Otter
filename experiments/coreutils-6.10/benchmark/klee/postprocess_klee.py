#!/usr/bin/env python
import sys, os, re, math
from collections import defaultdict

def mean(values):
    if len(values) == 0:
        raise Exception("Empty list")
    return sum(values) / len(values)

def mean_stdev(values):
    if len(values) == 0:
        raise Exception("Empty list")
    m = mean(values)
    sd = math.sqrt(mean([(x-m)*(x-m) for x in values]))
    return {"mean": m, "stdev": sd}

def median_siqr_outliers(values):
    length = len(values)
    if len(values) == 0:
        raise Exception("Empty list")
    if length < 3:
        print "Warning: list length < 3"
    values.sort()
    median = values[length/2]
    siqr = (values[length*3/4] - values[length/4]) / 2.0
    outliers = filter(lambda x: abs(x-median)>2*siqr, values)
    return { "median": median, "siqr": siqr, "outliers": outliers}

num_samples = int(sys.argv[1])
upper_bound = float(sys.argv[2])

table = defaultdict(list)

for ferr in sys.stdin.readlines():
    ferr = ferr.rstrip()

    prog = None
    results = re.compile(r"(.*/(.*?))/klee-out/(.*?)\.assert\.err").match(ferr)
    if results:
        path = results.group(1)
        prog = results.group(2)
        testname = results.group(3)

    func = None
    for line in open(ferr):
        results = re.compile(r"\s*#0\s*\d*\sin\s(.*?)\s.*").match(line)
        if results:
            func = results.group(1)
            break

    for line in open(os.path.join(path, "ktest-monitor.log")):
        results = re.compile(r"(.*)\.cov\s(\d.*)").match(line)
        if results:
            this_testname = results.group(1)
            time = float(results.group(2))
            if this_testname == testname:
                table[(prog, func)].append(time)
                break

for (prog, func), times in table.items():
    actual_prog = "%s%s" % (prog, "-inj" if "injected" in func else "")
    print "%s" % actual_prog, times


for (prog, func), times in table.items():
    while len(times) < num_samples:
        times.append(upper_bound)
    stats = median_siqr_outliers(times)
    actual_prog = "%s%s" % (prog, "-inj" if "injected" in func else "")
    print "%s & \\mso{%.1f}{%.1f}{%d} \\\\" % (actual_prog, stats["median"], stats["siqr"], len(stats["outliers"]))

print "Done"
