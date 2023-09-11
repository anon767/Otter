#!/usr/bin/env python
import sys, re, os
from collections import defaultdict

origin_function_re = re.compile(r".*\s([^0-9]\S*) \[\d+,\d+]")

input_file = sys.argv[1]
output_dir = sys.argv[2]
files = defaultdict(list)

for line in open(input_file):
    results = origin_function_re.match(line)
    if results:
        origin_function = results.group(1)
        files[origin_function].append(line.rstrip())
    else:
        files["0"].append(line.rstrip())

for origin_function, lines in files.items():
    output_file = os.path.join(output_dir, origin_function + ".log")
    try:
        os.makedirs(os.path.dirname(output_file))
    except OSError:
        pass
    f = open(output_file, "w")
    for line in lines:
        print >>f, line
    f.close()

