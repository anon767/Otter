#!/usr/bin/env python
# Find the first occurence of "ASSERT" in KLEE's output piped to timelines
import sys, re
time = "NA"
for line in sys.stdin.readlines():
    results = re.compile(r"^\s*\d+\.\d+\s*(\d+\.\d+).*ASSERTION FAIL.*").match(line)
    if results:
        time = float(results.group(1))
        break
print time

