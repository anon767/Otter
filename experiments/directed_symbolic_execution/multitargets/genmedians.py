#!/usr/bin/env python

import sys, csv

infty = float('inf')

def median_siqr_outliers(values):
    length = len(values)
    if len(values) == 0: raise Exception("Empty list")
    if length < 3: print "Warning: list length < 3"
    values = [ float(x) if x != 'NA' else infty for x in values ]
    values.sort()
    median = values[length/2]
    siqr = (values[length*3/4] - values[length/4]) / 2.0  if median != infty else infty
    outliers = filter(lambda x: abs(x-median)>2*siqr+0.000001, values)
    return [median, siqr, len(outliers)]

csv_reader = csv.reader(sys.stdin, delimiter=',', quotechar='"')
csv_writer = csv.writer(sys.stdout, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
#csv_writer.writerow(['Test','Strategy'] + ["d%d"%x for x in range(1,num_seeds+1)])

for row in csv_reader:
    program = row[0]
    strategy = row[1]
    if program == "Test" and strategy == "Strategy": continue  # Skip the header rows
    csv_writer.writerow([program, strategy] + median_siqr_outliers(row[2:]))

