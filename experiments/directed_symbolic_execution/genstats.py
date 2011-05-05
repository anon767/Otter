#!/usr/bin/env python

import sys, csv
from collections import defaultdict

def median_siqr_outliers(values):
    length = len(values)
    if len(values) == 0:
        raise Exception("Empty list")
    if length < 3:
        print "Warning: list length < 3"
    values = [ float(x if x != 'NA' else 'inf') for x in values ]
    values.sort()
    median = values[length/2]
    siqr = (values[length*3/4] - values[length/4]) / 2.0
    outliers = filter(lambda x: abs(x-median)>2*siqr, values)
    return { "median": median, "siqr": siqr, "outliers": outliers}

def pretty_float(f):
    return "%.1f" % f if f < float('inf') else ''

def pretty_int(f):
    return "%d" % f if f > 0 else ''

csv_reader = csv.reader(sys.stdin, delimiter=',', quotechar='"')
csv_reader.next()  # skip the first row

table = defaultdict(dict)

for row in csv_reader:
    program = row[0]
    strategy = row[1]
    table[program][strategy] = median_siqr_outliers(row[2:])

program_list = [
         #'ProSDSE'  ,
         #'ProCCBSE' ,
         #'ProMix'   ,
         "mkdir",
         "mkfifo",
         "mknod",
         "paste",
         "ptx",
         "seq",
        ]

directed_strategy_list = [
         'InterSDSE',
         'IntraSDSE',
         'CCBSE(RandomPath)',
         'CCBSE(InterSDSE)',
         'CCBSE(IntraSDSE)',
        ]

klee_strategy_list = [
        'KLEE',
        ]

undirected_strategy_list = [
         'OtterKLEE',
         'Mix(OtterKLEE)',
         'OtterSAGE',
         'Mix(OtterSAGE)',
         'RandomPath',
         'Mix(RandomPath)',
        ]

total = defaultdict(float)

for strategy_list in [directed_strategy_list, klee_strategy_list, undirected_strategy_list]:
    for program in program_list:
        output = [ program ]
        for strategy in strategy_list:
            try:
                stats = table[program][strategy]
                median = pretty_float(stats['median'])
                if median == '':
                    macro = '\\timedout{}'
                else:
                    macro = '\\mso{%s}{%s}{%s}' % (median, pretty_float(stats['siqr']), pretty_int(len(stats['outliers'])))
                output.append(macro)
                median = stats['median']
                total[strategy] += median if median < float('inf') else 1800.0
            except KeyError:
                # Data missing
                output.append("")
        result = ' & '.join(output) + " \\\\"
        print result

    # Output total
    output = [ 'Total' ]
    for strategy in strategy_list:
        output.append(str(total[strategy]))
    result = ' & '.join(output) + " \\\\"
    print result

    print

