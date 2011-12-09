#!/usr/bin/env python

import sys, csv
from collections import defaultdict

def median_siqr_outliers(values):
    length = len(values)
    if len(values) == 0:
        raise Exception("Empty list")
    if length < 3:
        print "Warning: list length < 3"
    values = [ float(x if x != 'NA' and x != '' else 'inf') for x in values ]
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

table = defaultdict(dict)

for row in csv_reader:
    program = row[0]
    strategy = row[1]
    if program == "Test" and strategy == "Strategy": continue  # Skip the header rows
    table[program][strategy] = median_siqr_outliers(row[2:])
    table["Total"][strategy] = {'median':0, 'siqr':0, 'outliers':[]}  # lazy way of initializing Total

program_info_list = [
        { "name" : 'Fig 3.1'  , "cmd" : 'mso' , "timelimit" : 900.0  , "count total" : False },
        { "name" : 'Fig 3.3'  , "cmd" : 'mso' , "timelimit" : 900.0  , "count total" : False },
        { "name" : 'Fig 3.5'  , "cmd" : 'mso' , "timelimit" : 900.0  , "count total" : False },
        { "name" : "mkdir"    , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "mkfifo"   , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "mknod"    , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "paste"    , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "seq"      , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "ptx"      , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "ptx2"     , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "md5sum"   , "cmd" : 'mso' , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "tac"      , "cmd" : 'mso' , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "pr"       , "cmd" : 'mso' , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "Total"    , "cmd" : 'tso' ,                        "count total" : False },
    ]

#strategies = sorted(__import__(sys.argv[1][:-3], globals(), locals(), ['strategies'], -1).strategies.keys())
strategies = [
    "SDSE",
    "SDSE-pr",
    "SDSE-intra",
    "B(SDSE)",
    "B(SDSE-pr)",
    "RR(RP,SDSE)",
    "B(RR(RP,SDSE))",
    "B(Ph(OtterKLEE,SDSE,3))",
    "CCBSE(SDSE)",
    "CCBSE(RP)",
    "OtterKLEE",
    "Mix-CCBSE(OtterKLEE)",
    "OtterSAGE",
    "Mix-CCBSE(OtterSAGE)",
    "RP",
    "Mix-CCBSE(RP)",
    "KLEE",
        ]


# Compute total
for program_info in program_info_list:
    program = program_info['name']
    if program_info["count total"]:
        for strategy in strategies:
            try:
                stats = table[program][strategy]
                median = stats['median']
                if median == float('inf'): median = program_info["timelimit"]
            except KeyError:
                median = program_info["timelimit"]
            table["Total"][strategy]['median'] += median


for program_info in program_info_list:
    program = program_info['name']
    output = [ program ]

    for strategy in strategies:
        try:
            stats = table[program][strategy]
            median = pretty_float(stats['median'])
            if median == '':
                macro = '\\timedout{}'
            else:
                macro = '\\%s{%s}{%s}{%s}' % (program_info["cmd"], median, pretty_float(stats['siqr']), pretty_int(len(stats['outliers'])))
            output.append(macro)
        except KeyError:
            output.append("")
    result = ' & '.join(output) + " \\\\"
    print result

print

