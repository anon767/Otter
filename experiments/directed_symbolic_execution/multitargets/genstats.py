#!/usr/bin/env python
import sys, csv, re
from collections import defaultdict

def pretty_float(f):
    return "%.1f" % f if f < float('inf') else ''

def pretty_int(f):
    return "%d" % f if f > 0 else ''

csv_reader = csv.reader(sys.stdin, delimiter=',', quotechar='"')

table = defaultdict(lambda: defaultdict(dict))

for row in csv_reader:
    results = re.compile(r'(.*):(.*)').match(row[0])
    if results != None:
        program = results.group(1)
        try:
            typ = int(results.group(2))
        except ValueError:
            typ = results.group(2)
        strategy = row[1]
        if program == "Test" and strategy == "Strategy": continue  # Skip the header rows
        table[program][typ][strategy] = {'median':float(row[2]), 'siqr':float(row[3]), 'outliers':int(row[4])}

program_info_list = [
        { "name" : "mkdir"    , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "mkfifo"   , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "mknod"    , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "paste"    , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "seq"      , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "ptx"      , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "md5sum"   , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "tac"      , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "pr"       , "timelimit" : 7200.0 , "count total" : True },
    ]

directed_strategy_list = [
         'InterSDSE',
         'InterSDSE(RoundRobin)',
         'InterSDSE(Probabilistic)',
         #'IntraSDSE',
         'CCBSE(RandomPath)',
         #'CCBSE(InterSDSE)',
         #'CCBSE(IntraSDSE)',
        ]

klee_strategy_list = [
        #'KLEE',
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

maxrows = 30

for strategy_list in [directed_strategy_list+ klee_strategy_list+ undirected_strategy_list]:
    for program_info in program_info_list:
        program = program_info['name']
        print '\\multitargetstable{%s}{' % program
        rows = sorted(table[program].keys())[:-3]
        for typ in ['(#targets)','(#removed)','(max)'] + rows[:maxrows]:
            output = [ str(typ) ]
            for strategy in strategy_list:
                try:
                    stats = table[program][typ][strategy]
                    median = pretty_float(stats['median'])
                    if median == '':
                        macro = '\\timedout{}'
                    else:
                        macro = '\\mso{%s}{%s}{%s}' % (median, pretty_float(stats['siqr']), pretty_int(stats['outliers']))
                    output.append(macro)
                except KeyError:
                    # Data missing
                    output.append("")
            result = ' & '.join(output) + ' \\\\'
            result = re.sub('#','\\#',result)
            print result
            if typ == '(max)':
                print '\\hline'
        if len(rows) > maxrows:
            result = ' & '.join(['(...)'] + ['']*len(strategy_list)) + ' \\\\'
            print result
        print '}'

