#!/usr/bin/env python
import sys, csv, re
from collections import defaultdict

infty = float('inf')

def pretty_float(f):
    return "%.1f" % f if f < infty else ''

def pretty_int(f):
    return "%d" % f if f > 0 else ''

def pretty_num(f,isint=True):
    if isint: return "%d" % (int(f)) if f < infty else ''
    else:     return pretty_float(f)

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

#strategies = sorted(__import__(sys.argv[1][:-3], globals(), locals(), ['strategies'], -1).strategies.keys())
strategies = [
    # renamer
    "Batched(InterSDSE-efficient)",
    "Batched(InterSDSE-probabilistic)",
    "Batched(InterSDSE-roundrobin)",
    "Batched(RoundRobin(RandomPath,InterSDSE-efficient))",
    "Batched(RoundRobin(RandomPath,InterSDSE-roundrobin))",
    "Batched(Phases(KLEE,InterSDSE))",

    "CCBSE(random-path)",
    "KLEE",
    "Mix(KLEE,0.75)",
    "SAGE",
    "Mix(SAGE,0.75)",
    "random-path",
    "Mix(random-path,0.75)",
]

rowname = {'(#targets)':'n','(#removed)':'Cov.','(max)':'Time'}


for strategy in strategies:
    # TODO: print ids
    print "% ", strategy

for program_info in program_info_list:
    program = program_info['name']
    #rows = sorted(table[program].keys())[:-3]
    rows = ['(#removed)','(max)'] # + rows

    program_ntargets = '%s(%d)' % (program,int(table[program]['(#targets)'][strategies[0]]['median']))
    for typ in rows:
        output = [ program_ntargets, rowname[str(typ)] ]
        for strategy in strategies:
            try:
                stats = table[program][typ][strategy]
                median = pretty_num(stats['median'],isint=(typ=='(#removed)'))
                if median == '':
                    macro = '\\timedout{}'
                else:
                    macro = '\\mso{%s}{%s}{%s}' % (median, pretty_num(stats['siqr'],isint=(typ=='(#removed)')), pretty_int(stats['outliers']))
                output.append(macro)
            except KeyError:
                # Data missing
                output.append("")
        result = ' & '.join(output) + ' \\\\'
        print result
        if typ == '(max)':
            print '\\hline'

total = defaultdict(lambda: defaultdict(float))

def percent(ntargets,nremoved):
    if ntargets < 0.0001: return 0.0
    return (nremoved/ntargets)*100.0

# TODO:compute total
for program_info in program_info_list:
    program = program_info['name']
    if program_info["count total"]:
        for strategy in strategies:
            ntargets = table[program]['(#targets)'][strategy]['median']
            nremoved = table[program]['(#removed)'][strategy]['median']
            total[strategy]['ntargets'] += ntargets
            total[strategy]['nremoved'] += nremoved
            total[strategy]['percentage'] += percent(ntargets,nremoved)/len(program_info_list)

print ' & '.join(['','Avg\\%']+[pretty_float(total[strategy]['percentage']) for strategy in strategies]) + ' \\\\'
print ' & '.join(['','Agg\\%']+[pretty_float(percent(total[strategy]['ntargets'],total[strategy]['nremoved'])) for strategy in strategies]) + ' \\\\'
