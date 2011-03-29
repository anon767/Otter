#!/usr/bin/env python

# Find the first occurence of "TargetReached" in Otter's output piped to timelines

import sys, re, os, csv
from collections import defaultdict

def loose_getopt(args, long_options):
    i = 0
    optlist = []
    while i < len(args):
        if args[i][0:2] == "--":
            if args[i][2:] in long_options:
                optlist.append((args[i][2:],''))
            elif args[i][2:]+"=" in long_options:
                optlist.append((args[i][2:],args[i+1]))
                i += 1
        i += 1
    return optlist

command_re = re.compile(r"^Command: .*(runotter|runbackotter) (.*)")
time_re = re.compile(r"^\s*\d+\.\d+\s*(\d+\.\d+).*`TargetReached.*")
program_re = re.compile(r"(.*)_\d+\.log")

table = defaultdict(list)

program_map = {
        "mkdir": "mkdir",
        "mkfifo": "mkfifo",
        "mknod": "mknod",
        "paste": "paste",
        "seq": "seq",
        "ptx": "ptx",
        "pro_distance_1": "Figure 2",
        "pro_backotter_3": "Figure 3",
        }

strategy_map = {
        frozenset(['runotter', ('queue', 'closest-to-targets-intraprocedural')]) : 'IntraSDSE',
        frozenset(['runotter', ('queue', 'KLEE')]) : 'Otter-KLEE',
        frozenset(['runotter', ('queue', 'random-path')]) : 'Random Path',
        frozenset(['runotter', ('queue', 'SAGE')]) : 'Otter-SAGE',
        frozenset(['runotter', ('queue', 'closest-to-targets')]) : 'InterSDSE',
        frozenset(['runbackotter', ('backward-queue', 'backotter-closest-to-targets'), ('bidirectional-search-ratio', '-1')]) : 'CCBSE(InterSDSE)',
        frozenset(['runbackotter', ('backward-queue', 'backotter-closest-to-targets-intraprocedural'), ('bidirectional-search-ratio', '-1')]) : 'CCBSE(IntraSDSE)',
        frozenset(['runbackotter', ('backward-queue', 'random-path'), ('bidirectional-search-ratio', '-1')]) : 'CCBSE(Random Path)',
        frozenset(['runbackotter', ('backward-queue', 'random-path'), ('bidirectional-search-ratio', '.5'), ('forward-queue', 'KLEE')]) : 'Mix-CCBSE(Otter-KLEE)',
        frozenset(['runbackotter', ('backward-queue', 'random-path'), ('bidirectional-search-ratio', '.5'), ('forward-queue', 'random-path')]) : 'Mix-CCBSE(Random Path)',
        frozenset(['runbackotter', ('backward-queue', 'random-path'), ('bidirectional-search-ratio', '.5'), ('forward-queue', 'SAGE')]) : 'Mix-CCBSE(Otter-SAGE)',
        }

num_seed = int(sys.argv[1])

for directory in sys.argv[2:]:
    filenames = os.listdir(directory)
    for filename in filenames:
        program = program_re.match(filename).group(1)
        if program not in program_map:
            print "%s doesn't exist in the program list" % program
            continue
        else:
            program = program_map[program]

        filename = os.path.join(directory, filename)
        file = open(filename)
        time = "NA"
        for line in file.readlines():
            results = command_re.match(line)
            if results:
                cmd = results.group(1)
                args = re.split(" |\t|\r|\n|=", results.group(2))
                optlist = loose_getopt(args, ["queue=", "forward-queue=", "backward-queue=", "bidirectional-search-ratio="])
                strategy = frozenset([cmd] + optlist)
                if strategy not in strategy_map:
                    print "%s doesn't exist in the strategy list" % str(strategy)
                    break
                else:
                    strategy = strategy_map[strategy]
                    continue
            results = time_re.match(line)
            if results:
                time = float(results.group(1))
                break
        table[(program, strategy)].append(time)
        file.close()


csv_writer = csv.writer(sys.stdout, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
csv_writer.writerow(['Test','Strategy'] + ["d%d"%x for x in range(1,num_seed+1)])

for (program, strategy), times in sorted(table.items()):
    if len(times) != num_seed:
        print "(%s,%s) has %d runs, not the same as %d" % (program, strategy, len(times), num_seed)
    csv_writer.writerow([program, strategy] + sorted(times))

