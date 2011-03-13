#!/usr/bin/env python

# Find the first occurence of "TargetReached" in Otter's output piped to timelines

import sys, re, os, math, string
from collections import defaultdict

def mean(values):
    if len(values) == 0:
        raise Exception("Empty list")
    return sum(values) / len(values)

def stdev(values):
    if len(values) == 0:
        raise Exception("Empty list")
    m = mean(values)
    return math.sqrt(mean([(x-m)*(x-m) for x in values]))

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
time_re = re.compile(r"^\s*\d+\.\d+\s*(\d+\.\d+).*TargetReached.*")
program_re = re.compile(r"(.*)_\d+\.log")

table = defaultdict(list)
programs = set()

directory = sys.argv[1]
filenames = os.listdir(directory)
for filename in filenames:
    program = program_re.match(filename).group(1)
    programs.add(program)
    filename = os.path.join(directory, filename)
    file = open(filename)
    time = 100000.0  # Penalty
    for line in file.readlines():
        results = command_re.match(line)
        if results:
            cmd = results.group(1)
            args = re.split(" |\t|\r|\n|=", results.group(2))
            optlist = loose_getopt(args, ["queue=", "forward-queue=", "bidirectional-search-ratio="])
            key = frozenset([cmd] + optlist)
            continue
        results = time_re.match(line)
        if results:
            time = float(results.group(1))
            break
    table[(program, key)].append(time)

stat = dict()
for key, values in table.items():
    stat[key] = { "len":len(values), "mean":mean(values), "stdev":stdev(values) }


# type key = Pure | Mix(strategy, ratio)
# Pure: runbackotter --bidirectional-search-ratio=-1
# Mix(strategy, 0%): runotter --queue=strategy
# Mix(strategy, ratio): runbackotter --forward-queue=strategy --bidirectional-search-ratio=ratio

# program: filename%_\d+\.log
# seed: --random-seed=seed

def format(entry):
    return "\\meanstdev{%.2f}{%.2f}" % (entry["mean"], entry["stdev"])

def getstat(program, opts):
    global stat
    key = (program, frozenset(opts))
    if key not in stat:
        return {"mean":-1.0,"stdev":-1.0}
    else:
        return stat[key]

for program in programs:
    output = [ program ]

    # Pure BackOtter
    output.append(format(getstat(program, ["runbackotter",("bidirectional-search-ratio","-1")])))

    for strategy in ["KLEE", "SAGE", "random-path", "depth-first"]:
        # Pure forward
        output.append(format(getstat(program, ["runotter",("queue",strategy)])))
        for ratio in [ ".75", ".5" ]:
            output.append(format(getstat(program, ["runbackotter",("forward-queue",strategy),("bidirectional-search-ratio",ratio)])))

    # Naive distance to targets (TODO)
    output.append(format(getstat(program, ["runotter",("queue","naive-distance-to-targets")])))

    # Simple ESD (TODO)
    output.append(format(getstat(program, ["runotter",("queue","simple-esd")])))

    # join
    result = string.join(output, " & ") + " \\\\"
    print result

