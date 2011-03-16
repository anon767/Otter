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

def median_interquartile_range_outliers(values):
    length = len(values)
    if length < 3:
        return None
    values.sort()
    median_index = length/2
    median = values[median_index]
    interquartile_range = values[median_index+1] - values[median_index-1]
    outliers = filter(lambda x: abs(x-median)>interquartile_range, values)
    return { "median": median, "interquartile_range": interquartile_range, "outliers": outliers}

def median_and_rest(values):
    length = len(values)
    if length < 3:
        return None
    values.sort()
    median_index = length/2
    median = values[median_index]
    rest = values[0:median_index] + values[median_index+1:]
    return { "median": median, "rest": rest}


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
#programs = set()

for directory in sys.argv[1:]:
    filenames = os.listdir(directory)
    for filename in filenames:
        program = program_re.match(filename).group(1)
        #programs.add(program)
        filename = os.path.join(directory, filename)
        file = open(filename)
        time = 100000.0
        for line in file.readlines():
            results = command_re.match(line)
            if results:
                cmd = results.group(1)
                args = re.split(" |\t|\r|\n|=", results.group(2))
                optlist = loose_getopt(args, ["queue=", "forward-queue=", "bidirectional-search-ratio=", "backotter-timing-method="])
                key = frozenset([cmd] + optlist)
                continue
            results = time_re.match(line)
            if results:
                time = float(results.group(1))
                break
        table[(program, key)].append(time)
        file.close()

stat = dict()
for key, values in table.items():
    stat[key] = median_and_rest(values)

# type key = Pure | Mix(strategy, ratio)
# Pure: runbackotter --bidirectional-search-ratio=-1
# Mix(strategy, 0%): runotter --queue=strategy
# Mix(strategy, ratio): runbackotter --forward-queue=strategy --bidirectional-search-ratio=ratio

# program: filename%_\d+\.log
# seed: --random-seed=seed

def median_interquartile_range_outliers_format(entry):
    if entry == None:
        return ""
    elif entry["median"] >= 1000.0 or entry["interquartile_range"] >= 1000.0:
        return "$\\infty$"
    else:
        return "\\mio{%.1f}{%.1f}{%s}" % (entry["median"], entry["interquartile_range"], "" if entry["outliers"]==[] else "%d"%len(entry["outliers"]))

def show_all_format(entry):
    if entry == None:
        return ""
    else:
        f = lambda x: "%.1f" % x if x < 1000.0 else "-"
        ff = lambda x: "%.0f" % x if x < 1000.0 else "-"
        g = lambda x, y: ff(x[y]) if y < len(x) else ""
        return "\\v{%s}{%s}{%s}{%s}{%s}" % (
                f(entry["median"]),
                g(entry["rest"],0),
                g(entry["rest"],1),
                g(entry["rest"],2),
                g(entry["rest"],3)
                )

def getstat_r(program, opts):
    global stat
    key = (program, frozenset(opts))
    if key not in stat:
        return None
    else:
        return stat[key]

def getstat(program, common_opts, opts):
    return getstat_r(program, opts+common_opts)

format = show_all_format

programs = [
        ("mkdir_comb", "mkdir"),
        ("mkfifo_comb", "mkfifo"),
        ("mknod_comb", "mknod"),
        ("paste_comb", "paste"),
        ("mkdir_1", "mkdir-inj"),
        ("mkfifo_1", "mkfifo-inj"),
        ("mknod_1", "mknod-inj"),
        ]

for common_opts in [[("backotter-timing-method", "stp-calls")], [("backotter-timing-method", "real")]]:

    print common_opts

    print "General:"
    for program, program_name in programs:
        output = [ program_name ]

        # Pure BackOtter
        output.append(format(getstat(program, common_opts, ["runbackotter",("bidirectional-search-ratio","-1")])))

        for strategy in ["KLEE", "SAGE", "random-path"]:
            # Pure forward
            output.append(format(getstat(program, common_opts, ["runotter",("queue",strategy)])))
            for ratio in [ ".75", ".5" ]:
                output.append(format(getstat(program, common_opts, ["runbackotter",("forward-queue",strategy),("bidirectional-search-ratio",ratio)])))

        # join
        result = string.join(output, " & ") + " \\\\"
        print result

    print "Directed:"
    for program, program_name in programs:
        output = [ program_name ]

        # Pure BackOtter
        output.append(format(getstat(program, common_opts, ["runbackotter",("bidirectional-search-ratio","-1")])))

        for strategy in ["closest-to-targets", "closest-to-targets-path-weighted", "distance-to-targets-weighted", "path-weighted", "ESD"]:
            output.append(format(getstat(program, common_opts, ["runotter",("queue",strategy)])))

        # join
        result = string.join(output, " & ") + " \\\\"
        print result

