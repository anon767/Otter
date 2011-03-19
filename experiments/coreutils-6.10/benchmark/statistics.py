#!/usr/bin/env python

# Find the first occurence of "TargetReached" in Otter's output piped to timelines

import sys, re, os, math, string, subprocess
from collections import defaultdict

def mean(values):
    if len(values) == 0:
        raise Exception("Empty list")
    return sum(values) / len(values)

def mean_stdev(values):
    if len(values) == 0:
        raise Exception("Empty list")
    m = mean(values)
    sd = math.sqrt(mean([(x-m)*(x-m) for x in values]))
    return {"mean": m, "stdev": sd}

def median_siqr_outliers(values):
    length = len(values)
    if len(values) == 0:
        raise Exception("Empty list")
    if length < 3:
        print "Warning: list length < 3"
    values.sort()
    median = values[length/2]
    siqr = (values[length*3/4] - values[length/4]) / 2.0
    outliers = filter(lambda x: abs(x-median)>2*siqr, values)
    return { "median": median, "siqr": siqr, "outliers": outliers}

def normalized_range(values):
    values.sort()
    range = values[-1] - values[0]
    return { "normalized_range": range / mean(values) }

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

for directory in sys.argv[1:]:
    filenames = os.listdir(directory)
    for filename in filenames:
        program = program_re.match(filename).group(1)
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
    stat[key] = { "size": len(values), "values": values }
    stat[key].update(median_siqr_outliers(values))
    stat[key].update(normalized_range(values))
    stat[key].update(mean_stdev(values))


# type key = Pure | Mix(strategy, ratio)
# Pure: runbackotter --bidirectional-search-ratio=-1
# Mix(strategy, 0%): runotter --queue=strategy
# Mix(strategy, ratio): runbackotter --forward-queue=strategy --bidirectional-search-ratio=ratio

# program: filename%_\d+\.log
# seed: --random-seed=seed

def format(x):
    return "%.1f" % x if x < 1000.0 else "-"

def show_median_siqr_outliers_format(entry):
    if entry == None:
        return ""
    else:
        return "\\mso{%s}{%s}{%s}" % (format(entry["median"]), format(entry["siqr"]), "" if entry["outliers"]==[] else "%d"%len(entry["outliers"]))

def show_mean_stdev_format(entry):
    if entry == None:
        return ""
    else:
        return "\\msd{%s}{%s}{%d}" % (format(entry["mean"]), format(entry["stdev"]), entry["size"])

def show_all_format(entry):
    if entry == None:
        return ""
    else:
        values = [ format(x) for x in entry["values"]]
        return "\n" + string.join(values, "\n") + "\n"


def show_normalized_range_format(entry):
    if entry == None:
        return ""
    else:
        return "%.1f" % entry["normalized_range"]

def getstat_r(program, opts):
    global stat
    key = (program, frozenset(opts))
    if key not in stat:
        return None
    else:
        return stat[key]

def gnuplot(program, common_opts, opts):
    entry = getstat_r(program, opts+common_opts)
    if entry == None:
        return ""
    testname = re.compile(r"[^A-Za-z0-9\.-]").sub("_",str([program]+opts))
    print "Process %s" % testname
    plot_sh = os.path.join("gnuplot", "%s.plot" % testname)
    plot_gif = os.path.join("gnuplot", "%s.gif" % testname)
    plot_datafile = os.path.join("gnuplot", "%s.dat" % testname)
    if not os.path.exists("gnuplot"):
        os.makedirs("gnuplot")

    plot_dat = ""
    for value in entry["values"]:
        if value > 2000.0:
            value = 2000.0
        plot_dat += "1\t%f\n" % value
    f = open(plot_datafile, "w")
    print >>f, plot_dat
    f.close()

    plot_cmd = """
set terminal gif
set output "%s"
set autoscale
unset log
unset label
set xtic auto
set ytic auto
set ylabel "Running time (secs)"
set xr [.5:1.5]
set size ratio 10
plot "%s" using 1:2 with points
""" % (plot_gif, plot_datafile)
    f = open(plot_sh, "w")
    print >>f, plot_cmd
    f.close()
    subprocess.call("gnuplot %s" % plot_sh, shell=True)
    return plot_cmd


def getstat(program, common_opts, opts):
    #return gnuplot(program, common_opts, opts)
    return show_median_siqr_outliers_format(getstat_r(program, opts+common_opts))
    #return show_all_format(getstat_r(program, opts+common_opts))


# Add new programs here
programs = [
        ("mkdir", "mkdir"),
        ("mkfifo", "mkfifo"),
        ("mknod", "mknod"),
        ("paste", "paste"),
        ("seq", "seq"),
        ("ptx", "ptx"),
        ("mkdir-inj", "mkdir-inj"),
        ("mkfifo-inj", "mkfifo-inj"),
        ("mknod-inj", "mknod-inj"),
        ]

common_opt_list = [
        #[],
        [("backotter-timing-method", "stp-calls")],
        #[("backotter-timing-method", "real")],
    ]

for common_opts in common_opt_list:

    print common_opts

    print "Un-directed:"
    for program, program_name in programs:
        output = [ program_name ]

        # Pure BackOtter
        output.append(getstat(program, common_opts, ["runbackotter",("bidirectional-search-ratio","-1")]))

        for strategy in ["KLEE", "SAGE", "random-path"]:
            # Pure forward
            output.append(getstat(program, common_opts, ["runotter",("queue",strategy)]))
            for ratio in [ ".75", ".5" ]:
                output.append(getstat(program, common_opts, ["runbackotter",("forward-queue",strategy),("bidirectional-search-ratio",ratio)]))

        # join
        result = string.join(output, " & ") + " \\\\"
        print result

    print "Directed:"
    for program, program_name in programs:
        output = [ program_name ]

        # Pure BackOtter
        output.append(getstat(program, common_opts, ["runbackotter",("bidirectional-search-ratio","-1")]))

        for strategy in ["closest-to-targets", "closest-to-targets-path-weighted", "distance-to-targets-weighted", "path-weighted", "ESD"]:
            output.append(getstat(program, common_opts, ["runotter",("queue",strategy)]))

        # join
        result = string.join(output, " & ") + " \\\\"
        print result

