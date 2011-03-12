#!/usr/bin/env python

# Find the first occurence of "TargetReached" in Otter's output piped to timelines

import sys, re, os, getopt
from collections import defaultdict

# type key = Pure | Mix(strategy, ratio)
# Pure: runbackotter --bidirectional-search-ratio=-1
# Mix(strategy, 0%): runotter --queue=strategy
# Mix(strategy, ratio): runbackotter --forward-queue=strategy --bidirectional-search-ratio=ratio

# program: filename%_\d+\.log
# seed: --random-seed=seed

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

def main():
    table = defaultdict(list)
    directory = sys.argv[1]
    filenames = os.listdir(directory)
    for filename in filenames:
        program = program_re.match(filename).group(1)
        filename = os.path.join(directory, filename)
        file = open(filename)
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
                table[(program, key)].append(time)
                break
    return table

table = main()
print table

