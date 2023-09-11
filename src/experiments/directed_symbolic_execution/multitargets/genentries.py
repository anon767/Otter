#!/usr/bin/env python

import sys, re, os.path
import os, errno
from types import *

seed_start = int(sys.argv[1])
seed_end = int(sys.argv[2])
base_in = sys.argv[3]
base_out = sys.argv[4]
module = sys.argv[5]

def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise

program_info_list = [
        { "name" : "mkdir"    , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "mkfifo"   , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "mknod"    , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "paste"    , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "seq"      , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "ptx"      , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "md5sum"   , "timelimit" : 1800.0 , "count total" : True },
        { "name" : "tac"      , "timelimit" : 7200.0 , "count total" : True },
        { "name" : "pr"       , "timelimit" : 7200.0 , "count total" : True },
    ]

_temp = __import__(module[:-3], globals(), locals(), ['strategies', 'options'], -1)

strategies = sorted(_temp.strategies.keys())
options    = sorted(_temp.options.keys())

strategy_list = strategies
seeds = range(seed_start,seed_end+1)


for seed in seeds:
    for option in options:
        for program_info in program_info_list:
            program = program_info["name"]
            output = [ program ]
            for strategy in strategy_list:
                path = os.path.join(base_in, "%d" % seed, "%s-%s-%s.log" % (program, strategy, option))
                output = os.path.join(base_out, option, program, strategy, "%d" % seed, "entry")

                add_targets = set()
                remove_targets = set()

                if os.path.exists(path):
                    file = open(path, "r")
                    lines = file.readlines()
                    for line in lines:
                        ## Change this part for different extraction needs
                        results = re.compile(r".*Add target at (.*)").match(line)
                        if results != None:
                            target = results.group(1)
                            add_targets.add(target)
                            continue
                        results = re.compile(r"^\s*\d+\.\d+\s*(\d+\.\d+).*Remove target at (.*)").match(line)
                        if results != None:
                            time = float(results.group(1))
                            target = results.group(2)
                            remove_targets.add((time, target))
                    file.close()

                    mkdir_p(os.path.dirname(output))
                    file = open(output, "w")
                    print >>file, "#targets: %d" % len(add_targets)
                    print >>file, "#removed: %d" % len(remove_targets)
                    for target in sorted(add_targets):
                        print >>file, "target: %s" % target
                    for time, target in sorted(remove_targets):
                        print >>file, "remove: %5.2f %s" % (time,target)

                    file.close()

