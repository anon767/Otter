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
        { "name" : "ProSDSE"  , "target" : ''          },
        { "name" : "ProCCBSE" , "target" : ''          },
        { "name" : "ProMix"   , "target" : ''          },
        { "name" : "mkdir"    , "target" : 'lib/quotearg.c:252'          },
        { "name" : "mkfifo"   , "target" : 'lib/quotearg.c:252'          },
        { "name" : "mknod"    , "target" : 'lib/quotearg.c:252'          },
        { "name" : "paste"    , "target" : 'src/paste.c:'                }, #{ "name" : "paste"    , "target" : 'src/paste.c:196'             },
        { "name" : "seq"      , "target" : 'src/seq.c:215'               },
        { "name" : "ptx"      , "target" : 'src/ptx.c:312'               },
        { "name" : "md5sum"   , "target" : 'src/md5sum.c:216'            },
        { "name" : "ptx2"     , "target" : 'src/ptx.c:1510'              },
        { "name" : "pr"       , "target" : 'src/pr.c:2674'               },
        { "name" : "tac"      , "target" : 'lib/regexec.c:568'           },
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
            target = program_info["target"]
            output = [ program ]
            for strategy in strategy_list:
                path = os.path.join(base_in, "%d" % seed, "%s-%s-%s.log" % (program, strategy, option))
                output = os.path.join(base_out, option, program, strategy, "%d" % seed, "entry")

                if os.path.exists(path):
                    file = open(path, "r")
                    lines = file.readlines()
                    time = None
                    for line in lines:
                        results = re.compile(r"^\s*\d+\.\d+\s*(\d+\.\d+).*KLEE: ERROR:.*").match(line)
                        if results != None and line.find(target) >= 0:
                            time = float(results.group(1))
                            break
                    file.close()

                    mkdir_p(os.path.dirname(output))
                    file = open(output, "w")
                    print >>file, "%.3f" % time if time != None else "NA"
                    file.close()

