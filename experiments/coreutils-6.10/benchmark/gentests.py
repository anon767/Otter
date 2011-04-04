#!/usr/bin/env python
import sys, random, string, os
from benchmarks import *

def dosub(s):
    for argv in sys.argv[1:]:
        [key, value] = argv.split('=')
        s = s.replace('@%s@'%key, value)
    return s

def writefile(filename, content):
    filename = dosub(filename)
    content = dosub(content)
    print "Write %s into %s" % (content, filename)
    dirname = os.path.dirname(filename)
    if not os.path.exists(dirname): os.makedirs(dirname)
    f = open(filename, "w")
    print >>f, content
    f.close()

num_seeds = int(dosub('@seeds@'))
num_parallel = int(dosub('@parallel@'))

run_files = []
for seed in range(1, num_seeds+1):
    test_files = []
    for strategy in strategies.items():
        for option in options.items():
            for benchmark in benchmarks.values():
                for program in benchmark['programs'].items():
                    test_file = '@dest@/tests/%d/%s-%s-%s.sh' % (seed, program[0], strategy[0], option[0])
                    test_out  = '"@dest@/results/%d/%s-%s-%s.log"' % (seed, program[0], strategy[0], option[0])
                    test_cmd  = 'mkdir -p $(dirname %s)' % test_out + "\n"
                    test_cmd += '%s %s %s %s --random-seed=%d 2>&1 | timelines > %s' % (benchmark['command'], strategy[1], option[1], program[1], seed, test_out)
                    writefile(test_file, test_cmd)
                    test_files.append(test_file)
    for i in range(0,7): random.shuffle(test_files)
    run_file = '@dest@/tests/%d.list' % seed
    writefile(run_file, string.join(['"%s"'%s for s in test_files], "\n"))
    run_files.append(run_file)

writefile('@dest@/at.sh', "cat \\\n %s | xargs -P %d -n 1 sh" % (string.join(['"%s"'%s for s in run_files], "\\\n "), num_parallel))

