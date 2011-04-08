#!/usr/bin/env python
import sys, random, string, os, getopt
from collections import defaultdict

macro = defaultdict(str)
seed_start = 1
num_seeds = 1
num_parallel = 1

try:
    opts, args = getopt.gnu_getopt(sys.argv[1:], "", ["trunk=", "dest=", "seed-start=", "num-seeds=", "parallel="])
except getopt.GetoptError, err:
    print str(err) 
    sys.exit(2)

for o, a in opts:
    if o == "--seed-start":
        seed_start = int(a)
    elif o == "--num-seeds":
        num_seeds = int(a)
    elif o == "--parallel":
        num_parallel= int(a)
    elif o.startswith("--"):
        macro['@%s@'%o[2:]] = a
    else:
        assert False, "unhandled option"

def macrosub(s):
    for key, value in macro.items():
        s = s.replace(key, value)
    return s

def writefile(filename, content):
    filename = macrosub(filename)
    content = macrosub(content)
    print "Write %s into %s" % (content, filename)
    dirname = os.path.dirname(filename)
    if not os.path.exists(dirname): os.makedirs(dirname)
    f = open(filename, "w")
    print >>f, content
    f.close()

test_files_map = defaultdict(list)

# For each benchmark module, generate its tests
for module in args:
    _temp = __import__(module, globals(), locals(), ['benchmarks', 'strategies', 'options', 'make_test'], -1)
    benchmarks = _temp.benchmarks
    strategies = _temp.strategies
    options    = _temp.options
    make_test  = _temp.make_test

    for seed in range(1, num_seeds+1):
        for strategy in strategies.items():
            for option in options.items():
                for benchmark in benchmarks.values():
                    command = benchmark['command']
                    for program in benchmark['programs'].items():
                        test_file, test_cmd = make_test(command, program, strategy, option, seed)
                        writefile(test_file, test_cmd)
                        test_files_map[seed].append(test_file)


# Generate run order, one per seed
run_files = []
for seed in range(1, num_seeds+1):
    test_files = test_files_map[seed]
    for i in range(0,7): random.shuffle(test_files)
    run_file = '@dest@/tests/%d.list' % seed
    writefile(run_file, string.join(['"%s"'%s for s in test_files], "\n"))
    run_files.append(run_file)

# Generate run script
writefile('@dest@/at.sh', "cat \\\n %s | xargs -P %d -n 1 sh" % (string.join(['"%s"'%s for s in run_files], "\\\n "), num_parallel))

