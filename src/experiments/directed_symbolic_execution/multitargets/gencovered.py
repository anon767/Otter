#!/usr/bin/env python
# Generate a dict of lines covered by strategies. Used by gentests to generate tests for individual line targets.
import sys, os, re, pprint

path = sys.argv[1]

table = dict()

# using last timestamp for two targets on the same line
for program in sorted(os.listdir(path)):
    table[program] = set()
    for strategy in sorted(os.listdir(os.path.join(path, program))):
        for seed in sorted(os.listdir(os.path.join(path, program, strategy))):
            p = os.path.join(path, program, strategy, seed, 'entry')
            if os.path.exists(p):
                file = open(p)
                for line in file:
                    results = re.compile(r"^remove: \S* (.*\.c:(\d+))").match(line)
                    if results!=None:
                        linenum = int(results.group(2))
                        line = results.group(1)
                        table[program].add((linenum, line))
                file.close()
    table[program] = sorted(table[program])

pp = pprint.PrettyPrinter(indent=4)
print "covered = ",
pp.pprint(table)

