from subprocess import *
import sys

if len(sys.argv) != 3:
    print 'Usage: python pathCov.py <data file> <possible values file>'
    sys.exit(0)

output = Popen(["./guaranteedCoverage_printPath",sys.argv[1],'--type','path','--fileWithPossibleValues',sys.argv[2]], stdout=PIPE).stdout

try:
    while True:
        line = output.next()
        if line.startswith('Sample'):
            print 'Under the condition'
        else:
            continue
        line = output.next()
        config = []
        while line != '\n' and not line.startswith('but '):
            config.append(line[:-1])
            line = output.next()
        config.sort()
        print '\n'.join(config)
        print 'these 1 paths are hit'
        print ','.join(config) # This represents the path covered by the configuration
        print
except StopIteration:
    pass
