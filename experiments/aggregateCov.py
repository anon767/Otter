import sys

if len(sys.argv) == 1:
    print 'Usage: python aggregateCov.py [--useMethod2] FILE...'
    sys.exit(0)

configs = []
coverage = []

counter=0

def merge1(config1,config2):
    if config1 <= config2:
        return config2
    if config1 >= config2:
        return config1
    return None

def merge2(config1,config2):
    union = config1 | config2
    varList = [x[0] for x in union]
    # If they agree on all variables they share, they are compatible
    if len(varList) == len(set(varList)):
        return union
    return None

if sys.argv[1] == '--useMethod2':
    merge = merge2
    sys.argv[:2] = []
else:
    merge = merge1
    sys.argv[:1] = []

# First, read in the coverage information from all of the files
for filename in sys.argv:
    file = open(filename)
    line = 'x'
    while line != '' and not line.startswith('STP was invoked'):
        line = file.readline()
        if line == 'Sample value:\n':
            counter += 1
            thisConfig = set() # A configuration is a set of (variable,value) pairs
            # Read in the configuration
            line = file.readline()
            while line != '\n':
                variable,value = line.split('=')
                thisConfig.add((variable,int(value)))
                line = file.readline()
            coveredLines = set()
            # Read in the coverage
            line = file.readline() # Skip past 'The lines hit were:'
            line = file.readline()
            while line != '\n':
                coveredLines.add(line)
                line = file.readline()
            # See if the configuration is subsumed by any other
            alreadyOccurs = False
            for (config,coverage) in configs:
                newConfig = merge(thisConfig,config)
                if newConfig:
                    alreadyOccurs = True
                    # If we merged thisConfig with an existing configuration,
                    # union in the new coverage and replace the old (config,coverage) pair
                    configs[configs.index((config,coverage))] = (newConfig, coverage | coveredLines)
                    break
            if not alreadyOccurs: # Otherwise, add this as a new configuration
                configs.append((thisConfig,coveredLines))

print 'Total number of paths:',counter
print 'Doing greedy set cover over',len(configs),'configurations'

# Now do the greedy set cover

# Compute the universe
remainingToCover = set()
for (ignore,coverage) in configs:
    remainingToCover |= coverage
coveredSoFar = set()

chosenConfigs = []
while remainingToCover:
    score = -1
    chosenConfig = None
    for (config,coverage) in configs:
        newScore = len(coverage & remainingToCover)
        if newScore > score:
            score = newScore
            chosenConfig = config
            chosenCoverage = coverage
    assert chosenConfig
    chosenConfigs.append(chosenConfig)
    remainingToCover -= chosenCoverage
    coveredSoFar |= chosenCoverage
    print '\nConfiguration',len(chosenConfigs),'is\n'
    for variable,value in sorted(chosenConfig):
        print variable + '=' + str(value)
    print '\nThe total coverage so far is',len(coveredSoFar)

print '\nThat covers everything, where everything is:'

linesAsPairs = [str.split(':') for str in coveredSoFar]
for fileLinePair in sorted([(x[0],int(x[1])) for x in linesAsPairs]):
    print '%s:%d' % fileLinePair

print '\nThese variables were set in some configuration:'
varsEverSet = set()
for config,ignore in configs:
    varsEverSet |= set([x[0] for x in config])
for variable in sorted(varsEverSet):
    print variable

print '\nThese variables were set in one of the chosen configurations:'
varsSetInChosenConfigs = set()
for config in chosenConfigs:
    varsSetInChosenConfigs |= set([x[0] for x in config])
for variable in sorted(varsSetInChosenConfigs):
    print variable

print '\nAnd these were set somewhere, but not in any chosen configuration:'
for variable in sorted(varsEverSet - varsSetInChosenConfigs):
    print variable
