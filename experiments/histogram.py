# copy from aggregateCov.py
# calculate histogram of runs per test
import sys,zipfile,collections

if len(sys.argv) == 1:
    print 'Usage: python histogram.py FILE...'
    sys.exit(0)

configs = []
coverage = []
histogram = collections.defaultdict(int)

counter=0
sys.argv[:1] = []

# First, read in the coverage information from all of the files
for zipfilename in sys.argv:
    print 'Reading from',zipfilename
    zipfileObj = zipfile.ZipFile(zipfilename)
    outputFilename = zipfilename[zipfilename.rindex('/')+1:-4] + '-test'
    file = open(zipfileObj.extract(outputFilename,'/tmp'))
    line = 'x'
    nPaths = 0
    while line != '' and not line.startswith('STP was invoked'):
        line = file.readline()
        if line.startswith('Sample value'):
            counter += 1
            nPaths += 1
            thisConfig = set() # A configuration is a set of (variable,value) pairs
            # Read in the configuration
            line = file.readline()
            while line != '\n':
                #variable,value = line.split('=')
                #thisConfig.add((variable,int(value)))
                line = file.readline()
            coveredLines = set()
            # Read in the coverage
            line = file.readline() # Skip past 'The lines hit were:'
            line = file.readline()
            while line != '\n':
                #coveredLines.add(line)
                line = file.readline()
            # See if the configuration is subsumed by any other
            # alreadyOccurs = False
            # for (config,coverage) in configs:
            #     newConfig = merge(thisConfig,config)
            #     if newConfig:
            #         alreadyOccurs = True
            #         # If we merged thisConfig with an existing configuration,
            #         # union in the new coverage and replace the old (config,coverage) pair
            #         configs[configs.index((config,coverage))] = (newConfig, coverage | coveredLines)
            #         break
            # if not alreadyOccurs: # Otherwise, add this as a new configuration
            #     configs.append((thisConfig,coveredLines))
    histogram[nPaths] += 1

print 'Total number of paths:',counter
for (x,y) in sorted(histogram.items()):
    print x, y



# print 'Doing greedy set cover over',len(configs),'configurations'

# Now do the greedy set cover

# Compute the universe
# remainingToCover = set()
# for (ignore,coverage) in configs:
#     remainingToCover |= coverage
# coveredSoFar = set()
# 
# chosenConfigs = []
# while remainingToCover:
#     score = -1
#     chosenConfig = None
#     for (config,coverage) in configs:
#         newScore = len(coverage & remainingToCover)
#         if newScore > score:
#             score = newScore
#             chosenConfig = config
#             chosenCoverage = coverage
#     assert chosenConfig
#     chosenConfigs.append(chosenConfig)
#     remainingToCover -= chosenCoverage
#     coveredSoFar |= chosenCoverage
#     print '\nConfiguration',len(chosenConfigs),'is\n'
#     for variable,value in sorted(chosenConfig):
#         print variable + '=' + str(value)
#     print '\nThe total coverage so far is',len(coveredSoFar)
# 
# print '\nThat covers everything'
# 
# print '\nThese variables were set in some configuration:'
# varsEverSet = set()
# for config,ignore in configs:
#     varsEverSet |= set([x[0] for x in config])
# for variable in sorted(varsEverSet):
#     print variable
# 
# print '\nThese variables were set in one of the chosen configurations:'
# varsSetInChosenConfigs = set()
# for config in chosenConfigs:
#     varsSetInChosenConfigs |= set([x[0] for x in config])
# for variable in sorted(varsSetInChosenConfigs):
#     print variable
# 
# print '\nAnd these were set somewhere, but not in any chosen configuration:'
# for variable in sorted(varsEverSet - varsSetInChosenConfigs):
#     print variable
# 
# print '\nHere are all lines ever covered:'
# linesAsPairs = [str.split(':') for str in coveredSoFar]
# for fileLinePair in sorted([(x[0],int(x[1])) for x in linesAsPairs]):
#     print '%s:%d' % fileLinePair
# 
