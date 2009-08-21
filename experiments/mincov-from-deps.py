import sys,time

if len(sys.argv) == 1:
    print 'Usage: python mincov-from-deps.py <guaranteed coverage summary file>'
    sys.exit(0)

# Read lines from theFile, adding them to theSet (without the trailing
# '\n'), until reaching a line starting with stopStr.
def addLinesTo(setOrList,lineIterator,stopStr):
    line = lineIterator.next()
    try:
        while not line.startswith(stopStr):
            line = line[:-1] # Chop off trailing '\n'
            try: setOrList.add(line)
            except AttributeError: setOrList.append(line)
            line = lineIterator.next()
    except StopIteration: pass

#def line_cmp(line1,line2):
#    try:
#        [file1,no1] = line1.split(":")
#        [file2,no2] = line2.split(":")
#        if file1<file2: 
#            return -1
#        elif file1>file2: 
#            return 1
#        else:
#            return int(no1)-int(no2)
#    except ValueError:
#        print "Error:",line1, line2
#        raise ValueError

def merge(config1,config2):
    theUnion = config1 | config2
    varList = [x[0] for x in theUnion]
    # If they agree on all variables they share, they are compatible
    if len(varList) == len(set(varList)):
        return theUnion
    return None

def printConfigSet(configs):
    for config in configs:
        print ':'.join(['%s=%d' % x for x in sorted(config)])

# Map each frozenset of (variable,value) pairs to the set of lines guaranteed to be covered by that assignment
coverage = dict()

with open(sys.argv[1]) as f:
    file = f.xreadlines()
    # Look for 'Under the condition'
    line = file.next()
    while not line.startswith('Under'):
        line = file.next()

    # Go through the rest of the file
    try:
        while not line.startswith('Summary'): # We're done when we hit 'Summary'
            # Read in the assignment
            thisAssignment = []
            addLinesTo(thisAssignment,file,'these')
            # Convert the set from a list of strings to a frozenset of (var,val) pairs
            thisAssignment = [x.split('=') for x in thisAssignment]
            thisAssignment = [(x[0],int(x[1])) for x in thisAssignment]
            thisAssignment = frozenset(thisAssignment)
            # Now read in the coverage
            coveredLines = []
            addLinesTo(coveredLines,file,'\n')
            coveredLines = set(coveredLines)

            assert thisAssignment not in coverage,'We saw the same interaction twice!'

            # Record thisAssignment's coverage
            coverage[thisAssignment] = coveredLines

            # Advance to the next line, getting to 'Summary' or skipping 'Under the condition'
            line = file.next()
    except StopIteration:
        assert False,'Unexpected end of file'

print 'Merging interactions',time.asctime()

def mergeInteractions(interactions):
    '''Takes an iterable of interactions, merges compatible
    interactions, and returns a dictionary mapping each merged
    interaction to the set of its components'''
    setToComponents = dict()
    for interaction in interactions:
        for otherInteraction in list(setToComponents):
            mergedInteraction = merge(interaction,otherInteraction)
            if mergedInteraction:
                try:
                    clique = setToComponents[mergedInteraction]
                except KeyError:
                    clique = set()
                    setToComponents[mergedInteraction] = clique
                clique.add(interaction)
                clique.update(setToComponents[otherInteraction])
        try:
            setToComponents[interaction].add(interaction)
        except KeyError:
            setToComponents[interaction] = set([interaction])
    return setToComponents

mergedToComponents = mergeInteractions(coverage)
maximalConfigs = []

print len(mergedToComponents),'pseudo-maximal configs'

print 'Computing maximal configs',time.asctime()

for config in mergedToComponents:
    isMaximal = True
    for i in reversed(xrange(len(maximalConfigs))): # reversed so we can delete at index i
        if config < maximalConfigs[i]:
            isMaximal = False
            break
        if maximalConfigs[i] < config:
            del maximalConfigs[i]
    if isMaximal:
        maximalConfigs.append(config)

print len(maximalConfigs),'maximal configs'

theAnswer = None

allLines = set()
for lines in coverage.itervalues(): allLines |= lines

print 'Finding coverage for max configs',time.asctime()

# Find the coverage for the maximal configs
coverageForMaximals = {}
for maxConf in maximalConfigs:
    covered = set()
    for config in mergedToComponents[maxConf]:
        covered |= coverage[config]
    if covered == allLines:
        theAnswer = [maxConf]
        break
    coverageForMaximals[maxConf] = covered

print 'Computing set cover',time.asctime()



# I got these from http://code.activestate.com/recipes/500268/
def k_subsets_i(n, k):
    '''
    Yield each subset of size k from the set of intergers 0 .. n - 1
    n -- an integer > 0
    k -- an integer > 0
    '''
    # Validate args
    if n < 0:
        raise ValueError('n must be > 0, got n=%d' % n)
    if k < 0:
        raise ValueError('k must be > 0, got k=%d' % k)
    # check base cases
    if k == 0 or n < k:
        yield set()
    elif n == k:
        yield set(range(n))

    else:
        # Use recursive formula based on binomial coeffecients:
        # choose(n, k) = choose(n - 1, k - 1) + choose(n - 1, k)
        for s in k_subsets_i(n - 1, k - 1):
            s.add(n - 1)
            yield s
        for s in k_subsets_i(n - 1, k):
            yield s

def k_subsets(s, k):
    '''
    Yield all subsets of size k from set (or list) s
    s -- a set or list (any iterable will suffice)
    k -- an integer > 0
    '''
    s = list(s)
    n = len(s)
    for k_set in k_subsets_i(n, k):
        yield set([s[i] for i in k_set])

sys.setrecursionlimit(2*len(maximalConfigs))
for roundNumber in xrange(len(maximalConfigs)):
    if theAnswer: break
    print roundNumber+1, time.asctime()
    for subset in k_subsets(maximalConfigs,roundNumber+1):
        lines = set()
        for maxConfig in subset:
            lines |= coverageForMaximals[maxConfig]
        if lines == allLines:
            theAnswer = subset
            break

print 'Done',time.asctime()

printConfigSet(theAnswer)

## Dynamic programming set cover algorithm, but it uses too much memory
#nextRound = dict()
#for config,lines in coverageForMaximals.iteritems():
#    nextRound[frozenset([config])] = lines
#roundNumber = 1
#while not theAnswer:
#    roundNumber += 1
#    print roundNumber, time.asctime()
#    thisRound = nextRound
#    nextRound = {}
#    for config,lines in coverageForMaximals.iteritems():
#        for subset,lines2 in thisRound.iteritems():
#            theConfigs = subset | set([config])
#            theLines = lines | lines2
#            if theLines == allLines:
#                theAnswer = theConfigs
#                break
#            nextRound[theConfigs] = theLines
#        if theAnswer: break
#
#print 'Done',time.asctime()
#
#printConfigSet(theAnswer)
