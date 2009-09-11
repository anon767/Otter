import sys,collections,copy

if len(sys.argv) == 1:
    print 'Usage: python mincovHelper.py <guaranteed coverage summary file> [gnuplot_output]'
    sys.exit(0)

# Map interactions to their coverage
interactionsToCoverage = collections.defaultdict(set)
# Map each coverage unit to all interactions that cover it
coveredBy = collections.defaultdict(set)
# gnuplot output
gnuplot_num_covered = 0
gnuplot_num_total = 1
gnuplot_output = ""
gnuplot_output_count = 0
if len(sys.argv)>3:
	gnuplot_num_total = int(sys.argv[3])

def configToString(config):
    return ':'.join(['%s=%d' % x for x in sorted(config)])

def printConfigs(configs):
    i = 1
    for config in configs:
        print i,configToString(config)
        i += 1
    print

def printConfigsWithCoverage(configCoveragePairs):
    global gnuplot_num_covered 
    global gnuplot_num_total 
    global gnuplot_output 
    global gnuplot_output_count 
    coveredSoFar = set()
    inputOrderedByCoverageAmount = []
    for i in xrange(len(configCoveragePairs)):
        best = None
        bestScore = -1
        for config,cov in configCoveragePairs:
            newCov = cov - coveredSoFar
            score = len(newCov)
            if score > bestScore:
                best = config,newCov
                bestScore = score
        coveredSoFar |= best[1]
        inputOrderedByCoverageAmount.append(best)

    for config,newCov in inputOrderedByCoverageAmount:
        print 'Under the condition'
        print '\n'.join(['%s=%d' % x for x in sorted(config)])
        print 'these %d are covered' % len(newCov)
	gnuplot_output_count += 1
	gnuplot_num_covered += len(newCov)
	gnuplot_output += "%d\t%.2f\n" % ( gnuplot_output_count, float(gnuplot_num_covered)/gnuplot_num_total)
        for item in sorted(newCov): print item
        print

# Print what configSet leaves uncovered, and print out all
# configurations that would cover what's left.
def printUncovered(covSet):
    uncovered = set(coveredBy.keys())
    for covered in covSet:
        uncovered -= covered
    if uncovered:
        # This is a set of sets of configurations. To get full
        # coverage, you need a representative element from each set.
        configSets = set()
        print "These %d aren't covered:" % len(uncovered)
        for i in sorted(uncovered):
            print i
            configSets.add(coveredBy[i])
        # We can filter out supersets from configSets because saying
        # that you need to choose one of {A,B,C} implies that you will
        # have chosen one of {A,B,C,D,E}
        filteredConfigSets = set()
        configSets = list(configSets)
        for i in xrange(len(configSets)):
            ithHasSubset = False
            for j in xrange(i+1,len(configSets)):
                if configSets[i] > configSets[j]:
                    ithHasSubset = True
                    break
            if not ithHasSubset:
                filteredConfigSets.add(configSets[i])
        print 'To cover these, choose at least one from each of the following:\n'
        for configSet in filteredConfigSets:
            printConfigs(configSet)
    else:
        print 'Everything is covered!'
        print

# If two configurations are compatible, return the merged
# configuration. Otherwise, return None.
def merge(configAndCov1,configAndCov2):
    config1,cov1 = configAndCov1
    config2,cov2 = configAndCov2
    union = config1 | config2
    varList = [x[0] for x in union]
    # If they agree on all variables they share, they are compatible
    if len(varList) == len(set(varList)):
        return union,(cov1 | cov2)
    return None

def getInteractionsWithCoverages(file):
    while True:
        line = file.next()
        while not line.startswith('Under '):
            if line == 'Summary\n': return
            line = file.next()
        # line == 'Under the condition'
        # Now read in the configuration
        interaction = set()
        line = file.next()
        while not line.startswith('these '):
            varVal = line[:-1].split('=')
            interaction.add( (varVal[0],int(varVal[1])) )
            line = file.next()
        interaction = frozenset(interaction)
        # line == 'these n ...s are hit'
        # Now read in the coverage
        line = file.next()[:-1]
        while line != '':
            interactionsToCoverage[interaction].add(line)
            coveredBy[line].add(interaction)
            line = file.next()[:-1]
        # Add the coverage from the subsets, too
        for previousInteraction,cov in interactionsToCoverage.iteritems():
            if previousInteraction < interaction:
                interactionsToCoverage[interaction] |= cov

with open(sys.argv[1]) as f:
    # Fill in interactionsToCoverage and coveredBy
    getInteractionsWithCoverages(f.xreadlines())

# Make coveredBy map to frozensets (instead of sets)
for key,val in coveredBy.iteritems():
    coveredBy[key] = frozenset(coveredBy[key])

def greedy():
    uncovered = set(coveredBy.keys())
    answer = []
    while uncovered:
        best = None
        bestScore = -1
        for interaction,coverage in interactionsToCoverage.iteritems():
            score = len(coverage & uncovered)
            if score > bestScore:
                best = interaction,coverage
                bestScore = score
        uncovered -= best[1]
        for i in xrange(len(answer)):
            mergedConfig = merge(answer[i],best)
            if mergedConfig:
                answer[i] = mergedConfig
                best = None # We don't need best anymore; it got merged in
                break
        if best:
            answer.append(best)
    return answer

greedyOutput = greedy()
print 'STEP1'
print 'Greedy algorithm output:'
printConfigs([x[0] for x in greedyOutput])
printConfigsWithCoverage(greedyOutput)
printUncovered([x[1] for x in greedyOutput])
print "(If that didn't just say that everything is covered, something is wrong.)"

if len(sys.argv)>2:
	gnuplot_output_file = open(sys.argv[2],"w")
	print >>gnuplot_output_file, gnuplot_output
	gnuplot_output_file.close()

print "DONE"
sys.exit(0)

def getOne(s): return s.__iter__().next()

# Find all interactions that guarantee something unique.
# Each of these must be in a minimum covering set.
interactionsWithUniqueCoverage = [getOne(x) for x in coveredBy.itervalues() if len(x) == 1]
# Remove duplicates, but leave it a list
interactionsWithUniqueCoverage = list(set(interactionsWithUniqueCoverage))

# We don't really need *all* these interactions, though; we can drop
# subsets. That is, if we need [x=0,y=0], we don't need to explicitly
# say that we need [x=0] or [y=0] alone---their coverage will be
# covered by the superset.
# Since we want to delete as we go, we want to traverse the list from
# the end; so we place the largest configurations at the end.

# Sort by increasing interaction size
interactionsWithUniqueCoverage.sort(key=len)

# Traverse the list backward so that we can delete things as we go.
for i in reversed(xrange(len(interactionsWithUniqueCoverage))):
    # Find all supersets of the i-th configuration
    foundSuperset = False
    for j in xrange(i,len(interactionsWithUniqueCoverage)):
        if interactionsWithUniqueCoverage[i] < interactionsWithUniqueCoverage[j]:
            foundSuperset = True
            # Attribute the coverage to the superset
            interactionsToCoverage[interactionsWithUniqueCoverage[j]].update(
                interactionsToCoverage[interactionsWithUniqueCoverage[i]])
    if foundSuperset:
        del interactionsWithUniqueCoverage[i]

print 'STEP2'
print 'interactionsWithUniqueCoverage:'
printConfigs(interactionsWithUniqueCoverage)
printUncovered([interactionsToCoverage[x] for x in interactionsWithUniqueCoverage])





def greedy():
    answer = []
    for interaction in interactionsWithUniqueCoverage:
        current = (interaction,interactionsToCoverage[interaction])
        for i in xrange(len(answer)):
            mergedConfig = merge(answer[i],current)
            if mergedConfig:
                answer[i] = mergedConfig
                current = None
                break
        if current:
            answer.append(current)
    return answer

greedyOutput = greedy()
print 'STEP3'
print 'Greedily merging those configurations gives:'
printConfigs([x[0] for x in greedyOutput])





# For some reason, putting the smallest configuration last (thereby
# trying to insert it first) seems to make this work much faster
interactionsWithUniqueCoverage.reverse()

# Find a minimum size set of configurations that includes all the
# interactionsWithUniqueCoverage, and see if that set happens to
# completely cover the program.
print 'STEP4'
def bruteforce(bucket,used,n):
    if n<0:
        # success
        printConfigs([x[0] for x in bucket])
        printUncovered([x[1] for x in bucket])
        sys.exit(0)
    for i in xrange(min(len(bucket),used+1)):
        newthing = merge((interactionsWithUniqueCoverage[n],interactionsToCoverage[interactionsWithUniqueCoverage[n]]),bucket[i])
        if newthing!=None:
            hasused = 0
            if i == used: hasused = 1
            newbucket = copy.deepcopy(bucket)
            newbucket[i] = newthing
            bruteforce(newbucket,used+hasused,n-1)

for t in range(1,len(interactionsWithUniqueCoverage)+1):
	bucket = [(set(),set())]*t
	bruteforce(bucket,0,len(interactionsWithUniqueCoverage)-1)
	print t, "fails"
