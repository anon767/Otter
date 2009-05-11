import sys,os,re

if len(sys.argv) == 1:
	print '''Usage: python combineVarDeps.py linecount dir1 [dir2 ...]
linecount: how many executable lines (-1 if not sure)
dir: the directory that stores the output of calculateVarDeps'''
	sys.exit(0)


# Read lines from theFile, adding them to theSet (without the trailing
# '\n'), until reaching a line starting with stopStr.
def addLinesTo(setOrList,theFile,stopStr):
	line = theFile.readline()
	while line != '' and not line.startswith(stopStr):
		line = line[:-1] # Chop off trailing '\n'
		try: setOrList.add(line)
		except AttributeError: setOrList.append(line)
		line = theFile.readline()


line_count = int(sys.argv[1])
dir = sys.argv[2:]


def print_size(name,len,print_percent=True):
	if print_percent and line_count>0:
		print "%s=%d(%.2f%%)" % (name,len,float(len)*100/line_count)
	else:
		print "%s=%d" % (name,len)


# All variable touched by any path condition
variables = []
# Map each frozenset of (variable,value) pairs to the set of lines guaranteed to be covered by that assignment
# Start by mapping the empty assignment to an empty set of lines
emptyAssignment = []
coverage = []

global_coverage = set()

# for each t
for t in range(0,len(dir)):
	variables.append(None)
	emptyAssignment.append(None)
	coverage.append(None)

	local_coverage = set()

	# All variable touched by any path condition
	variables[t] = set()
	# Map each frozenset of (variable,value) pairs to the set of lines guaranteed to be covered by that assignment
	# Start by mapping the empty assignment to an empty set of lines
	emptyAssignment[t] = frozenset()
	coverage[t] = { emptyAssignment[t] : set() }

	print "--------------------------------------------------"
	print 'Reading directory',dir[t]
	print ""
	for filename in os.listdir(dir[t]):
		#print 'Reading from',filename
		file = open(dir[t] + '/' + filename)
		file.readline() # 'These n variables[t]'
		# Read in the variables[t]
		addLinesTo(variables[t],file,'\n')
	
		line = file.readline() # 'n lines always executed'
		# Read in the lines which are always covered in all executions.
		# These lines are covered by the empty assignment
		addLinesTo(coverage[t][emptyAssignment[t]],file,'\n')
	
		# Go through the rest of the file
		while line != '':
			line = file.readline()
			if line.startswith('Under'): # This is beginning of one assignment's coverage[t]
				# Read in the assignment
				thisAssignment = []
				addLinesTo(thisAssignment,file,'these ')
				# Convert the set from a list of strings to a frozenset of (var,val) pairs
				thisAssignment = [x.split('=') for x in thisAssignment]
				thisAssignment = [(x[0],int(x[1])) for x in thisAssignment]
				thisAssignment = frozenset(thisAssignment)
				# Now read in the coverage[t]
				coveredLines = []
				addLinesTo(coveredLines,file,'\n')
				coveredLines = list(set(coveredLines)-global_coverage)
				local_coverage |= set(coveredLines)
				# Add the coverage[t] to thisAssignment's coverage[t]
				try:
					coverage[t][thisAssignment].update(coveredLines)
				except KeyError:
					coverage[t][thisAssignment] = set(coveredLines)

	global_coverage |= local_coverage

	#print
	print 'All variables[t] ever touched:'
	for var in sorted(variables[t]):
		print var
	
	print
	print 'Lines always executed (i.e., there is *some* test under which the line is always executed):'
	for line in sorted(coverage[t][emptyAssignment[t]]):
		print line
	
	cov_stat = []
	
	for assignment,lines in sorted(coverage[t].iteritems()):
		if assignment == emptyAssignment[t]:
			continue
		print
		print 'Under the condition'
		for varVal in sorted(assignment):
			print '%s=%d' % varVal
		print 'these',len(lines),'lines are hit'
		cov_stat.append(len(lines))
		for line in sorted(lines):
			print line
	
	print "\nSummary:"
	print_size("TOUCHED_VAR",len(variables[t]),False)
	print_size("%LINE_0way",len(coverage[t][emptyAssignment[t]]))
	print_size ("LEN", len(cov_stat),False)
	print_size ("MAX", max(cov_stat))
	print_size ("MIN", min(cov_stat))
	print_size ("SUM", sum(cov_stat))
	print_size ("AVG", float(sum(cov_stat))/len(cov_stat))
	print "\nDone\n"
	
# END for each t
