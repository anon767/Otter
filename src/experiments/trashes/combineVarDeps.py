import sys,os,re

#todo: look for: "User interrupt!"

if len(sys.argv) == 1:
	print '''Usage: python combineVarDeps.py dir
dir: the directory that stores the output of calculateVarDeps'''
	sys.exit(0)

# All variable touched by any path condition
variables = set()
# Map each frozenset of (variable,value) pairs to the set of lines guaranteed to be covered by that assignment
# Start by mapping the empty assignment to an empty set of lines
emptyAssignment = frozenset()
coverage = { emptyAssignment : set() }

dir = sys.argv[1]

# Read lines from theFile, adding them to theSet (without the trailing
# '\n'), until reaching a line starting with stopStr.
def addLinesTo(setOrList,theFile,stopStr):
	line = theFile.readline()
	while line != '' and not line.startswith(stopStr):
		line = line[:-1] # Chop off trailing '\n'
		try: setOrList.add(line)
		except AttributeError: setOrList.append(line)
		line = theFile.readline()

for filename in os.listdir(dir):
#	print 'Reading from',filename
	file = open(dir + '/' + filename)
	file.readline() # 'These n variables'
	# Read in the variables
	addLinesTo(variables,file,'\n')

	line = file.readline() # 'n lines always executed'
	# Read in the lines which are always covered in all executions.
	# These lines are covered by the empty assignment
	addLinesTo(coverage[emptyAssignment],file,'\n')

	# Go through the rest of the file
	while line != '':
		line = file.readline()
		if line.startswith('Under'): # This is beginning of one assignment's coverage
			# Read in the assignment
			thisAssignment = []
			addLinesTo(thisAssignment,file,'these ')
			# Convert the set from a list of strings to a frozenset of (var,val) pairs
			thisAssignment = [x.split('=') for x in thisAssignment]
			thisAssignment = [(x[0],int(x[1])) for x in thisAssignment]
			thisAssignment = frozenset(thisAssignment)
			# Now read in the coverage
			coveredLines = []
			addLinesTo(coveredLines,file,'\n')
			# Add the coverage to thisAssignment's coverage
			try:
				coverage[thisAssignment].update(coveredLines)
			except KeyError:
				coverage[thisAssignment] = set(coveredLines)

#print
print 'All variables ever touched:'
for var in sorted(variables):
	print var

print
print 'Lines always executed (i.e., there is *some* test under which the line is always executed):'
for line in sorted(coverage[emptyAssignment]):
	print line

for assignment,lines in sorted(coverage.iteritems()):
	if assignment == emptyAssignment:
		continue
	print
	print 'Under the condition'
	for varVal in sorted(assignment):
		print '%s=%d' % varVal
	print 'these',len(lines),'lines are hit'
	for line in sorted(lines):
		print line
