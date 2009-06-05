import sys,os,re
from collections import defaultdict

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

def line_cmp(line1,line2):
	try:
		[file1,no1] = line1.split(":")
		[file2,no2] = line2.split(":")
		if file1<file2: 
			return -1
		elif file1>file2: 
			return 1
		else:
			return int(no1)-int(no2)
	except ValueError:
		print "Error:",line1, line2
		raise ValueError


line_count = int(sys.argv[1])
dir = sys.argv[2:]


def print_size(name,len,print_percent=True):
	typ = "%d"
	if type(len) is float: typ = "%.2f"
	if print_percent and line_count>0:
		print ("%s="+typ+"(%.2f%%)") % (name,len,float(len)*100/line_count)
	else:
		print ("%s="+typ) % (name,len)


# All variable touched by any path condition
variables = []
# Map each frozenset of (variable,value) pairs to the set of lines guaranteed to be covered by that assignment
# Start by mapping the empty assignment to an empty set of lines
emptyAssignment = []
coverage = []
t_coverage = []
zero_coverage = set()
total_coverage = set()

# for each t
for t in range(0,len(dir)):
	variables.append(None)
	emptyAssignment.append(None)
	coverage.append(None)
	t_coverage.append(None)

	#local_coverage = set()

	# All variable touched by any path condition
	variables[t] = set()
	# Map each frozenset of (variable,value) pairs to the set of lines guaranteed to be covered by that assignment
	# Start by mapping the empty assignment to an empty set of lines
	emptyAssignment[t] = frozenset()
	coverage[t] = { emptyAssignment[t] : set() }
	t_coverage[t] = set()

	print "--------------------------------------------------"
	print 'Reading directory',dir[t]
	print ""
	for filename in os.listdir(dir[t]):
		#print 'Reading from',filename
		if not filename.endswith(".deps"):
			continue
		file = open(dir[t] + '/' + filename)
		line = file.readline() # 'These n variables[t]'
		while not line.startswith("These "):
			line = file.readline() # 'These n variables[t]'
			
		# Read in the variables[t]
		addLinesTo(variables[t],file,'\n')
	
		line = file.readline() # 'n lines always executed'
		# Read in the lines which are always covered in all executions.
		# These lines are covered by the empty assignment
		addLinesTo(coverage[t][emptyAssignment[t]],file,'\n')
		zero_coverage |= coverage[t][emptyAssignment[t]]
	
		# Go through the rest of the file
		while line != '':
			line = file.readline()
			if line.startswith('Under'): # This is beginning of one assignment's coverage[t]
				# Read in the assignment
				thisAssignment = []
				addLinesTo(thisAssignment,file,'these ')
				# Convert the set from a list of strings to a frozenset of (var,val) pairs
				#thisAssignment = [x.split('=') for x in thisAssignment]
				thisAssignment = [re.split(r'[=,]',x) for x in thisAssignment]
				thisAssignment = [(x[0],int(x[1])) for x in thisAssignment]
				thisAssignment = frozenset(thisAssignment)
				# Now read in the coverage[t]
				coveredLines = []
				addLinesTo(coveredLines,file,'\n')
				t_coverage[t] |= set(coveredLines)
				# Add the coverage[t] to thisAssignment's coverage[t]
				try:
					coverage[t][thisAssignment].update(coveredLines)
				except KeyError:
					coverage[t][thisAssignment] = set(coveredLines)

	t_coverage[t] -= zero_coverage
	for tt in range(0,t):
		t_coverage[t] -= t_coverage[tt]
	
	total_coverage |= t_coverage[t]

	#print
	print 'All variables[t] ever touched:'
	for var in sorted(variables[t]):
		print var
	
	print
	print 'Lines always executed (i.e., there is *some* test under which the line is always executed):'
	for line in sorted(coverage[t][emptyAssignment[t]],line_cmp):
		print line
	
	#print
	#print 'Lines covered by t-way but not (t-1)-way:'
	#for line in sorted(t_coverage[t]-zero_coverage,line_cmp):
	#	print line

	cov_stat = []
	allway_cov_stat = defaultdict(lambda:(0,set(),0,line_count))
	max_t = 0
	
	for assignment,lines in sorted(coverage[t].iteritems()):
		len_of_assignment = len(assignment)
		if max_t<len_of_assignment:
			max_t = len_of_assignment

	for tk in range(1,max_t+1):
		for assignment,lines in sorted(coverage[t].iteritems()):
			if assignment == emptyAssignment[t]:
				continue
			if len(assignment) != tk:
				continue
			lines -= zero_coverage
			for tt in range(0,t):
				lines -= t_coverage[tt]
			if len(lines)==0:
				continue
			#for tt in range(1,tk-1):
			for tt in range(1,tk+1):
				lines -= allway_cov_stat[tt][1]
			if len(lines)==0:
				continue
			print
			print 'Under the condition'
			for varVal in sorted(assignment):
				print '%s=%d' % varVal
			print 'these',len(lines),'lines are hit'
			cov_stat.append(len(lines))
			for line in sorted(lines,line_cmp):
				print line
			(count,s,MAX,MIN) = allway_cov_stat[len(assignment)] 
			allway_cov_stat[len(assignment)] = (count+1,s|lines,max([MAX,len(lines)]),min([MIN,len(lines)]))

	
	print "\nSummary:"
	print_size("TOUCHED_VAR",len(variables[t]),False)
	print_size("%LINE 0-way",len(coverage[t][emptyAssignment[t]]))
	for tk in allway_cov_stat.keys(): 
		(count,s,MAX,MIN) = allway_cov_stat[tk]
		print "%d-way:" % tk
		print_size("  %LINE",len(s))
		print_size ("  #config", (count),False)
		print_size ("  MAX", MAX)
		print_size ("  MIN", MIN)
		print_size ("  AVG", float(len(s))/count)
	print "\nDone\n"
	
# END for each t
total_coverage |= zero_coverage 
print
print "--------------------------------------------------"
print 'Lines covered by all ways:'
print_size ("Lines total",len(total_coverage))
for line in sorted(total_coverage,line_cmp):
	print line
