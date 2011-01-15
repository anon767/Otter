# TODO: sort lines appropriately
import sys,os
from collections import defaultdict

if len(sys.argv) < 2:
	print '''Usage: python aggregateGuaranteedCoverage.py <dir or file> [threshold]
<dir or file>: the directory that stores the output of calculateVarDeps, or a single file with this output
threshold: fraction of coverage at which to stop printing; if omitted, print all interactions'''
	sys.exit(0)

LINE_COMMAND_EXIT_CODE = "command exit code:"

dir   = sys.argv[1]
if len(sys.argv) > 2:
	threshold = float(sys.argv[2])
	assert 0 < threshold <= 1, 'threshold must be > 0 and <= 1'
else:
	threshold = 2 # Set this greater than 1 so that we print *all* interactions, even those with duplicate coverage

stat_coverage = defaultdict(set)
stat_coverage_inherited = defaultdict(set)
stat_touched = set()
stat_assignment = set()

def getlines(f):
	s = set()
	while True:
		line = f.readline()
		if line=="\n": break
		elif LINE_COMMAND_EXIT_CODE in line: 
			print "\tWarning: I see the line \"%s\"!" % LINE_COMMAND_EXIT_CODE
			continue
		else: s.add(line[:-1])
	return s

def process_file(filename):
	global stat_touched
	global stat_coverage
	global stat_assignment

	#print "Reading %s" % filename
	file = open(filename)
	while True:
		line = file.readline()
		# t=0
		if "always executed:" in line:
			stat_coverage[frozenset()] |= getlines(file)	
		# t>0
		elif "Under the condition" in line:
			key = set()
			while True:
				line = file.readline()
				if line=="": raise Error
				elif "are hit" in line:
					stat_coverage[frozenset(key)] |= getlines(file)
					break
				else:
					key.add(line[:-1])
					stat_assignment.add(line[:-1])
		# touched variables
		elif "variables are mentioned in the path conditions:" in line:	
			stat_touched |= getlines(file)
		elif line=="": 
			break

# single file
if dir.endswith(".deps"):
	process_file(dir)
# multiple files in a dir
else:
	for filename in os.listdir(dir):
		if not filename.endswith(".deps"):
			continue
		process_file(dir + '/' + filename)

# Remove inherited coverage
for (key,val) in sorted(stat_coverage.items(),key=lambda (k,v):len(k)):
	for (key0,val0) in sorted(stat_coverage.items(),key=lambda (k,v):len(k)):
		assert len(key0) <= len(key),"key0 can't be this long"
		if len(key0) == len(key): break
		if key0 < key: stat_coverage_inherited[key] |= (val0|stat_coverage_inherited[key0])
	stat_coverage[key] -= stat_coverage_inherited[key]

totalNum = len(reduce(set.union,stat_coverage.itervalues()))

print '%.2f * %d = %.0f' % (threshold,totalNum,totalNum*threshold)

linesSoFar = stat_coverage[frozenset()]

for (key,val) in sorted(stat_coverage.items(),key=lambda (k,v):-len(v)):
	if val==set():
		# We may reach an empty set if we are printing all interactions,
		# but we shouldn't if we have a real threshold
		if threshold == 2: break
		else: assert False,'val is empty'
	if key==set(): continue

	print "%d\t%d\t%d" % (len(linesSoFar),len(val),len(key))
	print 'Under\n','\n'.join(sorted(key)),'\nthese'

	linesSoFar |= val
	if len(linesSoFar) >= totalNum * threshold:
		print len(linesSoFar)
		break
