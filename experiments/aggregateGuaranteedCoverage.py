# TODO: sort lines appropriately
import sys,os
from collections import defaultdict

if len(sys.argv) != 4:
	print '''Usage: python aggregateGuaranteedCoverage.py type count dir
type : line | edge | block | condition
count: how many executable items (-1 if not sure)
dir: the directory that stores the output of calculateVarDeps'''
	sys.exit(0)

LINE_COMMAND_EXIT_CODE = "command exit code:"

covtype  = sys.argv[1]
count = int(sys.argv[2])
dir   = sys.argv[3]

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

for filename in os.listdir(dir):
	if not filename.endswith(".deps"):
		continue
	print "Reading %s" % filename
	file = open(dir + '/' + filename)
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
print ""

# calculate stat_coverage_inherited 
for (key,val) in sorted(stat_coverage.items(),key=lambda (k,v):len(k)):
	for x in key:
		key0 = key-set([x])
		stat_coverage_inherited[key] |= (stat_coverage[key0]|stat_coverage_inherited[key0])
	stat_coverage[key] -= stat_coverage_inherited[key]


# present the results
print "These %d variables are mentioned in the path conditions:" % len(stat_touched)
for v in sorted(stat_touched):
	print v
print ""

stat_total_numconf = 0
stat_all_lines = set()
stat_tway_numconf = defaultdict(int)
stat_tway_all_lines = defaultdict(set)
stat_tway_max = defaultdict(int)
stat_tway_min = defaultdict(lambda:sys.maxint)

for (key,val) in sorted(stat_coverage.items(),key=lambda (k,v):len(k)):
	if val==set(): continue
	print "Under the condition"
	for k in sorted(key): print k
	print "these %d %ss are hit" % (len(val),covtype)
	for v in sorted(val): print v

	print ""

	stat_total_numconf += 1
	stat_all_lines |= val
	stat_tway_numconf[len(key)] += 1
	stat_tway_all_lines[len(key)] |= val
	stat_tway_max[len(key)] = max(stat_tway_max[len(key)],len(val))
	stat_tway_min[len(key)] = min(stat_tway_min[len(key)],len(val))
print ""

def percentage(n): return (n,n*100.0/count)

print "Summary"
# The set of touched variables
print "# touched variable: %d" % len(stat_touched)
print "# configs: %d" % stat_total_numconf
print "# %ss: %d (%0.2f%%)" % ((covtype,) + percentage(len(stat_all_lines)))
# for each t,
for t in sorted(stat_tway_numconf.keys()):
	print "t: %d" % t
	print "\t# configs: %d" % stat_tway_numconf[t]
	print "\t# %ss: %d (%0.2f%%)" % ((covtype,) + percentage(len(stat_tway_all_lines[t])))
	print "\tmax: %d" % stat_tway_max[t]
	print "\tmin: %d" % stat_tway_min[t]
	print "\taverage: %.2f" % (float(len(stat_tway_all_lines[t]))/stat_tway_numconf[t])
print ""

# check for redudant assignment
for a0 in sorted(stat_assignment):
	for a1 in sorted(stat_assignment):
		if a0==a1: continue
		if a0.split("=")[0]!=a1.split("=")[0]: continue
		# check if replace a0 by a1 would not decrease any coverage
		can_replace = True
		for conf in stat_coverage.keys():
			if a0 not in conf: continue
			new_conf = (conf - set([a0])) | set([a1]) 
			if new_conf not in stat_coverage or stat_coverage[conf]-stat_coverage[new_conf]!=set():
				can_replace = False
				break
		if can_replace:
			print "Can replace %s with %s" % (a0,a1)

print "Done"

