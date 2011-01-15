# TODO: sort lines appropriately
import sys,os
from collections import defaultdict

if len(sys.argv) < 4:
	print '''Usage: python aggregateGuaranteedCoverage.py type count dir [possibleValues]
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

# calculate stat_coverage_inherited 
for (key,val) in sorted(stat_coverage.items(),key=lambda (k,v):len(k)):
	for (key0,val0) in sorted(stat_coverage.items(),key=lambda (k,v):len(k)):
		assert len(key0) <= len(key),"key0 can't be this long"
		if len(key0) == len(key): break
		if key0 < key: stat_coverage_inherited[key] |= (val0|stat_coverage_inherited[key0])
	stat_coverage[key] -= stat_coverage_inherited[key]


# present the results
#print "These %d variables are mentioned in the path conditions:" % len(stat_touched)
#for v in sorted(stat_touched):
#	print v
#print ""

stat_total_numconf = 0
stat_all_lines = set()
stat_tway_numconf = defaultdict(int)
stat_tway_all_lines = defaultdict(set)
stat_tway_max = defaultdict(int)
stat_tway_min = defaultdict(lambda:sys.maxint)

for (key,val) in sorted(stat_coverage.items(),key=lambda (k,v):-len(v)):
	if val==set(): continue
	configstr = ""
	for k in sorted(key): configstr+= k+","
	print "%d\t%d\t%s" % (len(val),len(key),configstr[:-1])

	stat_total_numconf += 1
	stat_all_lines |= val
	stat_tway_numconf[len(key)] += 1
	stat_tway_all_lines[len(key)] |= val
	stat_tway_max[len(key)] = max(stat_tway_max[len(key)],len(val))
	stat_tway_min[len(key)] = min(stat_tway_min[len(key)],len(val))
sys.exit(0)

def percentage(n): return (n,n*100.0/count)

gnuplot_str = ""
print "Summary"
# The set of touched variables
print "# touched variable: %d" % len(stat_touched)
print "# configs: %d" % stat_total_numconf
print "# %ss: %d (%0.2f%%)" % ((covtype,) + percentage(len(stat_all_lines)))
# for each t,
percent_acclines = 0.0
for t in sorted(stat_tway_numconf.keys()):
	percent_lines = percentage(len(stat_tway_all_lines[t]))
	percent_newlines = percentage(len(reduce(set.difference,[stat_tway_all_lines[n] for n in xrange(t)],stat_tway_all_lines[t])))
	percent_acclines += percent_newlines[1]
	print "t: %d" % t
	print "\t# configs: %d" % stat_tway_numconf[t]
	print "\t# %ss: %d (%0.2f%%)" % ((covtype,) + percent_lines)
	print "\t# new %ss:" % covtype, "%d (%0.2f%%)" % percent_newlines 
	print "\tmax: %d" % stat_tway_max[t]
	print "\tmin: %d" % stat_tway_min[t]
	print "\taverage: %.2f" % (float(len(stat_tway_all_lines[t]))/stat_tway_numconf[t])
	gnuplot_str += "%d\t%0.2f\t%0.2f\n" % (t,percent_lines[1],percent_acclines)
print ""

print "gnuplot data:"
print "#t\tind\tacc"
print gnuplot_str
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
print ""

# untouched variables are free to accept any values
#print "Untouched variables:"
if len(sys.argv)==5:
	possible_values = dict()
	for line in open(sys.argv[4],"r"):
		sline = line.split()
		possible_values[sline[0]] = sline[1:]
	for key in set(possible_values.keys())-stat_touched:
		for v1 in possible_values[key]:
			for v2 in possible_values[key]:
				if v1==v2: continue
				print "Can replace %s=%s with %s=%s" % (key, v1,key,v2)

# starting from this point, a line/edge/block/cond is covered by only one config (i.e., remove duplicates)
stat_coverage_nodup = defaultdict(set)

for (key,val) in sorted(stat_coverage.items(),key=lambda (k,v):len(k)):
	netval = val & stat_all_lines
	if netval==set(): continue
	stat_coverage_nodup[key] = netval
	stat_all_lines -= netval

print "# configs (without duplication): %d" % len(stat_coverage_nodup)

for aas in sorted(stat_coverage_nodup.keys(),key=lambda x:-len(x)):
	line=""
	for a in sorted(aas):
		line += a+":"
	print line[:-1]

# sys.exit(0)
# # work in progress
# def compatible(tar,src):
# 	for (k,v) in tar.items():
# 		if k in src and src[k]!=v: return False
# 	return True
# def merge(tar,src):
# 	for (k,v) in src.items():
# 		tar[k] = v
# 
# def try_pack(config,size,buckets):
# 	if config==[]: return (True,buckets)
# 	for i in range(1,size+1):
# 		if compatible(buckets[i],config[0]):
# 			print "Size: %d\tDepth: %d\t try bucket: %d" % (size,len(config),i)
# 			newbuckets = buckets.copy()
# 			merge(newbuckets[i],config[0])
# 			(found,retbuckets) = try_pack(config[1:],size,newbuckets)
# 			if found: return (found,retbuckets)
# 	return (False,buckets)
# 
# def set2dict(f):
# 	d = dict()
# 	for a in f: d[a.split("=")[0]] = a.split("=")[1]
# 	return d
# 
# config_to_pack = sorted([set2dict(f) for f in stat_coverage_nodup.keys()],key=lambda x: -len(x))
# for size in range(1,len(config_to_pack)+1):
# 	print "Try size = %d" % size
# 	(found,buckets) = try_pack(config_to_pack,size,defaultdict(dict))
# 	if found:
# 		for (k,v) in sorted(buckets.items()):
# 			print k, v
# 		break
		
	
print "Done"


