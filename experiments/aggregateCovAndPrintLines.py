import sys,zipfile,os,re,psyco
psyco.full()

#todo: look for: "User interrupt!"

if len(sys.argv) == 1:
	print '''Usage: python aggregateCovAndPrintLines.py option covtype [keyword] dir
dir: the directory that stores *.zip
option:
-0 : don't do greedy algorithm (only print lines covered and scattered graph only)
-1 : greedy algorithm, use method 1
-2 : greedy algorithm, use method 2 
<covtype> : line | edge | condition | block
<keyword> : only tests that contains <keyword> in the header will be processed (useful for distinguish t-way)'''
	sys.exit(0)

configs = []
coverage = []
lines_covered = set()
statistics = []

totalNumPaths=0
calculate = True

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

def get_covtype(line):
	# return line | edge | cond | none
	if not line.endswith("coverage:\n"):
		return "none"
	else:
		return  line.split(" ")[0].lower()



if sys.argv[1] == '-2':
	merge = merge2
elif sys.argv[1] == '-1':
	merge = merge1
else:
	calculate = False

covtype = sys.argv[2]
print "Coverage type: %s" % covtype

sys.argv[:3] = []

keyword = ""
#interrupt = "User interrupt!"
interrupt = "Timed out!"

if len(sys.argv) == 2:
	keyword = sys.argv[0]
	sys.argv[:1] = []

dir = sys.argv[0]
temp_dir = '/fs/skoll/symexe/data/.tmp'

# First, read in the coverage information from all of the files
for zipfilename in os.listdir(dir):
	if zipfilename[-5:]=="-test":
		print 'Reading from -test',zipfilename
		file = open(dir+"/"+zipfilename)
	elif zipfilename[-4:]==".zip":
		zipfilename = dir+'/'+zipfilename
		print 'Reading from',zipfilename,
		zipfileObj = zipfile.ZipFile(zipfilename)
		outputFilename = zipfilename[zipfilename.rindex('/')+1:-4] + '-test'
		filepath = temp_dir+"/"+outputFilename
		if os.path.isfile(filepath):
			print "\t","[previously unzipped]"
			file = open(filepath)
		else:
			file = open(zipfileObj.extract(outputFilename,temp_dir))
			print "\t","[unzipped]"
	else:
		continue
	line = 'x'


	while True: 
		need_process = False
		while line != '' and not line.startswith('STP was invoked'):
			line = file.readline()
			#if line.startswith("Running Test"):      #  Edit this and the line below to suit your need
			#	print line,
			#if line.startswith("Running Macro"):     #
			#	print line,
			if line.find(interrupt)>=0:
				print '(Warning: test interrupted)'
			if line.find(keyword)>=0:
				need_process = True
		line_stp_invoked = line
		line_time_spent = file.readline()
		while not 'paths ran' in line:
			line = file.readline()
			assert line!='', "All paths had errors"
		matchResult = re.match(r'(\d+).*(\d+) had errors',line)
		assert matchResult.group(2) == '0', 'Test had errors!'+zipfilename
		nPaths = int(matchResult.group(1))
		totalNumPaths += nPaths
		
		# spot for <covtype> coverage
		while(line!='' and not get_covtype(line)==(covtype)):
			line = file.readline()
		if line=='':
			print "Error in reading %s coverage" % covtype


		line = file.readline()
		while line != '' and get_covtype(line)==('none'):
			line = file.readline()
			if line.startswith('Sample value'):
				if not need_process:
					break
				thisConfig = set() # A configuration is a set of (variable,value) pairs
				# Read in the configuration
				line = file.readline()
				while line != '\n':
					if line.startswith("but these"):
						line = file.readline(); # the symbols
						break
					variable,value = line.split('=')
					thisConfig.add((variable,int(value)))
					line = file.readline()
				coveredLines = set()
				# Read in the coverage
				line = file.readline() # Skip past '* out of * *'
				line = file.readline() # Skip past '\n'
				line = file.readline() # Skip past 'The lines hit were:'
				line = file.readline()
				while line != '\n':
					if line.find("command exit code")<0:
						coveredLines.add(line)
					line = file.readline()
				# total lines covered
				lines_covered |= coveredLines;
				# See if the configuration is subsumed by any other
				if calculate:
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
	
		#if line=='':
		#	break
		#if need_process:
		#	print '[Done]'
		#else:
		#	print '[Omitted]'
		if need_process:
			# (nPaths) is the # of paths
			stp_invoked = int(re.match(r"STP was invoked (\S+) times",line_stp_invoked).group(1))
			time_spent = float(re.match(r"It ran for \S+ s, which is \S+ of the total (\S+) s execution.",line_time_spent).group(1))
			statistics.append((zipfilename,nPaths,stp_invoked,time_spent))
			#print line
			#line = file.readline()
			#print line
			#line = file.readline()
			#print line
		
		#Skip lines till Finish
		#Handle: "All 1 paths had errors."
		#while not (line == 'Finished.\n' or (line.startswith("All") and line.endswith("paths had errors.\n"))):
		#	line = file.readline()

		break
		print "WRONG"
		
	#os.unlink(filepath)
	#END while



print 'Total number of paths:',totalNumPaths


if calculate:
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
		print '\nConfiguration',len(chosenConfigs),'is\n'
		for varValPair in sorted(chosenConfig):
			print '%s=%d' % varValPair
		print '\nand it covers these new lines:\n'
		#newLinesAsPairs = [str.split(':') for str in (chosenCoverage & remainingToCover)]
		#for fileLinePair in sorted([(x[0],int(x[1])) for x in newLinesAsPairs]):
		#	print '%s:%d' % fileLinePair
		remainingToCover -= chosenCoverage
		coveredSoFar |= chosenCoverage
		print '\nThe total coverage so far is',len(coveredSoFar)
	
	print '\nThat covers everything'
	
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

print '\nStatistics:'
print 'test name\t#paths\tstp\ttime'
for x in sorted(statistics):
	print '%s\t%d\t%d\t%0.2f' % x
	

print '\nHere are all',len(lines_covered),covtype+'s ever covered:'
#linesAsPairs = [str.split(':') for str in lines_covered]
#for fileLinePair in sorted([(x[0],int(x[1])) for x in linesAsPairs]):
#	print '%s:%d' % fileLinePair
for x in sorted(lines_covered):
	print x,
