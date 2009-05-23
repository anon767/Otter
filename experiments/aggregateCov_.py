import sys,zipfile,os,re

#todo: look for: "User interrupt!"

if len(sys.argv) == 1:
	print '''Usage: python aggregateCov.py option [keyword] dir
dir: the directory that stores *.zip
option:
-0 : don't do greedy algorithm (only print lines covered and scattered graph only)
-1 : greedy algorithm, use method 1
-2 : greedy algorithm, use method 2 
<keyword> : only tests that contains <keyword> in the header will be processed (useful for distinguish t-way)'''
	sys.exit(0)

configs = []
coverage = []
lines_covered = set()
statistics = []

counter=0
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

if sys.argv[1] == '-2':
	merge = merge2
elif sys.argv[1] == '-1':
	merge = merge1
else:
	calculate = False

sys.argv[:2] = []

keyword = ""
interrupt = "User interrupt!"

if len(sys.argv) == 2:
	keyword = sys.argv[0]
	sys.argv[:1] = []

dir = sys.argv[0]
temp_dir = '/fs/skoll/symexe/data/.tmp'

# First, read in the coverage information from all of the files
for zipfilename in os.listdir(dir):
	if zipfilename[-4:]!=".zip":
		continue
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
	line = 'x'


	while True: 
		need_process = False
		nPaths = 0
		while line != '' and not line.startswith('STP was invoked'):
			line = file.readline()
			if line.startswith("Running Test"):      #  Edit this and the line below to suit your need
				print line,
			if line.startswith("Running Macro"):     #
				print line,
			if line.find(interrupt)>=0:
				print '(Warning: test interrupted)',
			if line.find(keyword)>=0:
				need_process = True
			if line.startswith('Sample value'):
				if not need_process:
					break
				counter += 1
				nPaths += 1
				thisConfig = set() # A configuration is a set of (variable,value) pairs
				# Read in the configuration
				line = file.readline()
				while line != '\n':
					variable,value = line.split('=')
					thisConfig.add((variable,int(value)))
					line = file.readline()
				coveredLines = set()
				# Read in the coverage
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
	
		if line=='':
			break
		#if need_process:
		#	print '[Done]'
		#else:
		#	print '[Omitted]'
		if need_process:
			# (nPaths) is the # of paths
			stp_invoked = int(re.match(r"STP was invoked (\S+) times",line).group(1))
			line = file.readline()
			time_spent = float(re.match(r"It ran for \S+ s, which is \S+ of the total (\S+) s execution.",line).group(1))
			statistics.append((nPaths,stp_invoked,time_spent))
			#print line
			#line = file.readline()
			#print line
			#line = file.readline()
			#print line
		
		#Skip lines till Finish
		#Handle: "All 1 paths had errors."
		while not (line == 'Finished.\n' or (line.startswith("All") and line.endswith("paths had errors.\n"))):
			line = file.readline()
		
	#os.unlink(filepath)
	#END while



print 'Total number of paths:',counter


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
		remainingToCover -= chosenCoverage
		coveredSoFar |= chosenCoverage
		print '\nConfiguration',len(chosenConfigs),'is\n'
		for variable,value in sorted(chosenConfig):
			print variable + '=' + str(value)
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
print '#paths\tstp\ttime'
for x in sorted(statistics):
	print '%d\t%d\t%0.2f' % x
	

print '\nHere are all',len(lines_covered),'lines ever covered:'
linesAsPairs = [str.split(':') for str in lines_covered]
for fileLinePair in sorted([(x[0],int(x[1])) for x in linesAsPairs]):
	print '%s:%d' % fileLinePair

