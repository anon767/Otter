import sys,os,re

dir = sys.argv[1]

for filename in os.listdir(dir):
	if not filename.endswith("-test"):
		continue
	file = open (filename)
	line = file.readline()
	while not line.startswith("Running Test"):
		line = file.readline()
	newfilename = re.match(r"Running Test vardeps_fewer_(\S+)",line).group(1)
	while line!="":
		while line.find("--numVars")<0:
			line = file.readline()
		numVar = int(re.search(r"--numVars (\S+) ",line).group(1))
		outputfile = open(newfilename+"."+str(numVar)+".deps","w")
		line = file.readline()
		while not line=="" and not line.startswith("======") and not line.startswith("command exit code"):
			outputfile.write(line)
			line = file.readline()
		outputfile.close()
		line = file.readline()


	
