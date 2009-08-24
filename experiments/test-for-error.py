import os,sys,zipfile,re

for filename in os.listdir(sys.argv[1]):
	if filename[-4:]!=".zip":
		continue
	filename = filename[:-4]
	print filename
	zf = zipfile.ZipFile(sys.argv[1]+"/"+filename+".zip")
	input = open(zf.extract(filename+"-test","/tmp"))

	output = "No problem"
	for line in input:
		if "Fatal error" in line:
			output = line[:-1]
			break
	print "\t%s" % output
