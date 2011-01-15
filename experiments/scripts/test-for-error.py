import os,sys,zipfile,re

for filename in os.listdir(sys.argv[1]):
	if filename[-4:]!=".zip":
		continue
	filename = filename[:-4]
	print filename,
	try:
		zf = zipfile.ZipFile(sys.argv[1]+"/"+filename+".zip")
		input = open(zf.extract(filename+"-test","/tmp"))

		output = ""
		for line in input:
			if "Fatal error" in line:
				output = line[:-1]
				break
			if line.startswith("0 lines left"):
				output += "No error "
			elif "BEGIN TEST_VARDEPS" in line:
				output += "\n"+re.match(r"#BEGIN TEST_VARDEPS TEST_NAME=(\S+)",line).group(1)+": "
		print "\t%s" % output
	except IOError:
		print "\t!!!!!!!!!!!!!!!!! IOError!"
