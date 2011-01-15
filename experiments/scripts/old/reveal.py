import sys,zipfile,os,re

if len(sys.argv) == 1:
	print '''Usage: python reveal.py dir
dir: the directory that stores *.zip
'''
	sys.exit(0)

dir = sys.argv[1]
temp_dir = '/fs/skoll/symexe/data/.tmp'

# First, read in the coverage information from all of the files
for zipfilename in os.listdir(dir):
	if zipfilename[-4:]!=".zip":
		continue
	zipfilename = dir+'/'+zipfilename
	print 'Reading from',zipfilename,"\t",
	zipfileObj = zipfile.ZipFile(zipfilename)
	outputFilename = zipfilename[zipfilename.rindex('/')+1:-4] + '-test'
	filepath = temp_dir+"/"+outputFilename
	if os.path.isfile(filepath):
		file = open(filepath)
	else:
		file = open(zipfileObj.extract(outputFilename,temp_dir))

	line = file.readline()
	if not line.startswith("running"):
		print "Not from skoll cluster!"
		continue
	testname = re.search(r"testcase=(\S+)\.\.\.",line).group(1)
	print testname


