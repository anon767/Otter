import sys,os,cgi

if len(sys.argv)<3:
	print '''Usage: python uncovered.py diff_file path1 [path2 [...]]
'''
	sys.exit(0)
	

path = []
def open_under_path(filename,mode):
	filename = "/"+filename
	for p in path:
		if os.path.exists(p+filename):
			print (p+filename)
			return open(p+filename,mode)
	raise NameError,filename+' not found'

	

map = dict()
def txt2html(filename):
	file = open_under_path(filename,"r")
	linenum = 0
	output = [""]
	
	for line in file:
		linenum += 1
		output.append(("%5d"%linenum)+ "\t"+ cgi.escape(line))
	
	file.close()
	map[filename] = output
	
path = sys.argv[2:]
file = open(sys.argv[1],"r")

for p in path:
	print p

for line in file:
	(filename,str_linenum) = line.split(":")
	linenum = int(str_linenum)
	if filename not in map:
		txt2html(filename)
	map[filename][linenum] = "<span style=\"background-color: #ffff00\">"+map[filename][linenum][:-1]+"</span>\n"

for (filename,lines) in map.items():
	try:
		os.mkdir("html")
	except OSError:
		pass
	file = open("html/"+filename+".html","w")
	print >>file, ("<html>")
	print >>file, ("<body>")
	print >>file, ("<pre>")
	for line in lines:
		if line=="": continue
		print >> file, line,
	print >>file, ("</pre>")
	print >>file, ("</body>")
	print >>file, ("</html>")
	file.close()
